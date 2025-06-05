setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
start_time <- Sys.time()
# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(caret)
library(faux)
library(kernlab)
library(randomForest)
library(naivebayes)
library(nnet)
library(moments)
library(xtable)
library(doParallel)
library(betacal)
library(grid)
library(tidyr)
# Load custom metric functions
source("utils/auc.R")
source("utils/brier_score.R")
source("utils/mce_ece.R")

# Ensure output directory exists
if (!dir.exists("plots")) dir.create("plots")

# Global settings
n <- 5000
set.seed(123)
# Define model configurations
fitControl_none <- trainControl(method = "none", classProbs = TRUE, savePredictions = TRUE)
fitControl_svm  <- trainControl(method = "cv", number = 10, classProbs = TRUE, savePredictions = TRUE)
fitControl_cv   <- trainControl(method = "cv", number = 10, classProbs = TRUE, savePredictions = TRUE)

tuneGrid_svm <- expand.grid(cost = c(0.001, 0.01, 0.1, 1))
tuneGrid_rf  <- expand.grid(mtry = c(2, 4, 6))
tuneGrid_nb  <- expand.grid(usekernel = TRUE, laplace = c(0, 0.5, 1), adjust = c(0.75, 1, 1.25, 1.5))
tuneGrid_nn  <- expand.grid(size = 3:10, decay = seq(0.1, 0.5, 0.1))

models <- list(
  lr = list(method = "glm", control = fitControl_none),
  svm = list(method = "svmLinear2", control = fitControl_svm, tuneGrid = tuneGrid_svm),
  random_forest = list(method = "rf", control = fitControl_cv, tuneGrid = tuneGrid_rf),
  naivebayes = list(method = "naive_bayes", control = fitControl_cv, tuneGrid = tuneGrid_nb),
  neural_net = list(method = "nnet", control = fitControl_cv, tuneGrid = tuneGrid_nn, trace = FALSE)
)

# Define distribution settings
distributions <- list(
  u_shape  = list(alpha = 0.5, beta = 0.5),
  asym     = list(alpha = 0.8, beta = 2.4),
  uniform  = list(alpha = 1,   beta = 1),
  bell     = list(alpha = 2.4, beta = 2.4)
)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
# Helper function to generate plots
generate_plots <- function(dist_name, alpha, beta) {
  prob <- rbeta(n, alpha, beta)
  lin_pred <- qlogis(prob)
  y <- rbinom(n, 1, prob)
  
  x <- matrix(rnorm(n * 9), nrow = n)
  x[,1] <- rnorm_pre(lin_pred, r = 0.3)
  x[,2] <- rnorm_pre(lin_pred, r = -0.3)
  x[,3] <- rnorm_pre(lin_pred, r = -0.2)
  x[,4] <- rnorm_pre(lin_pred, r = 0.4)
  x[,5]=runif(n,min=-3,max=3)
  x[,6]=runif(n,min=-3,max=3)
  coef=c(2,-1,-1,0.5,3,-4,1,-0.5,-3)
  x=cbind(x,lin_pred-(x%*%coef))
  data=as.data.frame(cbind(x,y))
  # Split
  train_ind <- createDataPartition(data$y,p=0.5,list=F)
  notrain_ind <- setdiff(seq_len(n),train_ind)
  valid_test_ind <- createDataPartition(data$y[notrain_ind], p=0.5, list=F)
  valid_ind <- notrain_ind[valid_test_ind]
  test_ind <- setdiff(notrain_ind, valid_ind)
  train <- data[train_ind, ]
  validation <- data[valid_ind,]
  test <- data[test_ind, ]
  nb_test <-n-length(valid_ind)-length(train_ind)
  
  results <- list()
  true_alpha <- alpha
  true_beta <- beta
  for (model in names(models)) {
    cat("Training", model, "on", dist_name, "\n")
    m <- models[[model]]
    
    fit <- train(factor(y, labels = c("zero", "un")) ~ ., data = train,
                 method = m$method, trControl = m$control,
                 tuneGrid = m$tuneGrid, preProc = c("center", "scale"))
    pred_val <- qlogis(predict(fit, newdata=validation[,1:ncol(x)], type = "prob")[, 2])
    pred <- qlogis(predict(fit, newdata=test[,1:ncol(x)], type = "prob")[, 2])
    pred_val_beta <- predict(fit, newdata=validation[,1:ncol(x)], type = "prob")[, 2]
    pred_beta <- predict(fit, newdata=test[,1:ncol(x)], type = "prob")[, 2]
    #Entraîner un nouveau modèle sur l'ensemble de validation pour la recalibration - Platt scaling
    base_val <- data.frame(prob=prob[valid_ind], y=validation$y, pred_val=pred_val,pred_val_beta=pred_val_beta)
    fit_PS <- train(factor(y,labels=c("zero","un"))~pred_val, 
                    data = base_val, 
                    method = "glm", 
                    trControl = trainControl(method="none"), 
                    tuneGrid=NULL
    )
    fit_beta <- beta_calibration(p=base_val$pred_val_beta, y=base_val$y, "abm")
    
    pred <- data.frame(pred_val=pred)
    p_PS <- predict(fit_PS, pred, type = "prob")[, 2]
    p_beta <-  beta_predict(p=pred_beta, fit_beta)
    
    # Après avoir obtenu p_PS et p_beta:
    df <- data.frame(
      prob = prob[test_ind],
      pred_ = pred_beta,  
      p_PS = p_PS,
      p_beta = p_beta
    )
    
    # Calcul des EL
    EL_raw  <- mean((df$pred_ - df$prob)^2)
    EL_PS   <- mean((df$p_PS - df$prob)^2)
    EL_beta <- mean((df$p_beta - df$prob)^2)
    
    # Stocker les résultats
    results[[model]] <- data.frame(
      model = model,
      TDS = dist_name,
      EL_raw = EL_raw,
      EL_PS = EL_PS,
      EL_beta = EL_beta
    )
    
  
  }
  # Combiner tous les résultats
  df_EL <- do.call(rbind, results)
  df_EL$delta_PS   <- df_EL$EL_raw -df_EL$EL_PS 
  df_EL$delta_beta <- df_EL$EL_raw -df_EL$EL_beta 
  return(df_EL)
}
  

# Appeler la fonction pour chaque distribution
all_results <- list()
for (dist_name in names(distributions)) {
  params <- distributions[[dist_name]]
  all_results[[dist_name]] <- generate_plots(dist_name, params$alpha, params$beta)
}

# Combiner tous les résultats
df_EL_combined <- do.call(rbind, all_results)

# Préparation des données pour le graphique
df_long <- pivot_longer(df_EL_combined, 
                        cols = c("delta_PS", "delta_beta"),
                        names_to = "method", 
                        values_to = "delta_EL")
df_long$model <- factor(df_long$model, levels = c("lr", "svm", "neural_net", "random_forest", "naivebayes"),
                        labels = c("LR", "SVM", "NN", "RF", "NB"))
df_long$TDS <- factor(df_long$TDS, levels = c("u_shape","asym", "uniform","bell" ),
                      labels = c("U-shaped","Asymmetric-shaped", "Uniform-shaped","Bell-shaped" ))

El_diff <- ggplot(df_long, aes(x = model, y = delta_EL, fill = method)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(delta_EL, 4)), position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
  facet_grid(~ TDS) +
  labs(title = "",
       x = "Classifiers", y = expression("Change in Epistemic Loss ("*Delta*EL*")")) +
  scale_fill_manual(values = c("delta_PS" = "#4C72B0", "delta_beta" = "#DD8452"),
                    labels = c("Platt Scaling", "Beta Calibration")) +
  theme_minimal()


# Sauvegarde
ggsave("plots/EL_diff_grid.pdf", plot = El_diff, width = 20, height = 8)

end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "secs")
cat("Execution time:", 
    if (elapsed > 60) paste(round(as.numeric(elapsed)/60, 2), "minutes") else paste(round(elapsed, 2), "seconds"),
    "\n")
stopCluster(cl)
