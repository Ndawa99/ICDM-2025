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
  bell     = list(alpha = 2.4, beta = 2.4),
  asym     = list(alpha = 0.8, beta = 2.4),
  uniform  = list(alpha = 1,   beta = 1)
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
    
    df <- data.frame(prob = prob[test_ind], p_PS = p_PS, p_beta=p_beta, pred=pred$pred_val)
    
    plot_pred <- ggplot(df, aes(x = pred_beta)) +
      geom_histogram(aes(y = after_stat(density)),bins = 100, color = "black", fill = "blue") +
      stat_function(fun = dbeta, args = list(shape1 = true_alpha, shape2 = true_beta),
                    color = "red", size = 1)+
      labs(
        title = paste("Before recalibration –", model),
        x = expression(hat(pi)[test]),
        y = paste(dist_name, "-shaped density")
      ) +
      coord_cartesian(ylim = c(0, 6)) +
      theme_minimal()
    plot_pred_true_val <- ggplot(base_val, aes(x = pred_val, y = prob)) +
      geom_point(color = "blue", alpha = 0.5) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      labs(
        title = paste("True vs Raw score in validation –", model),
        x = expression(hat(pi)[val]),
        y = expression(pi)
      ) +
      theme_minimal()

    plot_hist_PS <- ggplot(df, aes(x = p_PS)) +
      geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "blue", color = "black") +
      stat_function(fun = dbeta, args = list(shape1 = true_alpha, shape2 = true_beta),
                    color = "red", size = 1)+
      labs(
        title = bquote("Recalibrated PS Probabilities –" ~ .(model)),
        x = expression(hat(pi)),
        y = paste(dist_name, "-shaped density")
      ) +
      coord_cartesian(ylim = c(0, 6)) +
      theme_minimal()
    
    plot_hist_beta <- ggplot(df, aes(x = p_beta)) +
      geom_histogram(aes(y = after_stat(density)), bins = 100, fill = "blue", color = "black") +
      stat_function(fun = dbeta, args = list(shape1 = true_alpha, shape2 = true_beta),
                    color = "red", size = 1)+
      labs(
        title = bquote("Recalibrated Beta Probabilities –" ~ .(model)),
        x = expression(hat(pi)),
        y = paste(dist_name, "-shaped density")
      ) +
      coord_cartesian(ylim = c(0, 6)) +
      theme_minimal()

    
    results[[model]] <- list(PS = plot_hist_PS, beta=plot_hist_beta, pred=plot_pred, raw=plot_pred_true_val )
  }
  # Réorganiser les grobs par ligne logique : pred, raw, PS, beta
  models_order <- names(models)
  preds  <- lapply(models_order, function(m) results[[m]]$pred)
  raws   <- lapply(models_order, function(m) results[[m]]$raw)
  pss    <- lapply(models_order, function(m) results[[m]]$PS)
  betas  <- lapply(models_order, function(m) results[[m]]$beta)
  
  grobs_ordered <- c(preds, raws, pss, betas)
  
  # Afficher en 4 lignes (types de plot) × 5 colonnes (modèles)
  grid <- grid.arrange(
    grobs = grobs_ordered,
    nrow = 4,
    top = paste("")
  )
  
  # Sauvegarde PDF
  pdf(file = paste0("plots/summary_", dist_name, ".pdf"), width = 20, height = 20)
  grid.draw(grid)
  dev.off()
}

# Generate and collect all plots
all_plots <- list()
for (dist in names(distributions)) {
  infos <- distributions[[dist]]
  all_plots[[dist]] <- generate_plots(dist, infos$alpha, infos$beta)
}

end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "secs")
cat("Execution time:", 
    if (elapsed > 60) paste(round(as.numeric(elapsed)/60, 2), "minutes") else paste(round(elapsed, 2), "seconds"),
    "\n")
stopCluster(cl)
