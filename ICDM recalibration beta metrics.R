setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
start_time <- Sys.time()
# Load required libraries
library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)
library(gridExtra)
library(faux)
library(kernlab)
library(naivebayes)
library(moments)
library(xtable)
library(doParallel)
library(nnet)
library(xgboost)
library(betacal)
# Load custom metric functions
source("utils/auc.R")
source("utils/brier_score.R")
source("utils/mce_ece.R")

# Parameters
n <- 100
g <- 10
seeds <- 123#:152

# Beta distribution settings
distributions <- list(
  list(name = "uniform",    alpha = 1,   beta = 1),
  list(name = "asymmetric", alpha = 0.8, beta = 2.4),
  list(name = "bell",       alpha = 2.4, beta = 2.4),
  list(name = "u_shape",    alpha = 0.5, beta = 0.5)
)

# TrainControl settings
fitControl_none <- trainControl(method = "none", classProbs = TRUE, savePredictions = TRUE, allowParallel = TRUE)
fitControl_svm  <- trainControl(method = "cv", number = 10, classProbs = TRUE, savePredictions = TRUE, allowParallel = TRUE)
fitControl_cv   <- trainControl(method = "cv", number = 10, classProbs = TRUE, savePredictions = TRUE, allowParallel = TRUE)

# Models and tuning grids
models <- list(
  lr = list(method = "glm", control = fitControl_none),
  # svm = list(method = "svmLinear2", control = fitControl_svm, tuneGrid = expand.grid(cost = c(0.001, 0.01, 0.1, 1))),
  # random_forest = list(method = "rf", control = fitControl_cv, tuneGrid = expand.grid(mtry = c(2, 4, 6))),
  # naivebayes = list(method = "naive_bayes", control = fitControl_cv, tuneGrid = expand.grid(usekernel = TRUE, laplace = c(0, 0.5, 1), adjust = c(0.75, 1, 1.25, 1.5))),
  # neural_net = list(method = "nnet", control = fitControl_cv, tuneGrid = expand.grid(size = seq(from = 3, to = 10, by = 1), decay = seq(from = 0.1, to = 0.5, by = 0.1)))
)

# Log-loss function
log_loss <- function(actual, predicted) {
  eps <- 1e-9
  predicted <- pmax(pmin(predicted, 1 - eps), eps)
  -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
}

# Parallel setup
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(dplyr)
  library(randomForest)
  library(ggplot2)
  library(gridExtra)
  library(caret)
  library(faux)
  library(kernlab)
  library(naivebayes)
  library(moments)
  library(xtable)
  library(xgboost)
  library(betacal)
  
})

# Global results
final_results <- list()

# Loop over distribution types
for (dist in distributions) {
  cat("\n=== Processing distribution:", dist$name, "===\n")
  all_metrics <- list()
  
  for (s in seeds) {
    set.seed(s)
    cat("\n--- Processing seed", s, "---\n")    
    # Generate data
    prob <- rbeta(n, dist$alpha, dist$beta)
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
    
    # Model loop
    for (model_name in names(models)) {
      cat("    Training:", model_name, "\n")
      m <- models[[model_name]]
      
      tryCatch({
        fit <- train(factor(y, labels = c("zero", "un")) ~ ., data = train,
                     method = m$method, trControl = m$control,
                     tuneGrid = m$tuneGrid, preProc = c("center", "scale"))
        pred_val <- predict(fit, newdata=validation[,1:ncol(x)], type = "prob")[, 2]
        pred <- predict(fit, newdata=test[,1:ncol(x)], type = "prob")[, 2]
        
        #Entraîner un nouveau modèle sur l'ensemble de validation pour la recalibration
        base_val <- data.frame(y=validation$y, pred_val=pred_val)
        fit_val <- beta_calibration(p=base_val$pred_val, y=base_val$y, "abm")

        pred <- data.frame(pred_val=pred)
        #y_hat <- predict(fit_val, pred, type="raw")
        p_test <-  beta_predict(p=pred, model=fit_val)
        
        # Create calibration groups
        mtx <- data.frame(y = test$y, prob = p_test, id=rownames(test))
        mtx <- mtx[order(mtx$prob), ]
        split_mtx <- split(mtx, rep(1:ceiling(nrow(test)/g), each = nrow(test)/g, length.out = nrow(test)))
        moy_y <- sapply(split_mtx, function(group) mean(group$y))
        moy_y_vector <- unlist(lapply(split_mtx, function(group) rep(mean(group$y), nrow(group))))
        mtx$moy_y <- moy_y_vector
        mtx <- mtx[order(as.numeric(mtx$id)), ]
        test$C <- mtx$moy_y
        
        # Metrics
        results[[model_name]] <- list(
          kolmogrov_test = ks.test(prob[test_ind], p_test)$p.value,
          true_mse = mean((p_test - prob[test_ind])^2),
          ll_p = log_loss(prob[test_ind], p_test),
          ll_pi = log_loss(p_test, p_test),
          true_ll_epistemic_loss = log_loss(prob[test_ind], p_test) - log_loss(prob[test_ind], prob[test_ind]),
          irreducible_loss_ll = log_loss(test$y, prob[test_ind]),
          calibration_loss_ll = log_loss(test$C, p_test) - log_loss(test$C, test$C),
          refinement_loss_ll = log_loss(test$y, test$C),
          ll_y = log_loss(test$y, p_test),
          cab_large = mean(test$y)/mean(p_test),
          auc = auc(p_test, test$y),
          brier_score = brier_score(test$y, p_test),
          bs_epistemic_loss = brier_score(prob[test_ind], p_test) - brier_score(prob[test_ind], prob[test_ind]),
          irreducible_loss_bs = brier_score(test$y, prob[test_ind]),
          calibration_loss_bs = brier_score(test$C, p_test) - brier_score(test$C, test$C),
          refinement_loss_bs = brier_score(test$y, test$C),
          ece_stat = ece_mce(test$y, p_test, g = 10, 'C')$ece#,
         # ece_acc =  ece_mce(test$y, p_test, g=10, 'C',yhat = y_hat)$ece_acc
        )
        cat(" success\n")
      }, error = function(e) {
        cat(" failed:", e$message, "\n")
      })
    }
    
    # Format results
    if (length(results) > 0) {
      indicateurs_df <- do.call(rbind, lapply(names(results), function(name) {
        res <- results[[name]]
        data.frame(
          seed = s,
          model = name,
          p_valueks = res$kolmogrov_test,
          True_MSE = res$true_mse,
          LL_p = res$ll_p,
          LL_pi=res$ll_pi,
          Epistemic_Loss_LL = res$true_ll_epistemic_loss,
          Irreducible_Loss_LL = res$irreducible_loss_ll,
          Calibration_Loss_LL = res$calibration_loss_ll,
          Refinement_Loss_LL = res$refinement_loss_ll,
          LL_y = res$ll_y,
          ECE = res$ece_stat,
         # ECE_acc=res$ece_acc,
          AUC = res$auc,
          Brier = res$brier_score,
          Epistemic_Loss_BS = res$bs_epistemic_loss,
          Irreducible_Loss_BS=res$irreducible_loss_bs,
          Calibration_Loss_BS = res$calibration_loss_bs,
          Refinement_Loss_BS = res$refinement_loss_bs
        )
      }))
      all_metrics[[as.character(s)]] <- indicateurs_df
    }
  }
  
  # Combine all seeds
  if (length(all_metrics) > 0) {
    final_df <- do.call(rbind, all_metrics)
    final_results[[dist$name]] <- final_df
    
    # Save CSV summary
    pval_summary <- final_df %>%
      group_by(model) %>%
      summarise(count_pval_gt_005 = sum(p_valueks > 0.05),
                total = n(),
                proportion = count_pval_gt_005 / total)
    
    write.csv(pval_summary, paste0("recalibration/pvalue_counts_", dist$name, ".csv"), row.names = FALSE)
    print(pval_summary)
  }
}

# Stop cluster
stopCluster(cl)

# Export LaTeX tables
for (prob_name in names(final_results)) {
  cat("\n=== Summary for", prob_name, "===\n")
  
  summary_metrics <- final_results[[prob_name]] %>%
    group_by(model) %>%
    summarise(across(where(is.numeric), list(mean = mean, sd = sd), .names = "{.col}_{.fn}"))
  
  # Garder uniquement les colonnes se terminant par _mean
  mean_cols <- names(summary_metrics)[grepl("_mean$", names(summary_metrics))]
  sd_cols <- gsub("_mean$", "_sd", mean_cols)
  metric_names <- gsub("_mean$", "", mean_cols)
  
  # Formater en "moyenne (écart-type)"
  formatted <- data.frame(model = summary_metrics$model)
  for (i in seq_along(metric_names)) {
    m_col <- mean_cols[i]
    s_col <- sd_cols[i]
    new_col <- metric_names[i]
    formatted[[new_col]] <- sprintf("%.3f (%.3f)", summary_metrics[[m_col]], summary_metrics[[s_col]])
  }
  
  # Générer le tableau LaTeX
  latex_table <- xtable(t(formatted))
  print(latex_table)
  
  # Sauvegarde
  save(list = "latex_table", file = paste0("recalibration/latex_table_", prob_name, ".RData"))
  tex_filename <- paste0("recalibration/latex_table_", prob_name, ".tex")
  print(latex_table, type = "latex", file = tex_filename, include.rownames = TRUE)
  
}
end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "secs")
cat("Execution time:", 
    if (elapsed > 60) paste(round(as.numeric(elapsed)/60, 2), "minutes") else paste(round(elapsed, 2), "seconds"),
    "\n")