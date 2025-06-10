setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
 start_time <- Sys.time()
 library(tibble)
 library(dplyr)
 library(tidyr)
 select <- dplyr::select
 
 # 1. Automatically load and rename latex_table objects from .RData files
 load_table <- function(folder, suffix) {
   files <- list.files(folder, pattern = "\\.RData$", full.names = TRUE)
   for (f in files) {
     load(f)  # loads 'latex_table'
     base_name <- tools::file_path_sans_ext(basename(f))
     var_name <- paste0(base_name, "_", suffix)
     assign(var_name, latex_table, envir = .GlobalEnv)
     rm(latex_table)
   }
 }
 
 # Load the 3 sets: calibration, Platt scaling, and beta calibration
 load_table("calibration", "cal")
 load_table("recalibration", "PS")
 load_table("recalibration_beta", "BC")
 
 # 2. Function to clean latex_table: remove sd values in parentheses and extract relevant rows
 clean_latex_table <- function(tbl) {
   tbl_clean <- apply(tbl, 2, function(x) {
     cleaned <- gsub("\\s*\\(.*?\\)", "", x)
     return(cleaned)
   })
   colnames(tbl_clean) <- tbl_clean[1, ]
   return(as.data.frame(tbl_clean, stringsAsFactors = FALSE)[c(11,12,15,16), ])
 }
 
 # Apply cleaning and numeric conversion to all latex_table_* objects
 for (obj in ls(pattern = "^latex_table_.*")) {
   tbl <- get(obj)
   tbl_clean <- clean_latex_table(tbl)
   tbl_clean_ <- as.data.frame(lapply(tbl_clean, as.numeric))
   rownames(tbl_clean_) <- rownames(tbl_clean)
   assign(obj, tbl_clean_, envir = .GlobalEnv)
 }
 
 # 3. Compute differences between original and recalibrated metrics (Platt & Beta)
 build_delta_table <- function(form, method_label = form) {
   # Retrieve the cleaned tables
   cal <- get(paste0("latex_table_", form, "_cal"))
   ps  <- get(paste0("latex_table_", form, "_PS"))
   bc  <- get(paste0("latex_table_", form, "_BC"))
   
   # Compute deltas
   delta_PS <- cal - ps
   delta_BC <- cal - bc
   
   # Format data for both calibration methods
   df_BC <- data.frame(t(delta_BC), method = rep(method_label, 5))
   colnames(df_BC)[1:4] <- c("delta_LL_BC", "delta_ECE_BC", "delta_Brier_BC", "delta_EL_BC")
   
   df_PS <- data.frame(t(delta_PS))
   colnames(df_PS)[1:4] <- c("delta_LL_PS", "delta_ECE_PS", "delta_Brier_PS", "delta_EL_PS")
   
   df_final <- cbind(df_PS, df_BC)
   df_final <- rownames_to_column(df_final, var = "model")
   
   # Store the final object in global environment
   assign(method_label, df_final, envir = .GlobalEnv)
 }
 
 # Apply the function to all distribution shapes
 build_delta_table("uniform", "uniform")
 build_delta_table("asymmetric", "asym")
 build_delta_table("u_shape", "u_shaped")
 build_delta_table("bell", "bell")
 
 # 4. Combine all formatted tables into a single long-format data frame
 df_long <- rbind(uniform, asym, u_shaped, bell)
 
 df_long_all <- df_long %>%
   select(model, method, 
          delta_Brier_PS, delta_Brier_BC,
          delta_LL_PS, delta_LL_BC,
          delta_ECE_PS, delta_ECE_BC, delta_EL_PS, delta_EL_BC) %>%
   pivot_longer(
     cols = -c(model, method),
     names_to = c("metric", "calibration_method"),
     names_pattern = "delta_(.*)_(.*)",
     values_to = "delta_value"
   ) %>%
   mutate(
     calibration_method = recode(calibration_method,
                                 PS = "Platt Scaling",
                                 BC = "Beta Calibration"),
     metric = recode(metric,
                     Brier = "Brier Score",
                     LL = "Log-Loss",
                     ECE = "ECE",
                     EL = "EL"),
     method = factor(method, levels = c("u_shaped", "asym", "uniform", "bell")),
     model = factor(model, levels = c("lr", "svm", "neural_net", "naivebayes", "random_forest")),
     metric = factor(metric, levels = c("EL", "ECE", "Log-Loss", "Brier Score"))
   )
 
 # 5. Plot the delta values for each metric and model
 library(ggplot2)
 
 diff <- ggplot(df_long_all, aes(x = model, y = delta_value, fill = calibration_method)) +
   geom_bar(stat = "identity", position = "dodge") +
   geom_text(aes(label = round(delta_value, 4)), 
             position = position_dodge(width = 0.9), 
             vjust = -0.3, size = 3) +
   facet_grid(metric ~ method) +
   labs(title = "",
        x = "Classifiers", 
        y = expression("Change in metric ("*Delta*")"),
        fill = "Recalibration method") +
   scale_fill_manual(values = c("Platt Scaling" = "#4C72B0", 
                                "Beta Calibration" = "#DD8452")) +
   theme_minimal() +
   theme(
     panel.border = element_rect(colour = "grey40", fill = NA, linewidth = 0.5),
     strip.background = element_rect(fill = "grey90", colour = "grey40"),
     strip.text = element_text(face = "bold")
   )
 
 # 6. Save the plot to file
 ggsave("plots/diff.pdf", plot = diff, width = 20, height = 10)
 
end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "secs")
cat("Execution time:", if (elapsed > 60) paste(round(as.numeric(elapsed)/60, 2), "minutes") else paste(round(elapsed, 2), "seconds"),    "\n")