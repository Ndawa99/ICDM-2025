# main_pipeline.R

# ---- STEP 0: Set working directory (optional, if run in RStudio) ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ---- STEP 1: Evaluate metrics before recalibration ----
message("Step 1: Running initial calibration metrics...")
source("ICDM calibration metrics.R")

# ---- STEP 2: Generate graphics before recalibration ----
message("Step 2: Generating graphics before recalibration...")
source("ICDM graphics.R")

# ---- STEP 3: Apply Platt Scaling and evaluate metrics ----
message("Step 3: Running Platt Scaling recalibration...")
source("ICDM recalibration PS metrics.R")

# ---- STEP 4: Apply Beta Calibration and evaluate metrics ----
message("Step 4: Running Beta Calibration recalibration...")
source("ICDM recalibration beta metrics.R")

# ---- STEP 5: Generate graphics after recalibration ----
message("Step 5: Generating post-recalibration graphics...")
source("ICDM recalibration graphics.R")

# ---- STEP 6: Plot recalibration errors or differences ----
message("Step 6: Plotting recalibration error comparisons...")
source("ICDM error recalibration graphics.R")

message("\n✅ All steps completed successfully.")
