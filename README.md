# ICDM 2025 – Classifier Calibration and Recalibration

This repository contains the code for the paper:

**"Can we trust standard calibration metrics before and after recalibration?"**  
_Submitted to ICDM 2025_

---

## Structure

This study investigates how well probabilistic classifiers approximate the true underlying probability distribution, both **before and after recalibration**. It highlights the **limitations of common calibration metrics**, particularly the widely-used ECE, and evaluates the ability of recalibration methods to recover calibration. We simulate datasets where the true probabilities are known, allowing us to:
- Validate results in calibration.
- Analyze distortions in the **True Distribution Shape (TDS)** caused by classifiers.
- Compare calibration metrics and decompositions.
- Evaluate the effect of **Platt Scaling** and **Beta Calibration**.


Each folder in this repository corresponds to a step of the pipeline and includes its own `README.md` with usage instructions:

- `calibration/` – All metric tables for each TDS **before** recalibration
- `recalibration/` – Metric tables **after Platt Scaling**
- `recalibration_beta/` – Metric tables **after Beta Calibration**
- `utils/` – Custom metric functions (AUC, Brier Score, ECE variants)
- `plots/` – Graphics comparing calibration before/after recalibration

To reproduce the full pipeline, use the main script: [`main_script.R`](./main_script.R)

---

## Parallel Execution

All simulations and model evaluations are performed using **parallel computing** on 7 cores  
(by default, `detectCores() - 1`). You can modify the number of workers in each script if needed.

To ensure proper parallel behavior, each script registers a cluster via `doParallel::registerDoParallel()`.

---

## Run the Full Pipeline

To execute the full experimental setup, open R and run:

```r
## Dependencies
install.packages(c("dplyr", "caret", "randomForest", "ggplot2", "gridExtra", "faux", "kernlab", "naivebayes", "moments", "xtable", "doParallel", "nnet", "betacal"))

source("main_script.R")

```r



## Reproducibility
- Simulations use fixed seeds (123:152) for reproducibility across 30 runs.
- Data are generated synthetically using Beta-distributed true probabilities.
- All metric outputs are saved in CSV and table LaTeX formats.
