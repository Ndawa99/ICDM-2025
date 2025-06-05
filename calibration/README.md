# Before recalibration Results

This folder contains the outputs of the **Calibration** step, applied to each True Distribution Shape (TDS).  
All results were generated as part of the simulation study submitted to ICDM 2025.

---

##  Contents

For each TDS (`asymmetric`, `bell`, `u_shape`, `uniform`), we provide:

| File type                  | Description |
|---------------------------|-------------|
| `latex_table_*.tex`       | Formatted summary tables of all evaluation metrics (mean ± sd), ready for inclusion in the paper |
| `latex_table_*.RData`     | Corresponding R objects (`xtable`) for re-use or re-export |
| `pvalue_counts_*.csv`     | Proportion of runs (out of 30) where the Kolmogorov–Smirnov test does **not** reject similarity between predicted and true distributions |

---

## 🧪 Notes on Experimental Setup

- **Recalibration method**:No recalibration method.
- **Classifier**: Applied on all models (LR, RF, NB, SVM, NN) across 30 random seeds.
- **Data**: Simulated according to 4 different Beta distributions representing the TDS.
- **Evaluation metrics** include:
  - Log-Loss (decomposed)
  - Brier Score (decomposed)
  - ECE and ECE\_acc
  - AUC
  - KS test (distributional alignment)

---

## Usage

- Use `.tex` files to include summary tables directly in your LaTeX manuscript.
- Use `.RData` files to re-generate or customize tables via `xtable`.
- Use `.csv` files to compare statistical power across shapes and models.

---

## Related Scripts

These results were generated via:

- `ICDM calibration metrics.R`


