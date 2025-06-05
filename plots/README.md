# Visualizations – Calibration and Recalibration

This folder contains key visualizations produced throughout the simulation pipeline.  
They illustrate the shape and quality of predicted probabilities **before and after recalibration**, as discussed in the ICDM 2025 submission.

---

##  Contents

| File name                      | Description |
|-------------------------------|-------------|
| `all_models_histograms.pdf/png` | Histograms of predicted probabilities for all models, **before** recalibration |
| `summary_uniform.pdf`          | Density plots of predicted probabilities **after recalibration** (TDS = uniform) |
| `summary_bell.pdf`             | Same for bell-shaped TDS |
| `summary_u_shape.pdf`          | Same for U-shaped TDS |
| `summary_asym.pdf`             | Same for asymmetric TDS |
| `EL_diff_grid.pdf`             | Grid of Epistemic Loss differences (**before vs. after**) by model and TDS |
| `EL_diff_wrap.pdf`             | Wrapped version of EL_diff for cleaner presentation |
| `distri_shapes.pdf`            | Reference shapes of the **true probability distributions** used for simulation |
| `all_models_errors.pdf/png`    | Summary plots of metric errors across models and settings (may include legacy outputs) |

---

##  Notes

- Not all TDS summary plots are shown in the paper, but **all are generated** to support reproducibility.
- `EL_diff` provide a clear view of how recalibration affects the alignment with true probabilities.
- Histograms allow visual assessment of **calibration** across classifiers.

---

## Internal Notes

Some files in this folder (e.g. old `.png` versions or partial plots) are not referenced in the manuscript and may be excluded in the final archive.


