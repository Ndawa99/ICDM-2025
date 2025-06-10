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
| `diff.pdf`             | Grid of metrics differences (**before vs. after**) by model and TDS: ECE - EL - LL and BS |



---

##  Notes

- Not all plots are shown in the paper, but **all are generated** to support reproducibility only `diff` is in the paper.
- `diff` provides a clear view of how recalibration affects the alignment with true probabilities.
- Histograms allow visual assessment of **calibration** across classifiers.

---

## Internal Notes

Some files in this folder are not referenced in the manuscript and may be excluded in the final archive.


