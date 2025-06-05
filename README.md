# ICDM 2025 – Classifier Calibration and Recalibration

This repository contains the code for the paper:

**"Can we trust standard calibration metrics before and after recalibration?"**  
_Submitted to ICDM 2025_

---

##  Overview

This project investigates how well probabilistic classifiers approximate the true underlying probability distribution, both **before and after recalibration**. It highlights the **limitations of common calibration metrics**, particularly the widely-used ECE, and evaluates the ability of recalibration methods to recover calibration.

We simulate datasets where the true probabilities are known, allowing us to:
- Validate classical results in calibration.
- Analyze distortions in the **True Distribution Shape (TDS)** caused by classifiers.
- Compare calibration metrics and decompositions.
- Evaluate the effect of **Platt Scaling** and **Beta Calibration**.

---

## 📁 Repository Structure
├── calibration/ # All tables for initial evaluation 
├── recalibration/ # All tables for Platt Scaling
├── recalibration_beta/ # All tables for Beta Calibration
├── utils/ # Helper functions
├── plots/ # Generated figures
├── ICDM calibration metrics.R
├── ICDM recalibration PS metrics.R
├── ICDM recalibration beta metrics.R
├── ICDM graphics.R
├── ICDM recalibration graphics.R
├── ICDM error recalibration graphics.R
├── main_pipeline.R # Master script to reproduce the full pipeline
└── README.md # This file
