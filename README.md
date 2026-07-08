# Pancreatic Cancer LMIC VLW Reproducibility Package

This folder contains the code, input tables, session information, ETS diagnostics, and model-output tables used to reproduce the revised LMIC pancreatic cancer value of lost welfare (VLW) analysis.

## Folder Contents

- `scripts/`: full R scripts used for the main VLW analysis, R2 forecast validation, diagnostics, and cross-sectional correction workflow.
- `data_processed/merged.csv`: processed GBD 2023 pancreatic cancer DALY extraction used by the main analysis.
- `data_raw/gbd_daly_yearly/`: year-specific GBD DALY input files, 1990-2023.
- `data_raw/external_metadata/`: HALE, GDP, LMIC classification, and world map inputs used by the scripts.
- `data_raw/backup_results_2026-06-12/`: two historical output tables required by retained audit/correction helper scripts.
- `outputs/results_VLW/`: generated result tables, sensitivity outputs, and figure files.
- `outputs/R2/`: R2-specific validation outputs for excluded countries, holdout forecast validation, and covariance-aware prediction intervals.
- `diagnostics/`: country coverage, reconciliation checks, ETS diagnostics, provenance/methods note, and `sessionInfo.txt`.

## Main Reproduction Commands

Run commands from this `code/` directory.

```bash
Rscript scripts/run_LMIC_Pancreatic_VLW_v2.R
Rscript scripts/R2_forecast_validation.R
```

The primary script regenerates Tables 1-8, Figures 1-6, income-elasticity sensitivity outputs, ETS diagnostics, reconciliation checks, and session information under `outputs/results_VLW/` and `diagnostics/`.

The R2 validation script regenerates:

- `outputs/R2/R2_excluded_LMICs.csv`
- `outputs/R2/R2_forecast_holdout_validation_IE1.csv`
- `outputs/R2/R2_covariance_aware_all_LMIC_PI_IE1.csv`

## Software Environment

The recorded environment is in `diagnostics/sessionInfo.txt`.

Key versions:

- R 4.2.2
- tidyverse 2.0.0
- ggplot2 3.5.1
- dplyr 1.1.4
- readr 2.1.5
- forecast 8.21.1
- sf 1.0-16
- patchwork 1.2.0
- scales 1.3.0

## ETS Model Outputs

Estimated ETS parameters and diagnostics are supplied in:

- `diagnostics/ETS_diagnostics_all_IE.csv`
- `outputs/results_VLW/IE05/Stats_ETS_diagnostics.csv`
- `outputs/results_VLW/IE1/Stats_ETS_diagnostics.csv`
- `outputs/results_VLW/IE15/Stats_ETS_diagnostics.csv`

These files include the ETS method, smoothing parameters, damping parameter, variance, AIC, AICc, BIC, RMSE, and Ljung-Box p value.

## Input Data Provenance

Detailed provenance is documented in `diagnostics/PROVENANCE_and_METHODS.md`. In brief:

- Disease burden: GBD 2023 pancreatic cancer DALYs.
- HALE: GBD 2023 health-adjusted life expectancy.
- GDP: World Bank WDI PPP GDP per capita and total GDP.
- Income classification: World Bank LMIC classification table.
- Valuation constants: US DOT 2023 VSL, US GDP per capita PPP 2023, 3 percent base discount rate, and income elasticity scenarios of 0.5, 1.0, and 1.5.

