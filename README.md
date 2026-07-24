# Pancreatic Cancer LMIC VLW R3 Reproducibility Package

This folder is the cleaned, canonical R3 analysis package for the revised LMIC pancreatic-cancer value of lost welfare (VLW) manuscript. Superseded R2 scripts and outputs are outside this folder in `../legacy_R2_not_for_submission/` and are not part of the submission workflow.

## Run the analysis

From the project root:

```bash
Rscript code/scripts/run_LMIC_Pancreatic_VLW_R3.R
```

Or from this `code/` directory:

```bash
Rscript scripts/run_LMIC_Pancreatic_VLW_R3.R
```

`run_LMIC_Pancreatic_VLW_R3.R` is the only public entry point. It resolves the working directory and sources `run_LMIC_Pancreatic_VLW_v2.R`, the complete implementation retained under its historical filename for continuity. No `data_processed/merged.csv` file is required.

## Package contents

- `scripts/`: the R3 entry point and its complete implementation; no legacy analysis scripts.
- `data_raw/gbd_daly_yearly/`: 34 annual GBD DALY exports covering 1990-2023.
- `data_raw/external_metadata/`: HALE, GDP, LMIC classification, and world-map inputs.
- `outputs/results_VLW/`: generated tables and analysis-numbered figure files for IE = 0.5, 1.0, and 1.5.
- `outputs/R3/`: R3 holdout validation, ETS/ARIMA sensitivity, primary joint projections, reconciliation, regression, excluded-country burden, residual diagnostics, and ASR data.
- `outputs/results_VLW/diagnostics/`: coverage, forecast diagnostics, reconciliation, provenance/methods, and `sessionInfo.txt`.
- `outputs/R3_submission_figures/`: 7 main and 12 supplementary PDFs, automatically copied and named according to the revised manuscript.
- `outputs/R3_submission_tables/`: 6 main and 9 supplementary CSV tables, automatically named according to the revised manuscript.
- `docs/`: background documentation that is not an analysis input.

## R3 statistical policies

- Historical marginal lower/upper bounds are never summed. Aggregate historical 95% uncertainty intervals are not reported because matched GBD draws are unavailable.
- DALY is the primary projection estimand. Projected DALYs are then monetised using fixed 2023 income-group effective VSLYs.
- Every reported forecast interval explicitly uses `level = 95`.
- All-LMIC prediction intervals use 50,000 joint paths and the empirical residual-correlation matrix; subgroup endpoints are never summed.
- ETS is the primary forecast model; automatically selected non-seasonal ARIMA is a model-sensitivity analysis.
- The rate panel is the unweighted mean of country-specific age-standardised DALY rates.
- One discounted-annuity VSLY definition is used throughout, and both-sex values are derived as male plus female.
- IE = 0.5, 1.0, and 1.5 are valuation scenarios, not statistical uncertainty bounds.

## Manuscript-output mapping

| Revised manuscript item | Generated source |
|---|---|
| Tables 1-2 | `outputs/results_VLW/IE1/Table1_income_summary.csv`, `Table2_sex_income.csv` |
| Table 3 | `outputs/results_VLW/IE1/Table5_temporal.csv` |
| Table 4 | `outputs/results_VLW/IE1/Table6_age_specific.csv` |
| Table 5 | `outputs/results_VLW/IE1/Table7_forecast_summary.csv` |
| Table 6 | `outputs/results_VLW/CrossIE_sensitivity_summary.csv` |
| Figures 1-7 | `outputs/R3_submission_figures/Figure 1...pdf` through `Figure 7...pdf` |
| Supplementary Figures 1-12 | `outputs/R3_submission_figures/Supplementary Figure 1...pdf` through `Supplementary Figure 12...pdf` |
| Main Tables 1-6 | `outputs/R3_submission_tables/Main Table 1...csv` through `Main Table 6...csv` |
| Supplementary Tables 1-9 | `outputs/R3_submission_tables/Supplementary Table 1...csv` through `Supplementary Table 9...csv` |

The full country tables, annual forecasts, statistical tests, and R3-only audit outputs remain in their clearly labelled output directories.

## Forecast and provenance files

Key forecast outputs are:

- `outputs/results_VLW/diagnostics/Forecast_diagnostics_all_IE.csv`
- `outputs/results_VLW/IE05/Stats_forecast_diagnostics.csv`
- `outputs/results_VLW/IE1/Stats_forecast_diagnostics.csv`
- `outputs/results_VLW/IE15/Stats_forecast_diagnostics.csv`
- `outputs/R3/R3_holdout_validation_8_series.csv`
- `outputs/R3/R3_ETS_ARIMA_2050_8_series.csv`
- `outputs/R3/R3_primary_ETS_ARIMA_all_LMIC_projection.csv`
- `outputs/R3/R3_forecast_residual_diagnostics.csv`

Detailed input provenance and method notes are in `outputs/results_VLW/diagnostics/PROVENANCE_and_METHODS.md`. The recorded software environment is in `outputs/results_VLW/diagnostics/sessionInfo.txt`.

Core inputs are GBD 2023 pancreatic-cancer DALYs and HALE, World Bank WDI PPP GDP, World Bank LMIC classification, US DOT 2023 VSL, US 2023 PPP GDP per capita, a 3% base discount rate, and prespecified income-elasticity scenarios of 0.5, 1.0, and 1.5.
