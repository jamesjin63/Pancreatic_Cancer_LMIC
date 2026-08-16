# Supplementary Tables 1-14

This directory contains the 14 supplementary-table workbooks under their manuscript names and one
standalone R entry point per table in `code/`. All wrappers reuse
`scripts/make_submission_xlsx.R`; therefore batch and individual builds use exactly the same source
columns, labels, numeric-cell conversion, missing-value policy, and OOXML cleanup.

## Rebuild

From the package root (`code/`):

```bash
Rscript scripts/run_LMIC_Pancreatic_VLW_R3.R
Rscript scripts/run_R5_reviewer_analyses.R
Rscript "table/code/make_all_supplementary_tables.R"
Rscript "table/code/Supplementary Table 13.R"  # one table only
```

Tables 1-9 use sources in `outputs/R3_submission_tables/`. Tables 10-14 use reviewer-requested
analyses in `outputs/R5/`. Supplementary Table 13 has two worksheets: sex-specific 2050 estimates
and reconciliation against the primary all-LMIC total. Supplementary Table 14 has three worksheets:
annual effective group VSLY, composition-drift sensitivity, and its estimated impact in 2050.

## Source mapping

| Table | Source CSV(s) |
|---|---|
| 1 | `Supplementary Table 1_country burden.csv` |
| 2 | `Supplementary Table 8_income-gradient regression HC3.csv` |
| 3 | `Supplementary Table 2_country and sex burden.csv` |
| 4 | `Supplementary Table 3_annual ETS and ARIMA projections.csv` |
| 5 | `Supplementary Table 4_holdout validation.csv` |
| 6 | `Supplementary Table 5_forecast and residual diagnostics.csv` |
| 7 | `Supplementary Table 6_projection reconciliation.csv` |
| 8 | `Supplementary Table 7_excluded-country DALY burden.csv` |
| 9 | `Supplementary Table 9_unweighted country ASR means.csv` |
| 10 | `R5_rolling_origin_supplementary_table.csv` |
| 11 | `R5_rolling_origin_by_series.csv` |
| 12 | `R5_full_sample_residual_adequacy.csv` |
| 13 | `R5_sex_specific_2050.csv`; `R5_sex_reconciliation_2050.csv` |
| 14 | `R5_effective_group_VSLY_by_year.csv`; `R5_fixed_composition_sensitivity.csv`; `R5_fixed_composition_impact_2050.csv` |
