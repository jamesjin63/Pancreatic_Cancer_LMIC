# Supplementary Tables 1-19

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

## Added for Reviewer #3's Major and Minor points

| Table | Contents | Source |
|---|---|---|
| S12 (sheet B) | ARIMA coefficient estimates and standard errors | `outputs/R5/R5_arima_coefficients.csv` |
| S15 | The prespecified selection rule applied to the primary DALY series, with all four methods eligible | `outputs/R5/R5_model_selection_primary_DALY.csv` |
| S16 | Rolling-origin coverage of the complete aggregate interval construction, pooled and by horizon | `outputs/R5/R5_aggregate_interval_coverage_*.csv` |
| S17 | Joint-simulation specification, residual correlation matrix, its eigenvalues, and measured non-negativity | four CSVs in `outputs/R5/` |
| S18 | Composition-drift basis: endpoint CAGR versus a log-linear fit on all observations | `outputs/R5/R5_composition_drift_*.csv` |
| S19 | Input manifest with MD5 checksums, and the MASE definition | `outputs/R5/R5_data_provenance_manifest.csv`, `R5_MASE_definition.csv` |

**Numbering note.** These files are named under the *code* numbering, which since the R5 round has
differed from the numbering used in the manuscript (manuscript S4 is code S13, manuscript S8-S10
are the three panels of code S10 and code S11, and so on). Reconciling the two is a separate task;
until it is done, check the caption against the contents rather than trusting the number.
