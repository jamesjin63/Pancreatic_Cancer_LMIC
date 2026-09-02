# Supplementary Tables 1-17

This directory holds the seventeen supplementary-table workbooks under the numbers used in the
controlling R5 manuscript, together with one standalone R entry point per table in `code/`. Every
wrapper reuses `scripts/make_submission_xlsx.R`, so a batch build and a single-table build execute
identical logic: the same source columns, labels, numeric-cell conversion, missing-value policy,
row ordering, rounding and OOXML cleanup.

## Rebuild

From the package root (`code/`):

```bash
Rscript scripts/run_LMIC_Pancreatic_VLW_R3.R            # the main pipeline
Rscript scripts/run_R5_reviewer_analyses.R              # the reviewer-requested analyses
Rscript "table/code/make_all_supplementary_tables.R"    # all seventeen workbooks
Rscript "table/code/Supplementary Table 12.R"           # or one of them
```

The first two scripts must run first: they write the CSVs the workbooks are built from.

## Numbering

These are the numbers used in the manuscript caption list, in the submitted attachment set and in
the response letter. Earlier rounds numbered the files by the order in which the pipeline happened
to produce them, which stopped matching the manuscript at the R5 round; Reviewer #3 hit that
mismatch when checking whether Supplementary Table 11 contained the ARIMA coefficients its caption
promised. For a reader holding the R4 version:

| R4 | R5 | R4 | R5 | R4 | R5 |
|---|---|---|---|---|---|
| S1-S3 | S1-S3 | S7 | S7 | S12 | S14 |
| S6 | S4 | S8-S10 | S9-S11 | S13 | S15 |
| S4 | S5 | — | **S12** (new) | S11 | S16 |
| S5 | S6 | — | **S13** (new) | — | **S17** (new) |
| | | — | **S8** (new) | | |

Two outputs from earlier rounds are deliberately absent, because the manuscript no longer cites
them: the single 2018-2023 holdout validation, superseded by the rolling-origin work in S9-S11,
and the two-model forecast/residual diagnostics, superseded by the four-model table that is now
S16. Their CSVs are still written to `outputs/R3_submission_tables/`.

The data provenance manifests and the MASE definition are deposited as CSVs in `outputs/R5/`
(`R5_query_manifest.csv`, `R5_data_provenance_manifest.csv`, `R5_MASE_definition.csv`) rather than
as attachments, because the manuscript states them in the Methods rather than citing a table.

## Contents and sources

| Table | Contents | Source CSV(s) |
|---|---|---|
| S1 | Country-level ranking of the 2023 welfare burden | `Supplementary Table 1_country burden.csv` |
| S2 | Income-gradient regression, HC3 | `Supplementary Table 8_income-gradient regression HC3.csv` |
| S3 | Country- and sex-specific 2023 burden | `Supplementary Table 2_country and sex burden.csv` |
| S4 | Annual ARIMA primary and ETS sensitivity projections | `Supplementary Table 3_annual ETS and ARIMA projections.csv` |
| S5 | Secondary sex-specific 2050 projections, reconciliation, and the sex disparity over time | `R5_sex_specific_2050.csv`; `R5_sex_reconciliation_2050.csv`; `R5_sex_gap_over_time.csv` |
| S6 | DALY-derived versus independently projected VLW | `Supplementary Table 6_projection reconciliation.csv` |
| S7 | Effective group VSLY by year, composition drift, and its 2050 impact | `R5_effective_group_VSLY_by_year.csv`; `R5_fixed_composition_sensitivity.csv`; `R5_fixed_composition_impact_2050.csv` |
| **S8** | Endpoint CAGR versus an all-year log-linear fit as the composition-drift basis | `R5_composition_drift_basis_comparison.csv`; `R5_composition_drift_impact_comparison.csv` |
| S9 | Rolling-origin validation, all series, origins and horizons pooled | `R5_rolling_origin_supplementary_table.csv`, panel C |
| S10 | Rolling-origin validation by forecast horizon | `R5_rolling_origin_supplementary_table.csv`, panel A |
| S11 | Rolling-origin validation by series | `R5_rolling_origin_by_series.csv` |
| **S12** | The prespecified selection rule applied to each primary DALY series, all four methods eligible | `R5_model_selection_primary_DALY.csv` |
| **S13** | Rolling-origin coverage of the complete aggregate range construction | `R5_aggregate_interval_coverage_overall.csv`; `R5_aggregate_interval_coverage_by_horizon.csv` |
| S14 | Eligible, included and excluded LMICs, by income group and by country | `R5_exclusion_fractions_by_income_group.csv`; `Supplementary Table 7_excluded-country DALY burden.csv` |
| S15 | Annual unweighted means of country-specific age-standardised DALY rates | `Supplementary Table 9_unweighted country ASR means.csv` |
| S16 | Fitted-model specifications with AICc, and the ARIMA coefficients | `R5_full_sample_residual_adequacy.csv`; `R5_arima_coefficients.csv` |
| **S17** | Joint-simulation specification, correlation matrix, eigenvalues, non-negativity | four CSVs in `outputs/R5/` |

## Presentation options in the specification

Three declarative options in `scripts/make_submission_xlsx.R` exist so that the workbook a reader
downloads here and the workbook in the submitted attachment set are the same table rather than two
renderings of one CSV. They are applied in a fixed sequence, so a specification cannot depend on
the order in which the options are written.

| Option | Effect |
|---|---|
| `where` | named list, source column to permitted values; rows outside the set are dropped |
| `order` | named list, source column to level order; rows are sorted by those levels, first key outermost |
| `round` | named vector, output header to digits; applied after renaming, numeric columns only |

Every value the specification can select or reorder must already exist in the source CSV. Nothing
is computed here; the workbook builder never touches a number.

## Relation to the submitted attachments

Of the 3,536 numeric cells in the submitted workbooks, 3,534 are reproduced exactly by these
scripts. The two that are not are a unit and a placeholder, not a disagreement:

- Supplementary Table 5, sheet B reports the male-plus-female reconciliation difference as a
  percentage (2.33%); the submitted workbook printed the same quantity as a fraction (0.0233).
- Supplementary Table 7, sheet C leaves the primary scenario's difference from itself empty; the
  submitted workbook printed a literal `0`.

Two presentational differences remain and are deliberate. The submitted workbooks stacked
multi-part tables onto a single worksheet behind `A -` / `B -` banner rows; these workbooks give
each part its own worksheet, which keeps one type per column and makes every numeric cell
computable. And where the submitted workbook carried a rounded value only, these workbooks carry
the rounded value the manuscript quotes; the full-precision value is in the source CSV named above.
