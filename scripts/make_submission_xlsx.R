#!/usr/bin/env Rscript
# ==============================================================================
# make_submission_xlsx.R - build the submission xlsx tables from the verified CSVs
#
# Background
# ----
# The xlsx files in the R4 submission package were produced by importing the CSVs into Excel by
# hand; there was no code path that generated them. Under a locale in which "." is the thousands
# separator, that manual step read values of the form 0.xxx (exactly three decimals) as integers,
# inflating 628 cells by a factor of 1000:
#     Supplementary Table 3.xlsx  546   Supplementary Table 1.xlsx  81   Table 4.xlsx  1
# Example: male VLW for Zimbabwe is 0.127 (billion USD); the R4 table recorded 127.
#
# This script writes the xlsx files directly from outputs/R3_submission_tables/*.csv. Numbers are
# written to the sheet as numeric cells, so no locale-dependent text-to-number parsing occurs and
#
# the error cannot recur. The R4 presentation conventions are preserved:
#   - supplementary tables keep the R4 renumbering (R3 #8->R4 #2, #2->#3, #3->#4, #4->#5,
#     #5->#6, #6->#7, #7->#8)
#   - the tidied R4 header text and the columns R4 dropped (see the keep lists below)
#
# Usage
# ----
#   Rscript scripts/make_submission_xlsx.R            # writes outputs/submission_tables_xlsx/
#   Rscript scripts/make_submission_xlsx.R <output-dir>
# ==============================================================================

# Some launch environments export C.UTF-8, which is not a valid macOS locale. R then falls back to
# plain C and cannot address this package when a parent directory contains non-ASCII characters.
# Set a known macOS UTF-8 locale before resolving paths; numeric parsing below remains explicit.
try(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"), silent = TRUE)

suppressPackageStartupMessages({
  library(readr); library(writexl); library(zip)
})
# Note: writexl is used rather than openxlsx. openxlsx writes relationships in sheet1.xml.rels
# pointing at drawings/drawing1.xml and drawings/vmlDrawing1.vml without placing those files in
# the archive, leaving dangling references that strict parsers such as openpyxl reject. The
# structure writexl produces matches the original R4 Excel files and has no dangling references.

# -- Directory resolution (commandArgs encodes spaces in the path as "~+~"; reverse that) ------
.self <- function() {
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a) != 1L) return(normalizePath("."))
  dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", a), fixed = TRUE)))
}
CODE_DIR <- if (exists("CODE_DIR_OVERRIDE", inherits = FALSE)) {
  normalizePath(CODE_DIR_OVERRIDE)
} else {
  normalizePath(file.path(.self(), ".."))
}
source(file.path(CODE_DIR, "scripts", "_env.R"))   # rel_path(); see that file
CSV_DIR  <- file.path(CODE_DIR, "outputs", "R3_submission_tables")
args     <- commandArgs(trailingOnly = TRUE)
OUT_DIR  <- if (exists("OUT_DIR_OVERRIDE", inherits = FALSE)) {
  OUT_DIR_OVERRIDE
} else if (length(args) >= 1L) {
  args[1]
} else {
  file.path(CODE_DIR, "outputs", "submission_tables_xlsx")
}
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

if (!dir.exists(CSV_DIR))
  stop("CSV source directory not found; run scripts/run_LMIC_Pancreatic_VLW_R3.R first: ", CSV_DIR)

# -- Spec: out = output file, csv = source, keep = source columns in R4 order, header = R4 header
spec <- list(
  list(out = "Table 1.xlsx", csv = "Main Table 1_2023 income summary.csv",
       keep = c("Income Group", "Countries", "DALYs (thousands; point estimate)",
                "VLW (billion constant-2023 USD; point estimate)",
                "VLW undiscounted r=0 (billion constant-2023 USD)",
                "VLW/GDP (%; point estimate)", "Mean HALE"),
       header = c("Income Group", "Countries", "DALYs (thousands)",
                  "VLW (billion constant-2023 USD)",
                  "VLW undiscounted (billion constant-2023 USD)",
                  "VLW/GDP (%)", "Mean HALE")),

  list(out = "Table 2.xlsx", csv = "Main Table 2_sex-specific burden.csv",
       keep = c("Sex", "Income Group", "Countries", "DALYs (thousands; point estimate)",
                "VLW (billion constant-2023 USD; point estimate)",
                "VLW undiscounted (billion constant-2023 USD)"),
       header = c("Sex", "Income Group", "Countries", "DALYs (thousands)",
                  "VLW (billion constant-2023 USD)",
                  "VLW undiscounted (billion constant-2023 USD)")),

  list(out = "Table 3.xlsx", csv = "Main Table 3_temporal trends.csv",
       keep = c("Income Group", "Year", "DALYs (thousands; point estimate)",
                "VLW (billion constant-2023 USD; point estimate)"),
       header = c("Income Group", "Year", "DALYs (thousands)",
                  "VLW (billion constant-2023 USD)")),

  list(out = "Table 4.xlsx", csv = "Main Table 4_age-specific burden.csv",
       keep = c("Age Group", "DALYs (point estimate)",
                "VLW (billion constant-2023 USD; point estimate)", "Share of total VLW (%)"),
       header = c("Age Group", "DALYs", "VLW (billion constant-2023 USD)",
                  "Share of total VLW (%)")),

  list(out = "Table 5.xlsx", csv = "Main Table 5_selected projections.csv",
       keep = c("Model", "Year", "Group", "VLW (billion constant-2023 USD; scenario range)",
                "DALYs (thousands; scenario range)"),
       header = c("Model", "Year", "Group", "VLW (billion constant-2023 USD; scenario range)",
                  "DALYs (thousands; scenario range)")),

  list(out = "Table 6.xlsx", csv = "Main Table 6_income-elasticity sensitivity.csv",
       keep = c("Income Elasticity", "VLW base 3% (billion constant-2023 USD; point estimate)",
                "VLW undiscounted (billion constant-2023 USD)", "VLW/GDP (%; point estimate)"),
       header = c("Income Elasticity", "VLW base 3% (billion constant-2023 USD)",
                  "VLW undiscounted (billion constant-2023 USD)", "VLW/GDP (%)")),

  # ============================================================================================
  # Supplementary tables S1-S19.
  #
  # NUMBERING. S1-S13 are the numbers used in the R5 manuscript, in the order its caption list
  # gives them. Earlier rounds numbered these files by the order in which the pipeline happened
  # to produce them, which since the R5 round no longer matched the manuscript: manuscript S4 was
  # file S13, manuscript S8-S10 were three panels of file S10 plus file S11, and manuscript S11
  # was file S12. Reviewer #3 hit exactly that mismatch when checking whether Supplementary
  # Table 11 contained the ARIMA coefficients its caption promises. The files are now named for
  # the manuscript, so the caption, the file name, the per-table script and the response letter
  # all agree, and the manuscript needs no renumbering.
  #
  # S14-S19 are new at this round and are appended after the existing numbering, so no existing
  # citation in the manuscript has to move. Only six new captions have to be added.
  #
  # Two files from earlier rounds are deliberately NOT part of the submission set:
  #   - the single 2018-2023 holdout validation, superseded by the rolling-origin validation
  #     that now occupies S8-S10 and no longer cited in the manuscript;
  #   - the two-model forecast/residual diagnostics from the main pipeline, superseded by the
  #     four-model table that is now S11.
  # Their CSVs are still written to outputs/R3_submission_tables/ for continuity.
  # ============================================================================================

  # S1 - manuscript: "Country-level ranking of pancreatic cancer welfare burden in LMICs in 2023."
  list(out = "Supplementary Table 1.xlsx", csv = "Supplementary Table 1_country burden.csv",
       keep = c("Rank", "Country", "Income Group", "GDP pc (PPP)",
                "DALYs (thousands; point estimate)",
                "VLW (billion constant-2023 USD; point estimate)",
                "VLW/GDP (%; point estimate)"),
       header = c("Rank", "Country", "Income Group",
                  "GDP per capita (PPP, 2023 international $)", "DALYs (thousands)",
                  "VLW (billion constant-2023 USD)", "VLW/GDP (%)")),

  # S2 - manuscript: "Unweighted country-level regression analysis ... HC3 standard errors."
  list(out = "Supplementary Table 2.xlsx",
       csv = "Supplementary Table 8_income-gradient regression HC3.csv",
       keep = c("term", "estimate", "HC3_standard_error", "lower_95_CI", "upper_95_CI",
                "t_statistic", "df", "p_value_two_sided", "n", "R_squared",
                "adjusted_R_squared"),
       header = c("Term", "Estimate", "HC3 standard error", "Lower 95% CI", "Upper 95% CI",
                  "t statistic", "df", "Two-sided p value", "n", "R2", "Adjusted R2")),

  # S3 - manuscript: "Country- and sex-specific welfare burden of pancreatic cancer in 2023."
  list(out = "Supplementary Table 3.xlsx",
       csv = "Supplementary Table 2_country and sex burden.csv",
       keep = c("Country", "Sex", "Income Group", "DALYs (point estimate)",
                "VLW (billion constant-2023 USD; point estimate)",
                "VLW undiscounted (billion constant-2023 USD)",
                "VLW/GDP (%; point estimate)"),
       header = c("Country", "Sex", "Income Group", "DALYs",
                  "VLW (billion constant-2023 USD)",
                  "VLW undiscounted (billion constant-2023 USD)", "VLW/GDP (%)")),

  # S4 - manuscript: "Secondary sex-specific VLW projections and reconciliation with the primary
  #      DALY-derived projection through 2050."  (was file S13)
  list(out = "Supplementary Table 4.xlsx", sheets = list(
    list(sheet = "A - Sex-specific 2050", dir = "R5", csv = "R5_sex_specific_2050.csv",
         keep = c("Sex", "Year", "Estimand", "VLW_billion", "VLW_scenario_low",
                  "VLW_scenario_high", "Ljung_Box_p", "residual_autocorrelation_detected"),
         header = c("Sex", "Year", "Estimand", "VLW (billion constant-2023 USD)",
                    "VLW scenario range, low (billion constant-2023 USD)",
                    "VLW scenario range, high (billion constant-2023 USD)", "Ljung-Box p value",
                    "Residual autocorrelation detected at the 5% level")),
    list(sheet = "B - Reconciliation", dir = "R5", csv = "R5_sex_reconciliation_2050.csv",
         keep = c("Quantity", "Value_billion"),
         header = c("Quantity", "Value (billion constant-2023 USD, except relative difference)"))
  )),

  # S5 - manuscript: "Reconciliation between DALY-derived and independently projected
  #      welfare-loss estimates, 2024-2050."  (was file S7)
  list(out = "Supplementary Table 5.xlsx",
       csv = "Supplementary Table 6_projection reconciliation.csv",
       keep = c("model", "Year", "Group", "derived_VLW_billion", "direct_VLW_billion",
                "difference_billion", "percent_difference"),
       header = c("Model", "Year", "Group",
                  "VLW derived from DALY projections (billion constant-2023 USD)",
                  "Independently projected VLW (billion constant-2023 USD)",
                  "Absolute difference (billion constant-2023 USD)",
                  "Relative difference (%)")),

  # S6 - manuscript: "Annual primary ARIMA-based conditional projections and ETS sensitivity
  #      projections ... 2024-2050."  (was file S4)
  list(out = "Supplementary Table 6.xlsx",
       csv = "Supplementary Table 3_annual ETS and ARIMA projections.csv",
       keep = c("model", "Year", "Group", "DALY", "DALY_scenario_low", "DALY_scenario_high",
                "VLW_billion", "VLW_scenario_low_billion", "VLW_scenario_high_billion"),
       header = c("Model", "Year", "Group", "DALYs", "DALYs scenario range, low",
                  "DALYs scenario range, high", "VLW (billion constant-2023 USD)",
                  "VLW scenario range, low (billion constant-2023 USD)",
                  "VLW scenario range, high (billion constant-2023 USD)")),

  # S7 - manuscript: "Historical variation in effective group VSLY multipliers and sensitivity of
  #      projected 2050 welfare losses to composition drift."  (was file S14)
  list(out = "Supplementary Table 7.xlsx", sheets = list(
    list(sheet = "A - Effective VSLY", dir = "R5", csv = "R5_effective_group_VSLY_by_year.csv",
         keep = c("LMIC_group", "year", "Vbar"),
         header = c("Income group", "Year",
                    "Effective group VSLY (constant-2023 USD per DALY)")),
    list(sheet = "B - Composition drift", dir = "R5",
         csv = "R5_fixed_composition_sensitivity.csv",
         keep = c("LMIC_group", "Vbar_1990", "Vbar_2023", "change_1990_2023_pct",
                  "annualised_drift_pct", "min", "max", "implied_27yr_drift_pct"),
         header = c("Income group", "Effective VSLY in 1990", "Effective VSLY in 2023",
                    "Change, 1990-2023 (%)",
                    "Annualised composition drift, endpoint CAGR (%)",
                    "Minimum effective VSLY", "Maximum effective VSLY",
                    "Implied composition drift over 27 years (%)")),
    list(sheet = "C - Impact in 2050", dir = "R5",
         csv = "R5_fixed_composition_impact_2050.csv",
         keep = c("LMIC_group", "DALY", "VLW_billion", "factor_2050",
                  "VLW_drift_adjusted", "Scenario", "VLW_2050_billion"),
         header = c("Income group", "Projected DALYs in 2050",
                    "Primary VLW (billion constant-2023 USD)",
                    "Composition-drift factor in 2050",
                    "Drift-adjusted VLW (billion constant-2023 USD)", "Scenario",
                    "All-LMIC VLW in 2050 (billion constant-2023 USD, except difference)"))
  )),

  # S8 - manuscript: "Overall expanding-window rolling-origin validation performance."
  #      All four evaluated methods are candidates (Reviewer #3, Major 1).
  list(out = "Supplementary Table 8.xlsx", dir = "R5",
       csv = "R5_rolling_origin_model_ranking.csv",
       keep = c("model", "Role", "n_obs", "MAPE", "MASE", "MAE_rel_naive", "coverage_95",
                "n_series_lowest_MAPE_of_4"),
       header = c("Model", "Model role", "Forecast-actual pairs", "MAPE (%)", "MASE",
                  "Mean ratio of absolute error to the naive benchmark",
                  "Empirical coverage of the nominal 95% range (%)",
                  "Series with the lowest MAPE among the four candidates")),

  # S9 - manuscript: "Forecast-horizon-specific expanding-window rolling-origin validation."
  list(out = "Supplementary Table 9.xlsx", dir = "R5",
       csv = "R5_rolling_origin_by_horizon.csv",
       keep = c("model", "h", "n_origins", "n_obs", "MAE", "RMSE", "MAPE", "MASE",
                "coverage_95"),
       header = c("Model", "Forecast horizon (years)", "Rolling origins",
                  "Forecast-actual pairs", "MAE", "RMSE", "MAPE (%)", "MASE",
                  "Empirical coverage of the nominal 95% range (%)")),

  # S10 - manuscript: "Series-specific expanding-window rolling-origin validation."
  list(out = "Supplementary Table 10.xlsx", dir = "R5",
       csv = "R5_rolling_origin_by_series.csv",
       keep = c("series", "outcome", "model", "n_origins", "n_obs", "MAE", "RMSE",
                "MAPE", "MASE", "coverage_95"),
       header = c("Series", "Outcome", "Model", "Rolling origins", "Forecast-actual pairs",
                  "MAE", "RMSE", "MAPE (%)", "MASE",
                  "Empirical coverage of the nominal 95% range (%)")),

  # S11 - manuscript: "Fitted-model specifications, parameter estimates, information criteria,
  #      in-sample RMSE, and Ljung-Box residual diagnostics."
  #      Sheet B supplies the ARIMA coefficients the caption promises and sheet A now carries a
  #      populated AICc for ARIMA; both were missing at the R5 round (Reviewer #3, Minor 2).
  #      The diagnostic column records what the test observed, never "adequacy" (Minor 3).
  list(out = "Supplementary Table 11.xlsx", sheets = list(
    list(sheet = "A - Fitted models", dir = "R5",
         csv = "R5_full_sample_residual_adequacy.csv",
         keep = c("series", "outcome", "model", "method", "alpha", "beta", "phi", "sigma2",
                  "AIC", "AICc", "BIC", "RMSE", "Ljung_Box_lag", "Ljung_Box_fitdf",
                  "Ljung_Box_p", "residual_autocorrelation_detected"),
         header = c("Series", "Outcome", "Model", "Method", "alpha", "beta", "phi",
                    "Error variance (sigma2)", "AIC", "AICc", "BIC", "RMSE",
                    "Ljung-Box lag", "Ljung-Box degrees of freedom", "Ljung-Box p value",
                    "Residual autocorrelation detected at the 5% level")),
    list(sheet = "B - ARIMA coefficients", dir = "R5", csv = "R5_arima_coefficients.csv",
         keep = c("series", "outcome", "model", "term", "estimate", "std_error", "z"),
         header = c("Series", "Outcome", "Model", "Term", "Estimate", "Standard error",
                    "Estimate / standard error"))
  )),

  # S12 - manuscript: "Eligible, included, and excluded LMICs and excluded DALY burden."
  list(out = "Supplementary Table 12.xlsx",
       csv = "Supplementary Table 7_excluded-country DALY burden.csv",
       keep = c("location_name", "LMIC_group", "DALY", "income_group_total_DALYs",
                "percent_of_income_group_DALYs", "all_128_LMIC_DALYs",
                "percent_of_all_128_LMIC_DALYs"),
       header = c("Country", "Income group", "DALYs", "Total DALYs in income group",
                  "Percentage of income-group DALYs (%)", "Total DALYs among all 128 LMICs",
                  "Percentage of total DALYs among all 128 LMICs (%)")),

  # S13 - manuscript: "Annual unweighted arithmetic means of country-specific age-standardized
  #      pancreatic cancer DALY rates by World Bank income group, 1990-2023."
  list(out = "Supplementary Table 13.xlsx",
       csv = "Supplementary Table 9_unweighted country ASR means.csv",
       keep = c("LMIC_group", "year", "R"),
       header = c("Income group", "Year",
                  "Mean country-specific age-standardized DALY rate (per 100,000 population)")),

  # ============================================================================================
  # S14-S19: new at this round. Appended, so nothing already cited has to be renumbered.
  # ============================================================================================

  # S14 - the prespecified rule applied with every evaluated method eligible, on the primary
  #       DALY series. This is what lets a reader verify that nothing was excluded after the
  #       fact and that drift and naive fail on the residual criterion (Major 1a, 1b).
  list(out = "Supplementary Table 14.xlsx", dir = "R5",
       csv = "R5_model_selection_primary_DALY.csv",
       keep = c("Series", "Outcome", "Model", "Role", "Ljung_Box_p",
                "Residual_autocorrelation_detected", "Eligible_under_rule",
                "MAPE", "MASE", "coverage_95", "Rank_by_MAPE", "Selected"),
       header = c("Primary DALY series", "Outcome", "Model", "Model role", "Ljung-Box p value",
                  "Residual autocorrelation detected at the 5% level",
                  "Eligible under the prespecified rule", "MAPE (%)", "MASE",
                  "Empirical coverage of the nominal 95% range (%)",
                  "Rank by MAPE among all four candidates", "Selected")),

  # S15 - rolling-origin evaluation of the COMPLETE aggregate interval construction (Major 1).
  list(out = "Supplementary Table 15.xlsx", sheets = list(
    list(sheet = "A - Pooled", dir = "R5",
         csv = "R5_aggregate_interval_coverage_overall.csv",
         keep = c("model", "n_origins", "n_obs", "DALY_MAPE", "DALY_coverage",
                  "VLW_MAPE", "VLW_coverage", "mean_relative_width_pct",
                  "total_nonpositive_group_draws"),
         header = c("Model", "Rolling origins", "Forecast-actual pairs",
                    "Aggregate DALY MAPE (%)",
                    "Empirical coverage of the aggregate DALY range (%)",
                    "Aggregate VLW MAPE (%)",
                    "Empirical coverage of the aggregate VLW range (%)",
                    "Mean range width as a percentage of the point estimate",
                    "Non-positive simulation draws")),
    list(sheet = "B - By horizon", dir = "R5",
         csv = "R5_aggregate_interval_coverage_by_horizon.csv",
         keep = c("model", "h", "n_origins", "n_obs", "DALY_MAPE", "DALY_coverage",
                  "VLW_MAPE", "VLW_coverage", "mean_relative_width_pct"),
         header = c("Model", "Forecast horizon (years)", "Rolling origins",
                    "Forecast-actual pairs", "Aggregate DALY MAPE (%)",
                    "Empirical coverage of the aggregate DALY range (%)",
                    "Aggregate VLW MAPE (%)",
                    "Empirical coverage of the aggregate VLW range (%)",
                    "Mean range width as a percentage of the point estimate"))
  )),

  # S16 - full disclosure of the joint simulation (Major 3).
  list(out = "Supplementary Table 16.xlsx", sheets = list(
    list(sheet = "A - Specification", dir = "R5",
         csv = "R5_joint_simulation_specification.csv",
         keep = c("Element", "Specification"), header = c("Element", "Specification")),
    list(sheet = "B - Residual correlation", dir = "R5",
         csv = "R5_residual_correlation_matrix.csv",
         keep = c("Matrix", "Group", "Low income", "Lower middle income",
                  "Upper middle income"),
         header = c("Matrix", "Income group", "Low income", "Lower middle income",
                    "Upper middle income")),
    list(sheet = "C - Eigenvalues", dir = "R5", csv = "R5_correlation_eigenvalues.csv",
         keep = c("component", "eigenvalue_raw", "eigenvalue_after_clip",
                  "clip_was_binding"),
         header = c("Component", "Eigenvalue before adjustment",
                    "Eigenvalue after clipping", "Clip was binding")),
    list(sheet = "D - Non-negativity", dir = "R5", csv = "R5_simulation_nonnegativity.csv",
         keep = c("model", "Year", "draws", "n_group_draws_nonpositive",
                  "pct_group_draws_nonpositive", "n_total_draws_nonpositive",
                  "pct_total_draws_nonpositive"),
         header = c("Model", "Year", "Draws per horizon",
                    "Non-positive group draws", "Non-positive group draws (%)",
                    "Non-positive aggregate draws", "Non-positive aggregate draws (%)"))
  )),

  # S17 - the composition-drift rate is an endpoint CAGR; sheet B bounds that choice (Major 3).
  list(out = "Supplementary Table 17.xlsx", sheets = list(
    list(sheet = "A - Drift basis", dir = "R5",
         csv = "R5_composition_drift_basis_comparison.csv",
         keep = c("LMIC_group", "Vbar_1990", "Vbar_2023", "endpoint_cagr_pct",
                  "loglinear_cagr_pct", "loglinear_r2", "absolute_difference_pp"),
         header = c("Income group", "Effective VSLY in 1990", "Effective VSLY in 2023",
                    "Endpoint CAGR (%/year, published basis)",
                    "Log-linear OLS CAGR (%/year, bounding check)",
                    "Log-linear R squared", "Difference (percentage points per year)")),
    list(sheet = "B - Impact in 2050", dir = "R5",
         csv = "R5_composition_drift_impact_comparison.csv",
         keep = c("Basis", "VLW_2050_fixed_composition", "VLW_2050_drift_adjusted",
                  "difference_pct"),
         header = c("Basis", "Fixed-composition VLW in 2050 (billion constant-2023 USD)",
                    "Drift-adjusted VLW in 2050 (billion constant-2023 USD)",
                    "Difference (%)"))
  )),

  # S18 - data provenance manifest (Minor 1). Sheet A is the source-level query manifest; sheet B
  #       is the file-level manifest with checksums.
  list(out = "Supplementary Table 18.xlsx", sheets = list(
    list(sheet = "A - Query manifest", dir = "R5", csv = "R5_query_manifest.csv",
         keep = c("Source", "Dataset", "Access_tool_or_portal",
                  "Query_parameters", "Indicator_or_cause_codes", "Classification_vintage",
                  "Downloaded_files", "Licence_and_redistribution"),
         header = c("Source", "Dataset", "Access tool or portal",
                    "Query parameters", "Indicator or cause codes", "Classification vintage",
                    "Downloaded files", "Licence and redistribution")),
    list(sheet = "B - File manifest", dir = "R5",
         csv = "R5_data_provenance_manifest.csv",
         keep = c("file", "bytes", "modified_utc", "md5", "rows", "columns"),
         header = c("Input file (relative to data_raw/)", "Size (bytes)",
                    "File timestamp, UTC (NOT the query date)", "MD5 checksum",
                    "Rows", "Columns"))
  )),

  # S19 - the MASE definition, so the manuscript and the deposited code state the same thing
  #       (Major 2).
  list(out = "Supplementary Table 19.xlsx", dir = "R5", csv = "R5_MASE_definition.csv",
       keep = c("Item", "Definition"), header = c("Item", "Definition"))
)

# Optional exact output-name filter used by the standalone per-table scripts in table/code/.
# Keeping the filter here ensures that the all-table and one-table paths execute identical logic.
if (exists("TABLE_FILTER", inherits = FALSE)) {
  spec <- Filter(function(s) identical(s$out, TABLE_FILTER), spec)
  if (!length(spec)) stop("Unknown table requested: ", TABLE_FILTER)
}

# -- Per column: if every value is numeric (thousands commas / scientific notation allowed),
# convert to numeric, otherwise keep as text. This is what prevents the 1000x error: numeric
NUMRE <- "^[+-]?(\\d{1,3}(,\\d{3})+|\\d+)(\\.\\d+)?([eE][+-]?\\d+)?$"
as_col <- function(x) {
  x <- trimws(as.character(x))
  v <- x[!is.na(x) & x != ""]
  if (length(v) && all(grepl(NUMRE, v))) as.numeric(gsub(",", "", x)) else x
}

# cells are written as numbers, so Excel never parses them from text.
#
# Missing values: R4 stored the CSV "NA" as literal text; this script writes an empty cell. An
# xlsx column can hold only one type, so keeping "NA" as text would demote alpha/beta/phi/AIC/
# AICc/BIC in Supplementary Table 6 (NA on ARIMA rows) and the total row of Supplementary Table 8
# to text, making them non-computable. 34 cells are affected (32 in Supp 6, 2 in Supp 8); an empty
# cell carries the same "missing" meaning and preserves the numeric type of the column.

# -- Remove CJK script-fallback fonts from the OOXML theme --------------------------------------
# Every OOXML file carries a default theme that declares East Asian fallback typefaces
# (<a:font script="Hans" typeface="SimSun"/> and similar, written with the native font names).
# They are boilerplate rather than content, but they leave non-ASCII strings inside the archive.
# This strips any <a:font> element whose typeface is not plain ASCII; Office falls back to system
# defaults when a script-specific font is absent, so rendering is unaffected.
strip_cjk_theme <- function(path) {
  tmp <- file.path(tempdir(), paste0("x_", tools::file_path_sans_ext(basename(path))))
  unlink(tmp, recursive = TRUE); dir.create(tmp, recursive = TRUE)
  utils::unzip(path, exdir = tmp)
  th <- file.path(tmp, "xl", "theme", "theme1.xml")
  if (file.exists(th)) {
    x <- paste(readLines(th, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    x <- gsub('<a:font script="[^"]*" typeface="[^"]*[^\\x20-\\x7E][^"]*"/>', '', x, perl = TRUE)
    con <- file(th, open = "wb"); writeBin(charToRaw(x), con); close(con)
  }
  zip::zip(zipfile = normalizePath(path, mustWork = FALSE),
           files = list.files(tmp, recursive = TRUE, all.files = TRUE, no.. = TRUE),
           root = tmp)
  invisible(path)
}

read_table_sheet <- function(s) {
  src_dir <- if (is.null(s$dir)) CSV_DIR else file.path(CODE_DIR, "outputs", s$dir)
  f <- file.path(src_dir, s$csv)
  if (!file.exists(f)) stop("Missing source CSV: ", f)
  d <- read_csv(f, col_types = cols(.default = col_character()), progress = FALSE)
  missing <- setdiff(s$keep, names(d))
  if (length(missing)) stop(s$csv, " is missing columns: ", paste(missing, collapse = ", "))
  d <- d[, s$keep, drop = FALSE]
  stopifnot(length(s$header) == ncol(d))
  d[] <- lapply(d, as_col)
  names(d) <- s$header
  d
}

cat("Source CSVs: ", rel_path(CSV_DIR), "\nOutput     : ", rel_path(OUT_DIR),
    "\n\n", sep = "")
n_num <- 0L
for (s in spec) {
  sheet_specs <- if (is.null(s$sheets)) list(c(s, list(sheet = "Sheet1"))) else s$sheets
  sheets <- lapply(sheet_specs, read_table_sheet)
  names(sheets) <- vapply(sheet_specs, function(x) x$sheet, character(1))
  n_num <- n_num + sum(vapply(sheets, function(d) sum(vapply(d, is.numeric, logical(1))), integer(1)))

  out_path <- file.path(OUT_DIR, s$out)
  write_xlsx(sheets, out_path, format_headers = TRUE)
  strip_cjk_theme(out_path)

  dims <- paste(vapply(seq_along(sheets), function(i) {
    sprintf("%s: %d x %d", names(sheets)[i], nrow(sheets[[i]]), ncol(sheets[[i]]))
  }, character(1)), collapse = "; ")
  sources <- paste(vapply(sheet_specs, function(x) x$csv, character(1)), collapse = " + ")
  cat(sprintf("  %-30s <- %-58s [%s]\n", s$out, sources, dims))
}
cat(sprintf("\nDone: %d xlsx files; %d columns written as numeric.\n", length(spec), n_num))
