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
CODE_DIR <- normalizePath(file.path(.self(), ".."))
CSV_DIR  <- file.path(CODE_DIR, "outputs", "R3_submission_tables")
args     <- commandArgs(trailingOnly = TRUE)
OUT_DIR  <- if (length(args) >= 1L) args[1] else file.path(CODE_DIR, "outputs", "submission_tables_xlsx")
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
       keep = c("Model", "Year", "Group", "VLW (billion constant-2023 USD; 95% PI)",
                "DALYs (thousands; 95% PI)"),
       header = c("Model", "Year", "Group", "VLW (billion constant-2023 USD; 95% PI)",
                  "DALYs (thousands; 95% PI)")),

  list(out = "Table 6.xlsx", csv = "Main Table 6_income-elasticity sensitivity.csv",
       keep = c("Income Elasticity", "VLW base 3% (billion constant-2023 USD; point estimate)",
                "VLW undiscounted (billion constant-2023 USD)", "VLW/GDP (%; point estimate)"),
       header = c("Income Elasticity", "VLW base 3% (billion constant-2023 USD)",
                  "VLW undiscounted (billion constant-2023 USD)", "VLW/GDP (%)")),

  # -- Supplementary tables: R4 renumbering --
  list(out = "Supplementary Table 1.xlsx", csv = "Supplementary Table 1_country burden.csv",
       keep = c("Rank", "Country", "Income Group", "GDP pc (PPP)",
                "DALYs (thousands; point estimate)",
                "VLW (billion constant-2023 USD; point estimate)",
                "VLW/GDP (%; point estimate)"),
       header = c("Rank", "Country", "Income Group",
                  "GDP per capita (PPP, 2023 international $)", "DALYs (thousands)",
                  "VLW (billion constant-2023 USD)", "VLW/GDP (%)")),

  list(out = "Supplementary Table 2.xlsx",                       # <- R3 Supplementary Table 8
       csv = "Supplementary Table 8_income-gradient regression HC3.csv",
       keep = c("term", "estimate", "HC3_standard_error", "lower_95_CI", "upper_95_CI",
                "t_statistic", "df", "p_value_two_sided", "n", "R_squared",
                "adjusted_R_squared"),
       header = c("Term", "Estimate", "HC3 standard error", "Lower 95% CI", "Upper 95% CI",
                  "t statistic", "df", "Two-sided p value", "n", "R2", "Adjusted R2")),

  list(out = "Supplementary Table 3.xlsx",                       # <- R3 Supplementary Table 2
       csv = "Supplementary Table 2_country and sex burden.csv",
       keep = c("Country", "Sex", "Income Group", "DALYs (point estimate)",
                "VLW (billion constant-2023 USD; point estimate)",
                "VLW undiscounted (billion constant-2023 USD)",
                "VLW/GDP (%; point estimate)"),
       header = c("Country", "Sex", "Income Group", "DALYs",
                  "VLW (billion constant-2023 USD)",
                  "VLW undiscounted (billion constant-2023 USD)", "VLW/GDP (%)")),

  list(out = "Supplementary Table 4.xlsx",                       # <- R3 Supplementary Table 3
       csv = "Supplementary Table 3_annual ETS and ARIMA projections.csv",
       keep = c("model", "Year", "Group", "DALY", "DALY_lower_95_PI", "DALY_upper_95_PI",
                "VLW_billion", "VLW_lower_95_PI_billion", "VLW_upper_95_PI_billion"),
       header = c("model", "Year", "Group", "DALYs", "DALYs lower 95% PI",
                  "DALYs upper 95% PI", "VLW (billion constant-2023 USD)",
                  "VLW lower 95% PI (billion constant-2023 USD)",
                  "VLW upper 95% PI (billion constant-2023 USD)")),

  list(out = "Supplementary Table 5.xlsx",                       # <- R3 Supplementary Table 4
       csv = "Supplementary Table 4_holdout validation.csv",
       keep = c("series", "outcome", "model", "holdout", "MAE", "RMSE", "MAPE"),
       header = c("Series", "Outcome", "Model", "Holdout period", "MAE", "RMSE", "MAPE")),

  list(out = "Supplementary Table 6.xlsx",                       # <- R3 Supplementary Table 5
       csv = "Supplementary Table 5_forecast and residual diagnostics.csv",
       keep = c("series", "outcome", "model", "method", "alpha", "beta", "phi", "sigma2",
                "AIC", "AICc", "BIC", "RMSE", "Ljung_Box_lag", "Ljung_Box_fitdf",
                "Ljung_Box_p", "residual_autocorrelation_signal"),
       header = c("Series", "Outcome", "Model", "Method", "alpha", "beta", "phi",
                  "Error variance (σ2)", "AIC", "AICc", "BIC", "RMSE",
                  "Ljung-Box lag", "Ljung-Box degrees of freedom", "Ljung-Box p value",
                  "Residual autocorrelation signal")),

  list(out = "Supplementary Table 7.xlsx",                       # <- R3 Supplementary Table 6
       csv = "Supplementary Table 6_projection reconciliation.csv",
       keep = c("model", "Year", "Group", "derived_VLW_billion", "direct_VLW_billion",
                "difference_billion", "percent_difference"),
       header = c("model", "Year", "Group",
                  "VLW derived from DALY projections (billion constant-2023 USD)",
                  "Independently projected VLW (billion constant-2023 USD)",
                  "Absolute difference (billion constant-2023 USD)",
                  "Relative difference (%)")),

  list(out = "Supplementary Table 8.xlsx",                       # <- R3 Supplementary Table 7
       csv = "Supplementary Table 7_excluded-country DALY burden.csv",
       keep = c("location_name", "LMIC_group", "DALY", "income_group_total_DALYs",
                "percent_of_income_group_DALYs", "all_128_LMIC_DALYs",
                "percent_of_all_128_LMIC_DALYs"),
       header = c("Country", "Income group", "DALYs", "Total DALYs in income group",
                  "Percentage of income-group DALYs (%)",
                  "Total DALYs among all 128 LMICs",
                  "Percentage of total DALYs among all 128 LMICs (%)")),

  list(out = "Supplementary Table 9.xlsx",
       csv = "Supplementary Table 9_unweighted country ASR means.csv",
       keep = c("LMIC_group", "year", "R"),
       header = c("Income group", "Year",
                  "Mean country-specific age-standardized DALY rate (per 100,000 population)"))
)

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

cat("Source CSVs: ", CSV_DIR, "\nOutput     : ", OUT_DIR, "\n\n", sep = "")
n_num <- 0L
for (s in spec) {
  f <- file.path(CSV_DIR, s$csv)
  if (!file.exists(f)) stop("Missing source CSV: ", f)
  # Read everything as character to bypass readr type guessing, then decide column by column
  d <- read_csv(f, col_types = cols(.default = col_character()), progress = FALSE)

  missing <- setdiff(s$keep, names(d))
  if (length(missing)) stop(s$csv, " is missing columns: ", paste(missing, collapse = ", "))
  d <- d[, s$keep, drop = FALSE]

  stopifnot(length(s$header) == ncol(d))
  d[] <- lapply(d, as_col)
  n_num <- n_num + sum(vapply(d, is.numeric, logical(1)))
  names(d) <- s$header

  out_path <- file.path(OUT_DIR, s$out)
  write_xlsx(setNames(list(d), "Sheet1"), out_path, format_headers = TRUE)
  strip_cjk_theme(out_path)

  cat(sprintf("  %-30s <- %-58s %3d rows x %2d cols\n", s$out, s$csv, nrow(d), ncol(d)))
}
cat(sprintf("\nDone: %d xlsx files; %d columns written as numeric.\n", length(spec), n_num))
