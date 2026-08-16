# Shared runner for Supplementary Tables 1-14.
# Each numbered wrapper sets TABLE_NUMBER and sources this file. The actual workbook specification
# and writing logic remain in scripts/make_submission_xlsx.R, so individual and batch builds cannot
# drift apart.

if (!exists("TABLE_NUMBER", inherits = FALSE) ||
    length(TABLE_NUMBER) != 1L || TABLE_NUMBER < 1L || TABLE_NUMBER > 14L) {
  stop("A wrapper must set TABLE_NUMBER to an integer from 1 through 14.")
}

.wrapper_dir <- function() {
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a) != 1L) return(normalizePath("."))
  dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", a), fixed = TRUE)))
}

TABLE_CODE_DIR <- .wrapper_dir()
CODE_DIR_OVERRIDE <- normalizePath(file.path(TABLE_CODE_DIR, "..", ".."))
OUT_DIR_OVERRIDE <- normalizePath(file.path(TABLE_CODE_DIR, ".."), mustWork = FALSE)
TABLE_FILTER <- sprintf("Supplementary Table %d.xlsx", as.integer(TABLE_NUMBER))

source(file.path(CODE_DIR_OVERRIDE, "scripts", "make_submission_xlsx.R"), chdir = FALSE)
