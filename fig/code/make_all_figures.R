#!/usr/bin/env Rscript
# ==============================================================================
# make_all_figures.R - regenerate all 19 figures in fig/ (7 main + 12 supplementary)
#
# Each figure script in this directory (named after its manuscript figure) is invoked in its
# own R process, so no state is shared between them.
#
# Run:  Rscript "fig/code/make_all_figures.R"
# ==============================================================================

# R's commandArgs() encodes spaces in the --file= path as "~+~". This directory path contains
# spaces, so that encoding has to be reversed before the path can be used.
.self_path <- function() {
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a) != 1L) return(normalizePath("."))
  dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", a), fixed = TRUE)))
}
HERE <- .self_path()

figs <- c(
  sprintf("Figure %d %s", 1:7, c("2023 snapshot", "trends",
    "unweighted mean country ASR and fold change", "maps", "age specific",
    "forecast 2050", "IE sensitivity")),
  sprintf("Supplementary Figure %d %s", 1:12, c(
    "IE0.5 snapshot", "IE0.5 trends", "IE0.5 unweighted mean country ASR",
    "IE0.5 maps", "IE0.5 age specific", "IE0.5 forecast",
    "IE1.5 snapshot", "IE1.5 trends", "IE1.5 unweighted mean country ASR",
    "IE1.5 maps", "IE1.5 age specific", "IE1.5 forecast")))

t0 <- Sys.time(); fail <- character()
for (f in figs) {
  s <- file.path(HERE, paste0(f, ".R"))
  cat(sprintf("[%2d/%d] %s ... ", match(f, figs), length(figs), f))
  st <- system2("Rscript", shQuote(s), stdout = NULL, stderr = NULL)
  if (st == 0L) cat("OK\n") else { cat("FAILED (exit ", st, ")\n", sep = ""); fail <- c(fail, f) }
}
cat(sprintf("\nCompleted %d/%d in %.0f s\n", length(figs) - length(fail), length(figs),
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))
if (length(fail)) { cat("Failed:\n"); cat(paste0("  - ", fail, collapse = "\n"), "\n"); quit(status = 1L) }
