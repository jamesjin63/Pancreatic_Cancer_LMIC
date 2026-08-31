#!/usr/bin/env Rscript
# ==============================================================================
# rebuild_fig_common.R - regenerate fig/code/_common.R from the canonical script
#
# fig/code/_common.R is the canonical script's preamble, copied verbatim. Keeping it by hand is
# how the two drifted apart at the R5 round: the canonical script was edited, _common.R was not,
# and the per-figure scripts silently kept plotting from the old definitions. This script makes
# the copy mechanical, so the drift cannot recur.
#
# The cut point is located by the section-3 marker, never by a line number, for the same reason.
#
# Run: Rscript scripts/rebuild_fig_common.R
# ==============================================================================
self <- function() {
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a) != 1L) return(normalizePath("."))
  dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", a), fixed = TRUE)))
}
CODE_DIR <- normalizePath(file.path(self(), ".."))
setwd(CODE_DIR)

src  <- readLines(file.path("scripts", "run_LMIC_Pancreatic_VLW_v2.R"))
mark <- grep("^# 3\\. LOOP OVER IE", src)
if (length(mark) != 1L) stop("Could not locate the section-3 marker in the canonical script.")
end  <- mark - 2L

header <- c(
  "# ==============================================================================",
  "# _common.R - shared preamble for every figure script",
  "#",
  sprintf("# Contents = scripts/run_LMIC_Pancreatic_VLW_v2.R lines 1-%d, extracted verbatim:", end),
  "#   paths, reference constants, palettes/themes, helper functions, data loading (section 1),",
  "#   VLW computation (section 2), and the forecast helper functions.",
  "# Nothing is modified except the directory resolution and the environment pin below.",
  "#",
  "# GENERATED FILE - do not edit by hand. Regenerate with:",
  "#   Rscript scripts/rebuild_fig_common.R",
  "#",
  "# Sourced by the figure scripts; not run on its own.",
  "# ==============================================================================",
  "",
  "if (!exists(\"FIG_CODE_DIR\")) stop(\"Call this from a figure script in fig/code/; do not run _common.R directly.\")",
  "CODE_DIR <- normalizePath(file.path(FIG_CODE_DIR, \"..\", \"..\"))   # fig/code -> fig -> code",
  "FIG_DIR  <- normalizePath(file.path(FIG_CODE_DIR, \"..\"))          # fig/",
  "dir.create(FIG_DIR, showWarnings=FALSE, recursive=TRUE)",
  "setwd(CODE_DIR)   # the canonical script reads data_raw/ relative to code/",
  "source(file.path(CODE_DIR, \"scripts\", \"_env.R\"))   # pin ggplot2/scales; see that file",
  "",
  sprintf("# ------------- verbatim from run_LMIC_Pancreatic_VLW_v2.R:1-%d below -------------", end))

writeLines(c(header, src[seq_len(end)]), file.path("fig", "code", "_common.R"))
cat("Rebuilt fig/code/_common.R from canonical lines 1-", end, "\n", sep = "")
