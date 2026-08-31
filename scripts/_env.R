# ==============================================================================
# _env.R - pin the graphics stack for reproducibility
#
# The submitted figures were produced under ggplot2 3.5.1 / scales 1.3.0. ggplot2 4.0 rebuilt
# plot objects on S7, which is incompatible with the `&` operator of patchwork < 1.3: the
# expression `plot + plot_annotation(...) & theme(...)`, used by every multi-panel figure in this
# analysis, fails with "operations are possible only for numeric, logical or complex types"
# because Ops.S7_object and &.gg cannot both dispatch.
#
# code/.Rlibs holds the pinned versions and is prepended to the library path when present, so the
# analysis reproduces without altering the user's main R library. If the directory is absent the
# script continues with whatever is installed and prints the versions it actually used, so a
# mismatch is visible in the run log rather than silent.
#
# To recreate the pinned library from scratch:
#   Rscript -e 'dir.create("code/.Rlibs"); \
#     install.packages(c("https://cran.r-project.org/src/contrib/Archive/scales/scales_1.3.0.tar.gz", \
#                        "https://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.5.1.tar.gz"), \
#                      repos = NULL, type = "source", lib = "code/.Rlibs")'
# ==============================================================================
local({
  lib <- file.path(getwd(), ".Rlibs")
  if (dir.exists(lib)) .libPaths(c(normalizePath(lib), .libPaths()))
})

# ------------------------------------------------------------------------------
# rel_path(): render a path relative to the package root for console and log output.
#
# A deposited reproducibility package should not print the absolute location of one
# person's working copy. Absolute paths also carry whatever characters happen to be in the
# parent directories, which is how non-ASCII text reached an otherwise all-English run log.
# Everything the scripts print is therefore relativised to code/.
# ------------------------------------------------------------------------------
rel_path <- function(p, base = getwd()) {
  p <- suppressWarnings(normalizePath(p, mustWork = FALSE))
  b <- suppressWarnings(normalizePath(base, mustWork = FALSE))
  if (!nzchar(p)) return(p)
  if (identical(p, b)) return(".")
  if (startsWith(p, paste0(b, .Platform$file.sep)))
    return(substring(p, nchar(b) + 2L))
  basename(p)
}
