#!/usr/bin/env Rscript
# Regenerate Supplementary Tables 1-14 in table/. Each table runs in a clean R process.

.self_path <- function() {
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a) != 1L) return(normalizePath("."))
  dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", a), fixed = TRUE)))
}
HERE <- .self_path()

t0 <- Sys.time()
failed <- integer()
for (i in 1:14) {
  script <- file.path(HERE, sprintf("Supplementary Table %d.R", i))
  cat(sprintf("[%2d/14] Supplementary Table %d ... ", i, i))
  status <- system2("Rscript", shQuote(script), stdout = NULL, stderr = NULL)
  if (status == 0L) cat("OK\n") else {
    cat("FAILED (exit ", status, ")\n", sep = "")
    failed <- c(failed, i)
  }
}

cat(sprintf("\nCompleted %d/14 in %.1f s\n", 14L - length(failed),
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))
if (length(failed)) {
  cat("Failed tables: ", paste(failed, collapse = ", "), "\n", sep = "")
  quit(status = 1L)
}
