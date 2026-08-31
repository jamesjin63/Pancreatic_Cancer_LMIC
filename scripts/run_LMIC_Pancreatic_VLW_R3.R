#!/usr/bin/env Rscript

# Canonical entry point for the R3 revision.
# It can be called from either the repository root or the code/ directory:
#   Rscript code/scripts/run_LMIC_Pancreatic_VLW_R3.R
#   Rscript scripts/run_LMIC_Pancreatic_VLW_R3.R

script_arg <- grep("^--file=", commandArgs(trailingOnly=FALSE), value=TRUE)
if (length(script_arg) != 1L) stop("Unable to determine the R3 entry-point location.")
script_path <- normalizePath(sub("^--file=", "", script_arg), mustWork=TRUE)
code_dir <- dirname(dirname(script_path))
canonical_script <- file.path(code_dir,"scripts","run_LMIC_Pancreatic_VLW_v2.R")
if (!file.exists(canonical_script)) stop("Canonical workflow not found: ",canonical_script)
old_wd <- setwd(code_dir)
on.exit(setwd(old_wd),add=TRUE)
source(file.path(code_dir,"scripts","_env.R"))
source(canonical_script,chdir=FALSE)
