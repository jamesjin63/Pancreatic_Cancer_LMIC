#!/usr/bin/env Rscript
TABLE_NUMBER <- 8L
source(file.path(dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value=TRUE)), fixed=TRUE))), "_run_table.R"))
