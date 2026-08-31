# ==============================================================================
# _common.R - shared preamble for every figure script
#
# Contents = scripts/run_LMIC_Pancreatic_VLW_v2.R lines 1-455, extracted verbatim:
#   paths, reference constants, palettes/themes, helper functions, data loading (section 1),
#   VLW computation (section 2), and the forecast helper functions.
# Nothing is modified except the directory resolution and the environment pin below.
#
# GENERATED FILE - do not edit by hand. Regenerate with:
#   Rscript scripts/rebuild_fig_common.R
#
# Sourced by the figure scripts; not run on its own.
# ==============================================================================

if (!exists("FIG_CODE_DIR")) stop("Call this from a figure script in fig/code/; do not run _common.R directly.")
CODE_DIR <- normalizePath(file.path(FIG_CODE_DIR, "..", ".."))   # fig/code -> fig -> code
FIG_DIR  <- normalizePath(file.path(FIG_CODE_DIR, ".."))          # fig/
dir.create(FIG_DIR, showWarnings=FALSE, recursive=TRUE)
setwd(CODE_DIR)   # the canonical script reads data_raw/ relative to code/
source(file.path(CODE_DIR, "scripts", "_env.R"))   # pin ggplot2/scales; see that file

# ------------- verbatim from run_LMIC_Pancreatic_VLW_v2.R:1-455 below -------------
################################################################################
# LMIC Pancreatic Cancer — Value of Lost Welfare (VLW) Analysis — R3 CANONICAL WORKFLOW
# GBD 2023 · 1990-2023 · Forecast to 2050 · IE = 0.5, 1.0, 1.5
#
# R3 statistical policies implemented here:
#   1. Historical marginal GBD bounds are never summed; aggregate historical
#      uncertainty intervals are not reported without matched draws.
#   2. DALYs are forecast first, then monetised with fixed 2023 group-specific
#      effective VSLYs. Direct VLW forecasts are secondary diagnostics only.
#   3. Every reported forecast interval explicitly uses level = 95.
#   4. All-LMIC prediction intervals use 50,000 joint paths with empirical
#      residual correlation; subgroup interval endpoints are never summed.
#   5. Non-seasonal ARIMA is the primary model and damped-trend ETS is the
#      sensitivity model. Selected by a prespecified rule (lowest pooled
#      rolling-origin MAPE among candidates for which the Ljung-Box test does not detect
#      residual autocorrelation at the 5% level); see
#      scripts/run_R5_reviewer_analyses.R.
#   6. The rate panel is the unweighted mean of country-specific ASRs.
#
# The workflow also uses one consistent discounted-annuity VSLY definition,
# derives both-sex values as male + female, reports IE = 0.5/1.0/1.5 valuation
# scenarios separately from statistical uncertainty, and writes full diagnostics.
#
# NOTE: with a uniform VSLY, age-specific VLW is proportional to age-specific
#       DALYs, so VLW peaks in the SAME band as DALYs (65–69); the previous
#       "VLW peaks later (70–74)" result was an artefact of the inconsistent
#       age-specific denominator and is no longer reported (reviewer m5).
################################################################################

suppressMessages({
  library(tidyverse); library(sf); library(patchwork); library(scales); library(forecast)
})
options(warn = -1, dplyr.summarise.inform = FALSE)

# ── Paths ─────────────────────────────────────────────────────────────────────
# Reproducibility-package layout: run from reviseR2/code with
# Rscript scripts/run_LMIC_Pancreatic_VLW_v2.R
base       <- normalizePath(".")
daly_dir   <- file.path(base, "data_raw/gbd_daly_yearly")
hale_file  <- file.path(base, "data_raw/external_metadata/HALE.csv")
gdp_file   <- file.path(base, "data_raw/external_metadata/gdp.csv")
lmic_file  <- file.path(base, "data_raw/external_metadata/204_with_LMIC.csv")
world_file <- file.path(base, "data_raw/external_metadata/df_world2.geojson")
out_root   <- file.path(base, "outputs/results_VLW")
diag_dir   <- file.path(out_root, "diagnostics")
r3_dir     <- file.path(base, "outputs/R3")
submission_fig_dir <- file.path(base, "outputs/R3_submission_figures")
submission_table_dir <- file.path(base, "outputs/R3_submission_tables")
dir.create(out_root, showWarnings = FALSE, recursive = TRUE)
dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(r3_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(submission_fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(submission_table_dir, showWarnings = FALSE, recursive = TRUE)

# ── Reference constants (USA 2023) ────────────────────────────────────────────
VSL_USA       <- 13.2e6      # US DOT 2023 VSL
GDP_pc_USA    <- 82304.62    # World Bank, USA GDP per capita PPP 2023
discount_rate <- 0.03        # base-case annual discount rate
ie_values     <- c(0.5, 1.0, 1.5)
ie_base       <- 1.0

# annuity factor: present value of $1/yr for T years at rate r  (r=0 -> T)
annuity <- function(T, r) ifelse(T <= 0, NA_real_, ifelse(r == 0, T, (1 - (1 + r)^(-T)) / r))

# ── Palette / themes (unchanged) ──────────────────────────────────────────────
pal <- list(blue="#1B4F72", red="#C0392B", teal="#117A65",
            orange="#E67E22", grey="#5D6D7E", purple="#6C3483", pink="#CB4335")
income_pal <- c("Low income"="#C0392B","Lower middle income"="#E67E22","Upper middle income"="#117A65")
sex_pal    <- c("Male"="#1B4F72","Female"="#C0392B","Both"="#5D6D7E")
income_fct <- c("Low income","Lower middle income","Upper middle income")
map_blue <- c("#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1","#6BAED6","#4292C6","#2171B5","#084594")
map_red  <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#99000D")

theme_nm <- function(bs=7.5) theme_minimal(base_size=bs) %+replace% theme(
  plot.title=element_blank(), plot.subtitle=element_blank(),
  plot.tag=element_text(size=10,face="bold",hjust=0),
  axis.title=element_text(size=bs,face="bold",colour="grey20"),
  axis.text=element_text(size=bs-0.5,colour="grey30"),
  axis.line=element_line(colour="grey50",linewidth=0.3),
  legend.title=element_text(size=bs-0.5,face="bold"),
  legend.text=element_text(size=bs-1),
  legend.key.size=unit(0.3,"cm"), legend.background=element_blank(),
  panel.grid.major=element_line(colour="grey93",linewidth=0.25),
  panel.grid.minor=element_blank(),
  strip.text=element_text(size=bs,face="bold"),
  strip.background=element_rect(fill="grey96",colour=NA),
  plot.margin=margin(3,5,3,3,"pt"))
theme_nm_map <- function(bs=7.5) theme_void(base_size=bs) %+replace% theme(
  plot.title=element_blank(), plot.tag=element_text(size=10,face="bold",hjust=0),
  legend.position="right", legend.title=element_text(size=bs-0.5,face="bold"),
  legend.text=element_text(size=bs-1), legend.key.size=unit(0.3,"cm"),
  plot.margin=margin(2,2,2,2,"pt"))

# ── Helpers ───────────────────────────────────────────────────────────────────
gbd_to_wb <- c(
  "Bolivia (Plurinational State of)"="Bolivia","Congo"="Congo, Rep.",
  "Côte d'Ivoire"="Cote d'Ivoire",
  "Democratic People's Republic of Korea"="Korea, Dem. People's Rep.",
  "Democratic Republic of the Congo"="Congo, Dem. Rep.",
  "Egypt"="Egypt, Arab Rep.","Gambia"="Gambia, The",
  "Iran (Islamic Republic of)"="Iran, Islamic Rep.","Kyrgyzstan"="Kyrgyz Republic",
  "Lao People's Democratic Republic"="Lao PDR",
  "Micronesia (Federated States of)"="Micronesia, Fed. Sts.",
  "Palestine"="West Bank and Gaza","Republic of Moldova"="Moldova",
  "Saint Lucia"="St. Lucia","Saint Vincent and the Grenadines"="St. Vincent and the Grenadines",
  "Somalia"="Somalia, Fed. Rep.","Syrian Arab Republic"="Syrian Arab Republic",
  "Türkiye"="Turkiye","United Republic of Tanzania"="Tanzania","Yemen"="Yemen, Rep.")
norm_name <- function(x) x %>% str_replace_all("[\u2018\u2019]","'") %>%
  str_replace_all("\u00a0"," ") %>% str_squish() %>% str_to_lower()
fmt_ui <- function(v,lo,hi,d=1,s=1) paste0(
  formatC(v/s,format="f",digits=d,big.mark=",")," (",
  formatC(lo/s,format="f",digits=d,big.mark=","),"\u2013",
  formatC(hi/s,format="f",digits=d,big.mark=","),")")

std_ages <- c("<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
  "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
  "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
  "75-79 years","80-84 years","85-89 years","90-94 years","95+ years")
age_mids <- c(2.5,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87,92,97)
age_ids <- c(1,6:20,30:32,235)
age_lookup <- tibble(age_id=age_ids, age_name=std_ages, age_mid=age_mids,
                     age_order=seq_along(std_ages))
sex_lookup <- c(`1`="Male", `2`="Female", `3`="Both")

cat("================================================================\n")
cat("  Pancreatic LMIC VLW  R3 canonical  · IE=0.5/1.0/1.5\n")
cat("================================================================\n\n")

# ======================================================================
# 1. READ DATA
# ======================================================================
cat("[1] Reading data ...\n")

df_lmic_raw <- read_csv(lmic_file, show_col_types=FALSE)
df_lmic <- df_lmic_raw %>% filter(LMIC==1) %>%
  select(location_id,location_name,LMIC_group) %>%
  mutate(wb=ifelse(location_name %in% names(gbd_to_wb),gbd_to_wb[location_name],location_name),
         wb=case_when(location_id==155 ~ "Turkiye",
                      location_id==205 ~ "Cote d'Ivoire",
                      TRUE ~ wb),
         wb_n=norm_name(wb))

df_gdp <- read_csv(gdp_file,show_col_types=FALSE) %>%
  filter(year==2023,!is.na(NY.GDP.PCAP.PP.CD),!is.na(NY.GDP.MKTP.PP.CD)) %>%
  select(country,GDP_pc=NY.GDP.PCAP.PP.CD,GDP_tot=NY.GDP.MKTP.PP.CD) %>%
  mutate(cn=norm_name(country))

df_econ <- df_lmic %>% left_join(df_gdp,by=c("wb_n"="cn")) %>%
  filter(!is.na(GDP_pc)) %>% select(location_id,location_name,LMIC_group,GDP_pc,GDP_tot)
lmic_ids <- df_econ$location_id
stopifnot(nrow(df_lmic)==128L, nrow(df_econ)==122L)

# coverage log (reviewer m4)
n_lmic_total <- nrow(df_lmic)
n_with_gdp   <- nrow(df_econ)

daly_files <- sort(list.files(daly_dir, pattern="^measure2_DALYs_year[0-9]{4}\\.csv$",
                              full.names=TRUE))
stopifnot(length(daly_files) == 34L)
df_daly_raw <- map_dfr(daly_files, function(f) {
  read_csv(f, show_col_types=FALSE) %>%
    filter(measure==2, cause==456, location %in% df_lmic$location_id,
           sex %in% c(1,2,3), age %in% c(22,27,age_ids), metric %in% c(1,3)) %>%
    transmute(location_id=location, sex_id=sex, age_id=age, metric_id=metric,
              year, val, lower, upper)
}) %>%
  left_join(df_lmic %>% select(location_id,location_name), by="location_id") %>%
  mutate(sex_name=unname(sex_lookup[as.character(sex_id)]))

df_daly_all <- df_daly_raw %>%
  filter(metric_id==1, age_id==22) %>%
  select(location_id,location_name,sex_id,sex_name,year,DALY=val,lower,upper)

df_daly_age <- df_daly_raw %>%
  filter(metric_id==1, age_id %in% age_ids) %>%
  left_join(age_lookup, by="age_id") %>%
  select(location_id,location_name,sex_id,sex_name,age_name,age_mid,age_order,year,
         DALY_age=val,lower_age=lower,upper_age=upper)

df_daly_rate <- df_daly_raw %>%
  filter(metric_id==3, age_id==27) %>%
  select(location_id,sex_id,sex_name,year,rate=val,rate_lo=lower,rate_hi=upper)

df_hale_all <- read_csv(hale_file,show_col_types=FALSE) %>%
  filter(metric_name=="Years",year==2023,age_name=="All ages") %>%
  select(location_id,sex_id,HALE=val)

# countries usable (have GDP + sex-specific HALE)
hale_ids <- df_hale_all %>% filter(sex_id %in% c(1,2), !is.na(HALE), HALE>0) %>%
  distinct(location_id) %>% pull(location_id)
usable_ids <- intersect(lmic_ids, hale_ids)

df_world <- st_read(world_file,quiet=TRUE)
df_world$location_id <- as.numeric(df_world$location_id)
cat("   LMIC total:", n_lmic_total, "| with GDP:", n_with_gdp,
    "| with GDP+HALE (analysed):", length(usable_ids), "\n")
cat("   Data loaded.\n\n")

# ======================================================================
# 2. CONSISTENT VLW COMPUTATION
# ======================================================================
# VSLY per (country, sex) — single definition, base = discounted annuity.
mk_vsly <- function(ie, r) {
  df_econ %>% select(location_id, GDP_pc) %>%
    inner_join(df_hale_all %>% filter(sex_id %in% c(1,2)), by="location_id") %>%
    filter(!is.na(GDP_pc), !is.na(HALE), HALE > 0) %>%
    mutate(VSL_i  = VSL_USA * (GDP_pc / GDP_pc_USA)^ie,
           VSLY   = VSL_i / annuity(HALE, r),   # base case (discounted, T_i = HALE_i)
           VSLY_u = VSL_i / HALE)               # undiscounted sensitivity (r = 0)
}

# All-ages country×sex, with Both derived as Male+Female (reviewer M6)
compute_allages <- function(ie, r = discount_rate) {
  vsly <- mk_vsly(ie, r) %>% select(location_id, sex_id, VSLY, VSLY_u, HALE)
  base <- df_daly_all %>% filter(sex_id %in% c(1,2)) %>%
    inner_join(df_econ, by=c("location_id","location_name")) %>%
    inner_join(vsly, by=c("location_id","sex_id")) %>%
    mutate(VLW=VSLY*DALY/1e9, VLW_lo=VSLY*lower/1e9, VLW_hi=VSLY*upper/1e9,
           VLW_u=VSLY_u*DALY/1e9, VLW_u_lo=VSLY_u*lower/1e9, VLW_u_hi=VSLY_u*upper/1e9)
  both <- base %>%
    group_by(location_id,location_name,LMIC_group,GDP_pc,GDP_tot,year) %>%
    summarise(DALY=sum(DALY),lower=NA_real_,upper=NA_real_,
              VLW=sum(VLW),VLW_lo=NA_real_,VLW_hi=NA_real_,
              VLW_u=sum(VLW_u),VLW_u_lo=NA_real_,VLW_u_hi=NA_real_,
              HALE=mean(HALE), .groups="drop") %>%
    mutate(sex_id=3L, sex_name="Both")
  bind_rows(
    base %>% select(location_id,location_name,LMIC_group,sex_id,sex_name,year,
                    GDP_pc,GDP_tot,DALY,lower,upper,VLW,VLW_lo,VLW_hi,
                    VLW_u,VLW_u_lo,VLW_u_hi,HALE),
    both) %>%
    mutate(pct=VLW*1e9/GDP_tot*100, pct_lo=VLW_lo*1e9/GDP_tot*100, pct_hi=VLW_hi*1e9/GDP_tot*100)
}

# Age-specific 2023 — SAME uniform VSLY, Both = Male+Female (reviewer M1/M6)
compute_age <- function(ie, r = discount_rate) {
  vsly <- mk_vsly(ie, r) %>% select(location_id, sex_id, VSLY)
  base <- df_daly_age %>% filter(year==2023, sex_id %in% c(1,2)) %>%
    inner_join(df_econ %>% select(location_id,LMIC_group), by="location_id") %>%
    inner_join(vsly, by=c("location_id","sex_id")) %>%
    mutate(VLW_a=VSLY*DALY_age/1e9, VLW_a_lo=VSLY*lower_age/1e9, VLW_a_hi=VSLY*upper_age/1e9)
  both <- base %>%
    group_by(location_id,LMIC_group,age_name,age_mid,age_order,year) %>%
    summarise(DALY_age=sum(DALY_age),lower_age=NA_real_,upper_age=NA_real_,
              VLW_a=sum(VLW_a),VLW_a_lo=NA_real_,VLW_a_hi=NA_real_,.groups="drop") %>%
    mutate(sex_id=3L, sex_name="Both")
  bind_rows(
    base %>% select(location_id,LMIC_group,sex_id,sex_name,age_name,age_mid,age_order,
                    year,DALY_age,lower_age,upper_age,VLW_a,VLW_a_lo,VLW_a_hi),
    both)
}

# ── Forecast helpers: one canonical R3 workflow ───────────────────────────────
fh <- 2050 - 2023
forecast_level <- 95
simulation_n <- 50000L

# Reviewer #3 Major 1: the candidate set is the full set of evaluated methods. All four
#   models -- ETS, ARIMA, Naive and Drift -- are fitted, diagnosed and selected by one rule
#   applied uniformly. No method is excluded a posteriori on the basis of its observed
#   performance. MODEL_SET is therefore the single place where the candidate set is declared.
MODEL_SET <- c("ETS","ARIMA","Naive","Drift")

# Reviewer #3 Minor 2: ARIMA coefficients and AICc are now extracted. Previously AICc was
#   hard-coded to NA for ARIMA and fit$coef was never read, so the fitted-model supplementary
#   table did not contain the parameter estimates and information criteria its caption claimed.
fit_series <- function(series, model=MODEL_SET, h=fh, label="") {
  model <- match.arg(model, MODEL_SET)
  y <- ts(series, start=1990, frequency=1)
  fit <- switch(model,
    "ETS"   = ets(y, model="AAN", damped=TRUE),
    "ARIMA" = auto.arima(y, seasonal=FALSE, stepwise=FALSE, approximation=FALSE),
    "Naive" = rwf(y, h=h, drift=FALSE, level=forecast_level),
    "Drift" = rwf(y, h=h, drift=TRUE,  level=forecast_level))
  # R3 Comment 2: level=95 is explicit; column 1 is therefore the 95% simulation interval.
  # Major 1: that nominal level is a construction parameter, not a validated coverage claim.
  fc <- if (model %in% c("Naive","Drift")) fit else forecast(fit, h=h, level=forecast_level)
  # Estimated parameters entering the Ljung-Box degrees-of-freedom correction:
  #   ETS   - all smoothing and damping parameters returned by ets()
  #   ARIMA - the non-seasonal AR and MA orders (d costs no degree of freedom)
  #   Naive - none; Drift - the single drift term
  fitdf <- switch(model,
    "ETS"   = length(fit$par),
    "ARIMA" = sum(arimaorder(fit)[c("p","q","P","Q")], na.rm=TRUE),
    "Naive" = 0L,
    "Drift" = 1L)
  lag_use <- max(fitdf+3L, min(10L, floor(length(series)/5)))
  lag_use <- min(lag_use, length(series)-1L)
  res <- as.numeric(residuals(fit))
  lb <- Box.test(res[!is.na(res)], lag=lag_use, type="Ljung-Box", fitdf=fitdf)
  p <- if (model=="ETS") fit$par else numeric()
  gv <- function(nm) if (nm %in% names(p)) unname(p[nm]) else NA_real_
  method <- switch(model,
    "ETS"   = fit$method,
    "ARIMA" = paste0("ARIMA(",paste(arimaorder(fit)[c("p","d","q")],collapse=","),")"),
    "Naive" = "Random walk (naive)",
    "Drift" = "Random walk with drift")
  # Information criteria and in-sample RMSE. rwf() objects carry no likelihood, so the criteria
  # are undefined for the two benchmarks and are reported as NA rather than as a spurious value.
  has_lik <- model %in% c("ETS","ARIMA")
  sig2 <- switch(model,
    "ETS" = fit$sigma2, "ARIMA" = fit$sigma2,
    stats::var(res[!is.na(res)]))
  diag <- tibble(series=label, model=model, method=method,
    alpha=gv("alpha"), beta=gv("beta"), phi=gv("phi"),
    sigma2=sig2,
    AIC  = if (has_lik) AIC(fit)     else NA_real_,
    AICc = if (has_lik) fit$aicc     else NA_real_,   # Minor 2: was NA for ARIMA
    BIC  = if (has_lik) BIC(fit)     else NA_real_,
    RMSE=unname(accuracy(fit)[1,"RMSE"]), Ljung_Box_lag=lag_use,
    Ljung_Box_fitdf=fitdf, Ljung_Box_p=as.numeric(lb$p.value),
    # Minor 3: the Ljung-Box test can only fail to reject the null of no residual
    #   autocorrelation. The column records the test outcome, never "adequacy" as an
    #   established property. Wording downstream must follow the same convention.
    residual_autocorrelation_detected=ifelse(lb$p.value<0.05,"Yes","No"),
    residual_autocorrelation_signal=ifelse(lb$p.value<0.05,"Yes","No"))
  list(fit=fit, fc=fc, diag=diag, residuals=res,
       coefficients=arima_coef_table(fit, model, label))
}

# Minor 2: ARIMA coefficient estimates with standard errors, as a long table.
arima_coef_table <- function(fit, model, label="") {
  if (model != "ARIMA" || length(coef(fit)) == 0L)
    return(tibble(series=character(), model=character(), term=character(),
                  estimate=numeric(), std_error=numeric(), z=numeric()))
  est <- coef(fit)
  se  <- tryCatch(sqrt(diag(fit$var.coef)), error=function(e) rep(NA_real_, length(est)))
  tibble(series=label, model=model, term=names(est),
         estimate=unname(est), std_error=unname(se), z=unname(est)/unname(se))
}

calc_errors <- function(actual,pred) {
  e <- actual-pred
  tibble(MAE=mean(abs(e)),RMSE=sqrt(mean(e^2)),MAPE=mean(abs(e/actual))*100)
}

validate_series <- function(series,label,outcome) {
  train <- series[1:28] # 1990-2017
  test <- series[29:34] # 2018-2023
  map_dfr(c("ETS","ARIMA"),function(model) {
    z <- fit_series(train,model=model,h=length(test),label=label)
    calc_errors(test,as.numeric(z$fc$mean)) %>%
      mutate(series=label,outcome=outcome,model=model,holdout="2018-2023",
             scenario_nominal_level=forecast_level,.before=1)
  })
}

# Reviewer #3 Major 3: this is eigenvalue clipping followed by rescaling to unit diagonal,
#   NOT Higham's nearest-correlation-matrix algorithm. The previous name implied the latter.
#   Procedure, in order: (1) missing entries set to 0; (2) unit diagonal imposed;
#   (3) symmetrisation as (X + X')/2; (4) eigenvalues floored at EIGEN_FLOOR; (5) cov2cor
#   rescaling. The result is positive definite by construction. It is the nearest correlation
#   matrix only in the spectral sense, and is not claimed to be a Frobenius-norm projection.
EIGEN_FLOOR <- 1e-8
pd_correlation_eigen_clip <- function(x) {
  nm <- dimnames(x)
  x[is.na(x)] <- 0
  diag(x) <- 1
  e <- eigen((x+t(x))/2,symmetric=TRUE)
  y <- e$vectors %*% diag(pmax(e$values,EIGEN_FLOOR)) %*% t(e$vectors)
  out <- cov2cor(y)
  dimnames(out) <- nm      # eigen()/cov2cor() drop names; the groups must stay identifiable
  out
}

# ---------------------------------------------------------------------------------------------
# Joint income-group simulation.
#
# Reviewer #3 Major 3 - the method is stated in full here, and the manuscript Methods repeats
# it. Every element the reviewer asked for is named explicitly:
#
#  * Correlation matrix. Pearson correlation of the in-sample residuals of the three income-group
#    DALY models, computed once on the full 1990-2023 fit with use="pairwise.complete.obs". It is
#    a CROSS-SECTIONAL correlation between groups at a common time index. It carries no temporal
#    structure and is held fixed across horizons.
#  * Positive-definite adjustment. pd_correlation_eigen_clip(): eigenvalue clipping at
#    EIGEN_FLOOR followed by cov2cor rescaling. See that function for the exact ordering.
#  * Parameter treatment. Model parameters are fixed at their point estimates. Marginal standard
#    deviations are backed out of the marginal forecast interval as (upper-lower)/(2*z_0.975), so
#    the simulation propagates forecast-error variance ONLY. Parameter-estimation uncertainty and
#    model uncertainty are NOT propagated.
#  * Seeds. set.seed(seed + k) at horizon k, with seed = 20260724 for ETS and 20260725 for ARIMA.
#    Every horizon therefore draws from an independent stream.
#  * Non-negativity. No truncation is applied. The routine instead COUNTS draws falling at or
#    below zero and returns that count, so the assumption can be checked rather than assumed.
#  * Interpretation. Because each horizon is simulated independently, the draws are POINTWISE at
#    each year. They are not coherent temporal forecast paths, and the resulting ranges must not
#    be read as the probability of any trajectory over time.
#
# Reviewer #3 Major 1 - the output columns are named as scenario bounds, not as prediction
# intervals. Empirical coverage of the nominal 95% construction was 55.1% overall and 47.2% at a
# six-year horizon, so no calibrated probability statement is available and none is made. The
# nominal level is retained only as a construction parameter, under the name
# scenario_nominal_level, and is reported alongside the ranges it generated.
# ---------------------------------------------------------------------------------------------
joint_income_projection <- function(history,vsly_2023,model=MODEL_SET,
                                    seed=20260724L) {
  model <- match.arg(model, MODEL_SET)
  groups <- income_fct
  fits <- setNames(lapply(groups,function(g) {
    x <- history %>% filter(LMIC_group==g) %>% arrange(year) %>% pull(D)
    fit_series(x,model=model,label=paste("DALY",g,sep="|"))
  }),groups)
  resid_mat <- do.call(cbind,lapply(fits,`[[`,"residuals"))
  corr_raw <- cor(resid_mat, use="pairwise.complete.obs")
  corr <- pd_correlation_eigen_clip(corr_raw)
  years <- 2024:2050
  all_rows <- vector("list",length(years))
  group_rows <- vector("list",length(years))
  neg_rows <- vector("list",length(years))
  for (k in seq_along(years)) {
    means <- sapply(fits,function(z) as.numeric(z$fc$mean[k]))
    lowers <- sapply(fits,function(z) as.numeric(z$fc$lower[k,1]))
    uppers <- sapply(fits,function(z) as.numeric(z$fc$upper[k,1]))
    ses <- pmax((uppers-lowers)/(2*qnorm(0.975)),.Machine$double.eps)
    cov_mat <- diag(ses) %*% corr %*% diag(ses)
    set.seed(seed+k)
    z <- matrix(rnorm(simulation_n*length(groups)),ncol=length(groups))
    draws <- sweep(z %*% chol(cov_mat),2,means,"+")
    v <- vsly_2023$VSLY_effective[match(groups,vsly_2023$LMIC_group)]/1e9
    total_d <- rowSums(draws)
    total_v <- rowSums(sweep(draws,2,v,"*"))
    # Major 3: non-negativity is measured, not imposed.
    neg_rows[[k]] <- tibble(model=model,Year=years[k],
      draws=simulation_n,
      n_group_draws_nonpositive=sum(draws<=0),
      pct_group_draws_nonpositive=sum(draws<=0)/length(draws)*100,
      n_total_draws_nonpositive=sum(total_d<=0),
      pct_total_draws_nonpositive=sum(total_d<=0)/length(total_d)*100)
    all_rows[[k]] <- tibble(model=model,Year=years[k],Group="All LMICs",
      DALY=sum(means),DALY_scenario_low=quantile(total_d,0.025),
      DALY_scenario_high=quantile(total_d,0.975),
      VLW_billion=sum(means*v),VLW_scenario_low_billion=quantile(total_v,0.025),
      VLW_scenario_high_billion=quantile(total_v,0.975),
      scenario_nominal_level=forecast_level,simulation_draws=simulation_n,
      interval_interpretation="Model-dependent scenario range; nominal level is a construction parameter and is not a validated coverage claim",
      method="Pointwise joint normal simulation across income groups at each horizon, using the marginal forecast dispersion and the fixed cross-sectional residual correlation; horizons are simulated independently and the draws are not temporal paths")
    group_rows[[k]] <- tibble(model=model,Year=years[k],Group=groups,
      DALY=means,DALY_scenario_low=lowers,DALY_scenario_high=uppers,
      VLW_billion=means*v,VLW_scenario_low_billion=lowers*v,
      VLW_scenario_high_billion=uppers*v,scenario_nominal_level=forecast_level,
      simulation_draws=NA_integer_,
      interval_interpretation="Model-dependent scenario range; nominal level is a construction parameter and is not a validated coverage claim",
      method="Group DALY forecast monetized using the fixed 2023 group effective VSLY")
  }
  list(all=bind_rows(all_rows),groups=bind_rows(group_rows),
       diagnostics=bind_rows(lapply(fits,`[[`,"diag")),
       coefficients=bind_rows(lapply(fits,`[[`,"coefficients")),
       residual_correlation=corr, residual_correlation_raw=corr_raw,
       eigenvalues_raw=eigen((corr_raw+t(corr_raw))/2,symmetric=TRUE)$values,
       nonnegativity=bind_rows(neg_rows),
       seed=seed, draws=simulation_n,
       fits=fits)
}

