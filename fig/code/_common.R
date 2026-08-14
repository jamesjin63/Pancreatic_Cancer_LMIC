# ==============================================================================
# _common.R - shared preamble for every figure script
#
# Contents = scripts/run_LMIC_Pancreatic_VLW_v2.R lines 1-352, extracted verbatim:
#   paths, reference constants, palettes/themes, helper functions, data loading (section 1),
#   VLW computation (section 2), and the forecast helper functions.
# Nothing is modified except the directory resolution below.
#
# Sourced by the figure scripts; not run on its own.
# ==============================================================================

if (!exists("FIG_CODE_DIR")) stop("Call this from a figure script in fig/code/; do not run _common.R directly.")
CODE_DIR <- normalizePath(file.path(FIG_CODE_DIR, "..", ".."))   # fig/code -> fig -> code
FIG_DIR  <- normalizePath(file.path(FIG_CODE_DIR, ".."))          # fig/
dir.create(FIG_DIR, showWarnings=FALSE, recursive=TRUE)
setwd(CODE_DIR)   # the canonical script reads data_raw/ relative to code/

# ------------- verbatim from run_LMIC_Pancreatic_VLW_v2.R:1-352 below -------------
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
#      rolling-origin MAPE subject to Ljung-Box residual adequacy); see
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

fit_series <- function(series, model=c("ETS","ARIMA"), h=fh, label="") {
  model <- match.arg(model)
  y <- ts(series, start=1990, frequency=1)
  fit <- if (model=="ETS") {
    ets(y, model="AAN", damped=TRUE)
  } else {
    auto.arima(y, seasonal=FALSE, stepwise=FALSE, approximation=FALSE)
  }
  # R3 Comment 2: level=95 is explicit; column 1 is therefore the 95% interval.
  fc <- forecast(fit, h=h, level=forecast_level)
  fitdf <- if (model=="ETS") length(fit$par) else
    sum(arimaorder(fit)[c("p","q","P","Q")], na.rm=TRUE)
  lag_use <- max(fitdf+3L, min(10L, floor(length(series)/5)))
  lag_use <- min(lag_use, length(series)-1L)
  lb <- Box.test(residuals(fit), lag=lag_use, type="Ljung-Box", fitdf=fitdf)
  p <- if (model=="ETS") fit$par else numeric()
  gv <- function(nm) if (nm %in% names(p)) unname(p[nm]) else NA_real_
  method <- if (model=="ETS") fit$method else
    paste0("ARIMA(",paste(arimaorder(fit)[c("p","d","q")],collapse=","),")")
  diag <- tibble(series=label, model=model, method=method,
    alpha=gv("alpha"), beta=gv("beta"), phi=gv("phi"),
    sigma2=ifelse(model=="ETS",fit$sigma2,fit$sigma2),
    AIC=AIC(fit), AICc=ifelse(model=="ETS",fit$aicc,NA_real_), BIC=BIC(fit),
    RMSE=unname(accuracy(fit)[1,"RMSE"]), Ljung_Box_lag=lag_use,
    Ljung_Box_fitdf=fitdf, Ljung_Box_p=as.numeric(lb$p.value),
    residual_autocorrelation_signal=ifelse(lb$p.value<0.05,"Yes","No"))
  list(fit=fit, fc=fc, diag=diag, residuals=as.numeric(residuals(fit)))
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
             interval_level=forecast_level,.before=1)
  })
}

nearest_correlation <- function(x) {
  x[is.na(x)] <- 0
  diag(x) <- 1
  e <- eigen((x+t(x))/2,symmetric=TRUE)
  y <- e$vectors %*% diag(pmax(e$values,1e-8)) %*% t(e$vectors)
  cov2cor(y)
}

joint_income_projection <- function(history,vsly_2023,model=c("ETS","ARIMA"),
                                    seed=20260724L) {
  model <- match.arg(model)
  groups <- income_fct
  fits <- setNames(lapply(groups,function(g) {
    x <- history %>% filter(LMIC_group==g) %>% arrange(year) %>% pull(D)
    fit_series(x,model=model,label=paste("DALY",g,sep="|"))
  }),groups)
  corr <- nearest_correlation(cor(do.call(cbind,lapply(fits,`[[`,"residuals")),
                                  use="pairwise.complete.obs"))
  years <- 2024:2050
  all_rows <- vector("list",length(years))
  group_rows <- vector("list",length(years))
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
    all_rows[[k]] <- tibble(model=model,Year=years[k],Group="All LMICs",
      DALY=sum(means),DALY_lower_95_PI=quantile(total_d,0.025),
      DALY_upper_95_PI=quantile(total_d,0.975),
      VLW_billion=sum(means*v),VLW_lower_95_PI_billion=quantile(total_v,0.025),
      VLW_upper_95_PI_billion=quantile(total_v,0.975),
      interval_level=forecast_level,simulation_draws=simulation_n,
      method="Joint normal simulation using explicit 95% marginal forecast intervals and empirical residual correlation")
    group_rows[[k]] <- tibble(model=model,Year=years[k],Group=groups,
      DALY=means,DALY_lower_95_PI=lowers,DALY_upper_95_PI=uppers,
      VLW_billion=means*v,VLW_lower_95_PI_billion=lowers*v,
      VLW_upper_95_PI_billion=uppers*v,interval_level=forecast_level,
      simulation_draws=NA_integer_,method="Group DALY forecast monetized using the fixed 2023 group effective VSLY")
  }
  list(all=bind_rows(all_rows),groups=bind_rows(group_rows),
       diagnostics=bind_rows(lapply(fits,`[[`,"diag")),residual_correlation=corr,
       fits=fits)
}

# ---------------------------- end of shared preamble ----------------------------
