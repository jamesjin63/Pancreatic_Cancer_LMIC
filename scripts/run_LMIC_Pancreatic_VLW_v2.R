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
#   5. ETS is the primary model and non-seasonal ARIMA is a sensitivity model.
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
norm_name <- function(x) x %>% str_replace_all("[‘’]","'") %>%
  str_replace_all(" "," ") %>% str_squish() %>% str_to_lower()
fmt_ui <- function(v,lo,hi,d=1,s=1) paste0(
  formatC(v/s,format="f",digits=d,big.mark=",")," (",
  formatC(lo/s,format="f",digits=d,big.mark=","),"–",
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

# ======================================================================
# 3. LOOP OVER IE — FULL CONSISTENT OUTPUT
# ======================================================================
recon_log <- list()
ets_diag_all <- list()

for (ie in ie_values) {
  ie_tag <- paste0("IE", gsub("\\.","",as.character(ie)))
  ie_lab <- format(ie, nsmall=1)
  ie_dir <- file.path(out_root, ie_tag)
  dir.create(ie_dir, showWarnings=FALSE, recursive=TRUE)
  cat("──────── IE =", ie_lab, "────────\n")

  df    <- compute_allages(ie)            # base case (3% discount)
  df_av <- compute_age(ie)
  d23   <- df %>% filter(year==2023, sex_name=="Both")
  d23_all <- df %>% filter(year==2023)

  # ── RECONCILIATION CHECKS (written to file) ──
  tot_all   <- sum(d23$VLW)
  tot_age   <- df_av %>% filter(sex_name=="Both") %>% summarise(s=sum(VLW_a)) %>% pull(s)
  tot_sexsum<- d23_all %>% filter(sex_name %in% c("Male","Female")) %>% summarise(s=sum(VLW)) %>% pull(s)
  recon_log[[ie_tag]] <- tibble(
    IE=ie,
    VLW_total_allages=tot_all,
    VLW_total_agesum=tot_age,
    age_vs_total_diff=tot_age-tot_all,
    VLW_both=tot_all,
    VLW_male_plus_female=tot_sexsum,
    sex_vs_both_diff=tot_sexsum-tot_all)

  # ════════ TABLES ════════
  mk_sum <- function(data, gv=NULL) {
    g <- if(!is.null(gv)) data %>% group_by(across(all_of(gv))) else data
    g %>% summarise(N=n(),
      D=sum(DALY),V=sum(VLW),Vu=sum(VLW_u),
      Wp=weighted.mean(pct,GDP_tot),mH=mean(HALE),.groups="drop")
  }

  # Table 1 — income summary (+ IE valuation-sensitivity range only on base IE)
  t1_core <- bind_rows(
    mk_sum(d23,"LMIC_group") %>% arrange(factor(LMIC_group,levels=income_fct)),
    mk_sum(d23) %>% mutate(LMIC_group="All LMICs"))
  t1 <- t1_core %>% transmute(`Income Group`=LMIC_group, Countries=N,
    `DALYs (thousands; point estimate)`=round(D/1e3,1),
    `VLW (billion constant-2023 USD; point estimate)`=round(V,2),
    `VLW undiscounted r=0 (billion constant-2023 USD)`=round(Vu,2),
    `VLW/GDP (%; point estimate)`=round(Wp,4),
    `Mean HALE`=formatC(mH,format="f",digits=1))
  write_csv(t1, file.path(ie_dir,"Table1_income_summary.csv"))
  if (ie==ie_base) write_csv(t1,file.path(submission_table_dir,
    "Main Table 1_2023 income summary.csv"))

  # Table 2 — sex × income (Male+Female sum to Both by construction)
  t2 <- d23_all %>% group_by(sex_name,LMIC_group) %>%
    summarise(N=n(),D=sum(DALY),V=sum(VLW),Vu=sum(VLW_u),.groups="drop") %>%
    transmute(Sex=sex_name,`Income Group`=LMIC_group,Countries=N,
      `DALYs (thousands; point estimate)`=round(D/1e3,1),
      `VLW (billion constant-2023 USD; point estimate)`=round(V,2),
      `VLW undiscounted (billion constant-2023 USD)`=round(Vu,2))
  write_csv(t2, file.path(ie_dir,"Table2_sex_income.csv"))
  if (ie==ie_base) write_csv(t2,file.path(submission_table_dir,
    "Main Table 2_sex-specific burden.csv"))

  # Table 3 — all countries ranked by VLW/GDP
  t3 <- d23 %>% arrange(desc(pct)) %>% mutate(Rank=row_number()) %>%
    transmute(Rank,Country=location_name,`Income Group`=LMIC_group,
      `GDP pc (PPP)`=formatC(GDP_pc,format="f",digits=0,big.mark=","),
      `DALYs (thousands; point estimate)`=round(DALY/1e3,1),
      `VLW (billion constant-2023 USD; point estimate)`=round(VLW,3),
      `VLW/GDP (%; point estimate)`=round(pct,4))
  write_csv(t3, file.path(ie_dir,"Table3_all_LMIC_countries.csv"))
  if (ie==ie_base) write_csv(t3,file.path(submission_table_dir,
    "Supplementary Table 1_country burden.csv"))

  # Table 4 — country × sex full
  t4 <- d23_all %>% transmute(Country=location_name,Sex=sex_name,`Income Group`=LMIC_group,
    `DALYs (point estimate)`=round(DALY,0),
    `VLW (billion constant-2023 USD; point estimate)`=round(VLW,3),
    `VLW undiscounted (billion constant-2023 USD)`=round(VLW_u,3),
    `VLW/GDP (%; point estimate)`=round(pct,4))
  write_csv(t4, file.path(ie_dir,"Table4_country_full.csv"))
  if (ie==ie_base) write_csv(t4,file.path(submission_table_dir,
    "Supplementary Table 2_country and sex burden.csv"))

  # Table 5 — temporal
  t5 <- df %>% filter(sex_name=="Both",year %in% c(1990,2000,2010,2023)) %>%
    group_by(LMIC_group,year) %>%
    summarise(D=sum(DALY),V=sum(VLW),.groups="drop") %>%
    transmute(`Income Group`=LMIC_group,Year=year,
      `DALYs (thousands; point estimate)`=round(D/1e3,1),
      `VLW (billion constant-2023 USD; point estimate)`=round(V,2)) %>%
    arrange(factor(`Income Group`,levels=income_fct),Year)
  write_csv(t5, file.path(ie_dir,"Table5_temporal.csv"))
  if (ie==ie_base) write_csv(t5,file.path(submission_table_dir,
    "Main Table 3_temporal trends.csv"))

  # Table 6 — age-specific (now SUMS to Table 1 total)
  age_both <- df_av %>% filter(sex_name=="Both") %>%
    group_by(age_name,age_mid,age_order) %>%
    summarise(D=sum(DALY_age),V=sum(VLW_a),.groups="drop") %>%
    arrange(age_order)
  t6 <- age_both %>% transmute(`Age Group`=age_name,
      `DALYs (point estimate)`=round(D,0),
      `VLW (billion constant-2023 USD; point estimate)`=round(V,3),
      `Share of total VLW (%)`=formatC(V/sum(V)*100,format="f",digits=1))
  write_csv(t6, file.path(ie_dir,"Table6_age_specific.csv"))
  if (ie==ie_base) write_csv(t6,file.path(submission_table_dir,
    "Main Table 4_age-specific burden.csv"))

  # ════════ INFERENTIAL TEST — Kruskal-Wallis (reviewer M4) ════════
  kdat <- d23 %>% mutate(grp=factor(LMIC_group,levels=income_fct))
  kt <- kruskal.test(pct ~ grp, data=kdat)
  n_k <- nrow(kdat); k_g <- length(unique(kdat$grp))
  eps2 <- unname(kt$statistic) / (n_k - 1)              # epsilon-squared effect size
  eta2 <- (unname(kt$statistic) - k_g + 1) / (n_k - k_g)
  ph <- suppressWarnings(pairwise.wilcox.test(kdat$pct, kdat$grp, p.adjust.method="bonferroni"))
  kw_summary <- tibble(
    test="Kruskal-Wallis (VLW/GDP across income groups)",
    H=unname(kt$statistic), df=unname(kt$parameter), p_value=kt$p.value,
    n=n_k, groups=k_g, epsilon2=eps2, eta2=eta2,
    note="Groups are income-defined; test is descriptive of between-group rank differences (potential circularity acknowledged).")
  write_csv(kw_summary, file.path(ie_dir,"Stats_KruskalWallis.csv"))
  ph_df <- as.data.frame(ph$p.value) %>% rownames_to_column("group")
  write_csv(ph_df, file.path(ie_dir,"Stats_KW_posthoc_pairwise_wilcoxon_bonferroni.csv"))

  # Descriptive country-level log-linear regression with HC3 inference (R3 minor point).
  reg <- lm(log(pct) ~ log(GDP_pc),data=d23)
  reg_vcov <- sandwich::vcovHC(reg,type="HC3")
  reg_beta <- coef(reg)[2]
  reg_se <- sqrt(diag(reg_vcov))[2]
  reg_df <- df.residual(reg)
  reg_t <- reg_beta/reg_se
  reg_ci <- reg_beta+c(-1,1)*qt(0.975,reg_df)*reg_se
  reg_out <- tibble(
    term="log GDP per capita (PPP, current international $)",estimate=reg_beta,
    HC3_standard_error=reg_se,lower_95_CI=reg_ci[1],upper_95_CI=reg_ci[2],
    t_statistic=reg_t,df=reg_df,p_value_two_sided=2*pt(abs(reg_t),reg_df,lower.tail=FALSE),
    n=nobs(reg),R_squared=summary(reg)$r.squared,adjusted_R_squared=summary(reg)$adj.r.squared,
    outcome="log country VLW/GDP (%)",weights="None (country-level unweighted OLS)",
    inference="HC3 heteroskedasticity-consistent standard errors; two-sided t test and 95% CI",
    interpretation="Descriptive association only; the outcome and income grouping contain GDP-derived quantities.")
  write_csv(reg_out,file.path(ie_dir,"Stats_income_gradient_regression_HC3.csv"))
  write_csv(kw_summary %>% mutate(
    regression_term=reg_out$term,regression_estimate=reg_out$estimate,
    regression_HC3_SE=reg_out$HC3_standard_error,
    regression_lower_95_CI=reg_out$lower_95_CI,
    regression_upper_95_CI=reg_out$upper_95_CI,
    regression_p_two_sided=reg_out$p_value_two_sided,
    regression_R2=reg_out$R_squared,
    regression_note=reg_out$interpretation),
    file.path(ie_dir,"Stats_KruskalWallis_and_regression.csv"))
  if (ie==ie_base) {
    write_csv(reg_out,file.path(r3_dir,"R3_income_gradient_regression_HC3.csv"))
    write_csv(reg_out,file.path(submission_table_dir,
      "Supplementary Table 8_income-gradient regression HC3.csv"))
  }

  # ════════ FIGURES ════════
  # Figure 1 — 2023 snapshot
  bar1 <- d23 %>% group_by(LMIC_group) %>%
    summarise(V=sum(VLW),.groups="drop") %>%
    mutate(LMIC_group=factor(LMIC_group,levels=income_fct))
  p1a <- ggplot(bar1,aes(LMIC_group,V,fill=LMIC_group))+
    geom_col(width=0.6,colour="white",linewidth=0.3)+
    scale_fill_manual(values=income_pal,guide="none")+
    scale_y_continuous(expand=expansion(mult=c(0,0.1)))+
    scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+labs(x=NULL,y="VLW (billion USD)")+theme_nm()
  p1b <- ggplot(d23,aes(factor(LMIC_group,income_fct),pct,fill=LMIC_group))+
    geom_boxplot(width=0.5,outlier.shape=NA,alpha=0.5,linewidth=0.3)+
    geom_point(aes(colour=LMIC_group),size=0.6,alpha=0.5,
      position=position_jitter(width=0.12,height=0,seed=20260724L))+
    scale_fill_manual(values=income_pal,guide="none")+scale_colour_manual(values=income_pal,guide="none")+
    scale_y_continuous(labels=\(x) paste0(x,"%"))+scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+
    labs(x=NULL,y="VLW / GDP (%)")+theme_nm()
  sex_bar <- d23_all %>% filter(sex_name %in% c("Male","Female")) %>%
    group_by(sex_name,LMIC_group) %>% summarise(V=sum(VLW),.groups="drop") %>%
    mutate(LMIC_group=factor(LMIC_group,levels=income_fct))
  p1c <- ggplot(sex_bar,aes(LMIC_group,V,fill=sex_name))+
    geom_col(position=position_dodge(0.65),width=0.55,colour="white",linewidth=0.2)+
    scale_fill_manual(values=sex_pal,name="Sex")+scale_y_continuous(expand=expansion(mult=c(0,0.1)))+
    scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+labs(x=NULL,y="VLW (billion USD)")+
    theme_nm()+theme(legend.position=c(0.15,0.85))
  p1d <- ggplot(d23,aes(GDP_pc,pct,colour=LMIC_group,size=DALY))+
    geom_point(alpha=0.6,stroke=0.2)+
    scale_x_log10(labels=dollar_format(),breaks=c(1000,3000,10000,30000))+
    scale_y_continuous(labels=\(x) paste0(x,"%"))+
    scale_colour_manual(values=income_pal,name="Income Group")+
    scale_size_continuous(name="DALYs",range=c(1.5,9),
      breaks=c(5000,20000,100000,500000),labels=c("5K","20K","100K","500K"))+
    labs(x="GDP per capita (PPP, USD)",y="VLW / GDP (%)")+theme_nm()+
    guides(colour=guide_legend(order=1,override.aes=list(size=2.5)),size=guide_legend(order=2))
  pdf(file.path(ie_dir,"Figure1_2023_snapshot.pdf"),width=8.5,height=7)
  print((p1a|p1b)/(p1c|p1d)+plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))); dev.off()

  # Figure 2 — temporal
  tot_t <- df %>% filter(sex_name=="Both") %>% group_by(year) %>%
    summarise(V=sum(VLW),.groups="drop")
  p2a <- ggplot(tot_t,aes(year,V))+
    geom_line(colour=pal$blue,linewidth=0.7)+scale_x_continuous(breaks=seq(1990,2023,5))+
    scale_y_continuous(expand=expansion(mult=c(0,0.05)))+labs(x="Year",y="Total VLW (billion USD)")+theme_nm()
  inc_t <- df %>% filter(sex_name=="Both") %>% group_by(LMIC_group,year) %>%
    summarise(V=sum(VLW),.groups="drop") %>% mutate(LMIC_group=factor(LMIC_group,levels=income_fct))
  p2b <- ggplot(inc_t,aes(year,V,colour=LMIC_group))+geom_line(linewidth=0.6)+
    geom_point(data=inc_t %>% filter(year %in% c(1990,2000,2010,2023)),size=1.2)+
    scale_colour_manual(values=income_pal,name="Income Group")+scale_x_continuous(breaks=seq(1990,2023,5))+
    labs(x="Year",y="VLW (billion USD)")+theme_nm()+theme(legend.position=c(0.28,0.78))
  daly_t <- df %>% filter(sex_name=="Both") %>% group_by(LMIC_group,year) %>%
    summarise(D=sum(DALY)/1e6,.groups="drop") %>% mutate(LMIC_group=factor(LMIC_group,levels=income_fct))
  p2c <- ggplot(daly_t,aes(year,D,colour=LMIC_group))+geom_line(linewidth=0.6)+
    scale_colour_manual(values=income_pal,name="Income Group")+scale_x_continuous(breaks=seq(1990,2023,5))+
    labs(x="Year",y="DALYs (millions)")+theme_nm()+theme(legend.position=c(0.28,0.78))
  sex_t <- df %>% filter(sex_name %in% c("Male","Female")) %>% group_by(sex_name,year) %>%
    summarise(V=sum(VLW),.groups="drop")
  p2d <- ggplot(sex_t,aes(year,V,colour=sex_name))+geom_line(linewidth=0.6)+
    scale_colour_manual(values=sex_pal,name="Sex")+scale_x_continuous(breaks=seq(1990,2023,5))+
    labs(x="Year",y="VLW (billion USD)")+theme_nm()+theme(legend.position=c(0.2,0.82))
  pdf(file.path(ie_dir,"Figure2_trends.pdf"),width=9,height=7)
  print((p2a|p2b)/(p2c|p2d)+plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))); dev.off()

  # Figure 3 — maps
  md <- d23 %>% select(location_id,VLW,pct,LMIC_group)
  w  <- df_world %>% left_join(md,by="location_id")
  p3a <- ggplot(w)+geom_sf(aes(fill=VLW),colour="grey40",linewidth=0.06)+
    scale_fill_gradientn(colours=map_blue,na.value="grey92",trans="log1p",
      breaks=c(0,0.1,1,10,100),labels=c("0","0.1","1","10","100"),name="VLW\n(billion)",
      guide=guide_colourbar(barheight=grid::unit(34,"mm"),title.position="top"))+theme_nm_map()
  p3b <- ggplot(w)+geom_sf(aes(fill=pct),colour="grey40",linewidth=0.06)+
    scale_fill_gradientn(colours=map_red,na.value="grey92",trans="log1p",
      breaks=c(0,0.1,0.5,1),labels=c("0","0.1","0.5","1"),name="VLW/GDP\n(%)",
      guide=guide_colourbar(barheight=grid::unit(34,"mm"),title.position="top"))+theme_nm_map()
  w2 <- df_world %>% left_join(d23 %>% select(location_id,LMIC_group),by="location_id")
  p3c <- ggplot(w2)+geom_sf(aes(fill=LMIC_group),colour="grey40",linewidth=0.06)+
    scale_fill_manual(values=income_pal,na.value="grey92",name="Income Group")+theme_nm_map()
  pdf(file.path(ie_dir,"Figure3_maps.pdf"),width=8,height=11)
  print(p3a/p3b/p3c+plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))); dev.off()

  # Figure 4 — age-specific (VLW now proportional to DALYs)
  age_all <- df_av %>% filter(sex_name=="Both") %>% group_by(age_name,age_mid,age_order) %>%
    summarise(V=sum(VLW_a),.groups="drop") %>% arrange(age_order)
  p4a <- ggplot(age_all,aes(age_mid,V))+
    geom_line(colour=pal$teal,linewidth=0.7)+geom_point(colour=pal$teal,size=1.2)+
    scale_x_continuous(breaks=seq(0,100,10))+labs(x="Age (years)",y="VLW (billion USD)")+theme_nm()
  age_daly <- df_av %>% filter(sex_name=="Both") %>% group_by(age_name,age_mid,age_order) %>%
    summarise(D=sum(DALY_age)/1e3,.groups="drop") %>% arrange(age_order)
  p4b <- ggplot(age_daly,aes(age_mid,D))+
    geom_line(colour=pal$purple,linewidth=0.7)+geom_point(colour=pal$purple,size=1.2)+
    scale_x_continuous(breaks=seq(0,100,10))+labs(x="Age (years)",y="DALYs (thousands)")+theme_nm()
  age_sex <- df_av %>% filter(sex_name %in% c("Male","Female")) %>%
    group_by(sex_name,age_name,age_mid,age_order) %>% summarise(V=sum(VLW_a),.groups="drop") %>% arrange(age_order)
  p4c <- ggplot(age_sex,aes(age_mid,V,colour=sex_name))+geom_line(linewidth=0.6)+geom_point(size=1)+
    scale_colour_manual(values=sex_pal,name="Sex")+scale_x_continuous(breaks=seq(0,100,10))+
    labs(x="Age (years)",y="VLW (billion USD)")+theme_nm()+theme(legend.position=c(0.15,0.85))
  age_inc <- df_av %>% filter(sex_name=="Both") %>% group_by(LMIC_group,age_name,age_mid,age_order) %>%
    summarise(V=sum(VLW_a),.groups="drop") %>% arrange(age_order) %>%
    mutate(LMIC_group=factor(LMIC_group,levels=income_fct))
  p4d <- ggplot(age_inc,aes(age_mid,V,colour=LMIC_group))+geom_line(linewidth=0.6)+geom_point(size=1)+
    scale_colour_manual(values=income_pal,name="Income Group")+scale_x_continuous(breaks=seq(0,100,10))+
    labs(x="Age (years)",y="VLW (billion USD)")+theme_nm()+theme(legend.position=c(0.2,0.78))
  pdf(file.path(ie_dir,"Figure4_age_specific.pdf"),width=9,height=7)
  print((p4a|p4b)/(p4c|p4d)+plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))); dev.off()

  # ════════ FORECAST to 2050: DALY is the primary estimand (R3 Comments 2-5) ════════
  fc_inc_raw <- df %>% filter(sex_name=="Both") %>% group_by(LMIC_group,year) %>%
    summarise(V=sum(VLW),D=sum(DALY),.groups="drop")
  vsly_group_2023 <- fc_inc_raw %>% filter(year==2023) %>%
    transmute(LMIC_group,VSLY_effective=V*1e9/D)
  primary_ets <- joint_income_projection(fc_inc_raw,vsly_group_2023,"ETS",seed=20260724L)
  primary_arima <- joint_income_projection(fc_inc_raw,vsly_group_2023,"ARIMA",seed=20260725L)

  obs_all <- fc_inc_raw %>% group_by(year) %>% summarise(V=sum(V),D=sum(D),.groups="drop") %>%
    mutate(Vl=NA_real_,Vh=NA_real_,Dl=NA_real_,Dh=NA_real_,type="Observed")
  pred_all <- primary_ets$all %>% transmute(year=Year,V=VLW_billion,
    Vl=VLW_lower_95_PI_billion,Vh=VLW_upper_95_PI_billion,D=DALY,
    Dl=DALY_lower_95_PI,Dh=DALY_upper_95_PI,type="Forecast")
  fc_all <- bind_rows(obs_all,pred_all)

  obs_inc <- fc_inc_raw %>% transmute(LMIC_group,year,V,D,Vl=NA_real_,Vh=NA_real_,
                                      Dl=NA_real_,Dh=NA_real_,type="Observed")
  pred_inc <- primary_ets$groups %>% transmute(LMIC_group=Group,year=Year,
    V=VLW_billion,Vl=VLW_lower_95_PI_billion,Vh=VLW_upper_95_PI_billion,
    D=DALY,Dl=DALY_lower_95_PI,Dh=DALY_upper_95_PI,type="Forecast")
  fc_inc <- bind_rows(obs_inc,pred_inc)

  # Sex-specific direct VLW forecasts are retained as secondary descriptive series.
  fc_sex_raw <- df %>% filter(sex_name %in% c("Male","Female")) %>%
    group_by(sex_name,year) %>% summarise(V=sum(VLW),.groups="drop")
  sex_fits <- setNames(lapply(c("Male","Female"),function(sx) {
    x <- fc_sex_raw %>% filter(sex_name==sx) %>% arrange(year) %>% pull(V)
    fit_series(x,"ETS",label=paste(ie_tag,"VLW",sx,sep="|"))
  }),c("Male","Female"))
  fc_sex <- bind_rows(lapply(names(sex_fits),function(sx) {
    s <- fc_sex_raw %>% filter(sex_name==sx)
    z <- sex_fits[[sx]]$fc
    bind_rows(s %>% mutate(Vl=NA_real_,Vh=NA_real_,type="Observed"),
      tibble(sex_name=sx,year=2024:2050,V=as.numeric(z$mean),
             Vl=as.numeric(z$lower[,1]),Vh=as.numeric(z$upper[,1]),type="Forecast"))
  }))

  diags <- bind_rows(primary_ets$diagnostics,primary_arima$diagnostics,
                     bind_rows(lapply(sex_fits,`[[`,"diag"))) %>% mutate(IE=ie,.before=1)
  ets_diag_all[[ie_tag]] <- diags
  write_csv(diags,file.path(ie_dir,"Stats_forecast_diagnostics.csv"))
  write_csv(diags %>% filter(model=="ETS"),file.path(ie_dir,"Stats_ETS_diagnostics.csv"))

  # Forecast tables: all-LMIC PIs come from joint simulation, never summed endpoints.
  fc_yrs <- c(2025,2030,2035,2040,2045,2050)
  t7 <- bind_rows(primary_ets$all,primary_ets$groups,primary_arima$all,primary_arima$groups) %>%
    filter(Year %in% fc_yrs) %>% arrange(Year,model,Group)
  write_csv(t7,file.path(ie_dir,"Table7_forecast_summary.csv"))
  write_csv(bind_rows(primary_ets$all,primary_arima$all),file.path(ie_dir,"Table8_forecast_annual.csv"))

  if (ie==ie_base) {
    main_t5 <- bind_rows(
      t7 %>% filter(Group=="All LMICs") %>%
        arrange(Year,factor(model,levels=c("ETS","ARIMA"))),
      t7 %>% filter(Year==2050,Group!="All LMICs") %>%
        arrange(factor(model,levels=c("ETS","ARIMA")),
                factor(Group,levels=income_fct))
    ) %>% transmute(
      Model=model,Year,Group,
      `VLW (billion constant-2023 USD; 95% PI)`=
        fmt_ui(VLW_billion,VLW_lower_95_PI_billion,
               VLW_upper_95_PI_billion,d=2),
      `DALYs (thousands; 95% PI)`=
        fmt_ui(DALY,DALY_lower_95_PI,DALY_upper_95_PI,d=1,s=1000)
    )
    write_csv(main_t5,file.path(submission_table_dir,
      "Main Table 5_selected projections.csv"))
    write_csv(bind_rows(primary_ets$all,primary_arima$all),
      file.path(submission_table_dir,
        "Supplementary Table 3_annual ETS and ARIMA projections.csv"))

    # Holdout validation retains the eight previously reported series so the
    # ETS-versus-ARIMA comparison is completely transparent.
    validation <- bind_rows(
      map_dfr(income_fct,function(g) {
        s <- fc_inc_raw %>% filter(LMIC_group==g) %>% arrange(year)
        bind_rows(validate_series(s$D,g,"DALY"),validate_series(s$V,g,"VLW"))
      }),
      map_dfr(c("Male","Female"),function(sx) {
        s <- fc_sex_raw %>% filter(sex_name==sx) %>% arrange(year)
        validate_series(s$V,sx,"VLW")
      }))
    write_csv(validation,file.path(r3_dir,"R3_holdout_validation_8_series.csv"))
    write_csv(validation,file.path(submission_table_dir,
      "Supplementary Table 4_holdout validation.csv"))

    # Full-series 2050 projections for the same eight series under both models.
    long_range <- bind_rows(
      map_dfr(income_fct,function(g) {
        s <- fc_inc_raw %>% filter(LMIC_group==g) %>% arrange(year)
        bind_rows(map_dfr(c("ETS","ARIMA"),function(m) {
          z <- fit_series(s$D,m,label=g); tibble(series=g,outcome="DALY",model=m,
            Year=2050,point=as.numeric(z$fc$mean[fh]),lower_95_PI=as.numeric(z$fc$lower[fh,1]),
            upper_95_PI=as.numeric(z$fc$upper[fh,1]))}),
          map_dfr(c("ETS","ARIMA"),function(m) {
          z <- fit_series(s$V,m,label=g); tibble(series=g,outcome="VLW_billion",model=m,
            Year=2050,point=as.numeric(z$fc$mean[fh]),lower_95_PI=as.numeric(z$fc$lower[fh,1]),
            upper_95_PI=as.numeric(z$fc$upper[fh,1]))}))
      }),
      map_dfr(c("Male","Female"),function(sx) {
        s <- fc_sex_raw %>% filter(sex_name==sx) %>% arrange(year)
        map_dfr(c("ETS","ARIMA"),function(m) {
          z <- fit_series(s$V,m,label=sx); tibble(series=sx,outcome="VLW_billion",model=m,
            Year=2050,point=as.numeric(z$fc$mean[fh]),lower_95_PI=as.numeric(z$fc$lower[fh,1]),
            upper_95_PI=as.numeric(z$fc$upper[fh,1]))
        })
      }))
    write_csv(long_range,file.path(r3_dir,"R3_ETS_ARIMA_2050_8_series.csv"))

    diagnostics8 <- bind_rows(
      map_dfr(income_fct,function(g) {
        s <- fc_inc_raw %>% filter(LMIC_group==g) %>% arrange(year)
        bind_rows(
          map_dfr(c("ETS","ARIMA"),function(m)
            fit_series(s$D,m,label=g)$diag %>% mutate(outcome="DALY")),
          map_dfr(c("ETS","ARIMA"),function(m)
            fit_series(s$V,m,label=g)$diag %>% mutate(outcome="VLW_billion")))
      }),
      map_dfr(c("Male","Female"),function(sx) {
        s <- fc_sex_raw %>% filter(sex_name==sx) %>% arrange(year)
        map_dfr(c("ETS","ARIMA"),function(m)
          fit_series(s$V,m,label=sx)$diag %>% mutate(outcome="VLW_billion"))
      })) %>% select(series,outcome,everything())
    write_csv(diagnostics8,file.path(r3_dir,"R3_forecast_residual_diagnostics.csv"))
    write_csv(diagnostics8,file.path(submission_table_dir,
      "Supplementary Table 5_forecast and residual diagnostics.csv"))

    # Reconciliation: primary DALY-derived VLW versus independently forecast VLW.
    direct_rows <- map_dfr(c("ETS","ARIMA"),function(m) {
      map_dfr(income_fct,function(g) {
        s <- fc_inc_raw %>% filter(LMIC_group==g) %>% arrange(year)
        z <- fit_series(s$V,m,label=g)
        tibble(model=m,Year=2024:2050,Group=g,direct_VLW_billion=as.numeric(z$fc$mean))
      })
    })
    primary_rows <- bind_rows(primary_ets$groups,primary_arima$groups) %>%
      select(model,Year,Group,derived_VLW_billion=VLW_billion)
    reconciliation <- primary_rows %>% left_join(direct_rows,by=c("model","Year","Group")) %>%
      mutate(difference_billion=direct_VLW_billion-derived_VLW_billion,
             percent_difference=100*difference_billion/derived_VLW_billion)
    write_csv(reconciliation,file.path(r3_dir,"R3_projection_reconciliation.csv"))
    write_csv(reconciliation,file.path(submission_table_dir,
      "Supplementary Table 6_projection reconciliation.csv"))
    write_csv(bind_rows(primary_ets$all,primary_arima$all),
              file.path(r3_dir,"R3_primary_ETS_ARIMA_all_LMIC_projection.csv"))
  }

  # Figure 5 — forecast
  p5a <- ggplot(fc_all,aes(year,V))+
    geom_ribbon(data=fc_all %>% filter(type=="Forecast"),aes(ymin=Vl,ymax=Vh),fill=pal$red,alpha=0.12)+
    geom_line(data=fc_all %>% filter(type=="Observed"),colour=pal$blue,linewidth=0.7)+
    geom_line(data=fc_all %>% filter(type=="Forecast"),colour=pal$red,linewidth=0.7)+
    geom_vline(xintercept=2023,linetype="dashed",colour="grey50",linewidth=0.3)+
    scale_x_continuous(breaks=seq(1990,2050,10))+scale_y_continuous(expand=expansion(mult=c(0,0.05)))+
    labs(x="Year",y="Total VLW (billion USD)")+theme_nm()
  p5b <- ggplot(fc_all,aes(year,D/1e6))+
    geom_ribbon(data=fc_all %>% filter(type=="Forecast"),aes(ymin=Dl/1e6,ymax=Dh/1e6),fill=pal$orange,alpha=0.12)+
    geom_line(data=fc_all %>% filter(type=="Observed"),colour=pal$purple,linewidth=0.7)+
    geom_line(data=fc_all %>% filter(type=="Forecast"),colour=pal$orange,linewidth=0.7)+
    geom_vline(xintercept=2023,linetype="dashed",colour="grey50",linewidth=0.3)+
    scale_x_continuous(breaks=seq(1990,2050,10))+scale_y_continuous(expand=expansion(mult=c(0,0.05)))+
    labs(x="Year",y="DALYs (millions)")+theme_nm()
  fc_inc2 <- fc_inc %>% mutate(LMIC_group=factor(LMIC_group,levels=income_fct))
  p5c <- ggplot(fc_inc2,aes(year,V,colour=LMIC_group))+
    geom_ribbon(data=fc_inc2 %>% filter(type=="Forecast"),aes(ymin=Vl,ymax=Vh,fill=LMIC_group),alpha=0.08,colour=NA)+
    geom_line(linewidth=0.6)+geom_vline(xintercept=2023,linetype="dashed",colour="grey50",linewidth=0.3)+
    scale_colour_manual(values=income_pal,name="Income Group")+scale_fill_manual(values=income_pal,guide="none")+
    scale_x_continuous(breaks=seq(1990,2050,10))+labs(x="Year",y="VLW (billion USD)")+
    theme_nm()+theme(legend.position=c(0.25,0.78))
  p5d <- ggplot(fc_sex,aes(year,V,colour=sex_name))+
    geom_ribbon(data=fc_sex %>% filter(type=="Forecast"),aes(ymin=Vl,ymax=Vh,fill=sex_name),alpha=0.08,colour=NA)+
    geom_line(linewidth=0.6)+geom_vline(xintercept=2023,linetype="dashed",colour="grey50",linewidth=0.3)+
    scale_colour_manual(values=sex_pal,name="Sex")+scale_fill_manual(values=sex_pal,guide="none")+
    scale_x_continuous(breaks=seq(1990,2050,10))+labs(x="Year",y="VLW (billion USD)")+
    theme_nm()+theme(legend.position=c(0.2,0.82))
  pdf(file.path(ie_dir,"Figure5_forecast_2050.pdf"),width=9,height=7)
  print((p5a|p5b)/(p5c|p5d)+plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))); dev.off()

  # Figure 6 — explicitly labelled unweighted mean of country ASRs + fold change
  asr_t <- df_daly_rate %>% filter(sex_name=="Both",location_id %in% lmic_ids) %>%
    left_join(df_econ %>% select(location_id,LMIC_group),by="location_id") %>%
    group_by(LMIC_group,year) %>% summarise(R=mean(rate),.groups="drop") %>%
    mutate(LMIC_group=factor(LMIC_group,levels=income_fct))
  p6a <- ggplot(asr_t,aes(year,R,colour=LMIC_group))+geom_line(linewidth=0.6)+
    scale_colour_manual(values=income_pal,name="Income Group")+scale_x_continuous(breaks=seq(1990,2023,5))+
    labs(x="Year",y="Unweighted mean of country-specific\nage-standardized DALY rates (per 100,000)")+
    theme_nm()+theme(legend.position=c(0.72,0.22))
  chg <- df %>% filter(sex_name=="Both",year %in% c(1990,2023)) %>% group_by(LMIC_group,year) %>%
    summarise(V=sum(VLW),.groups="drop") %>% pivot_wider(names_from=year,values_from=V,names_prefix="y") %>%
    mutate(fold=y2023/y1990, LMIC_group=factor(LMIC_group,levels=income_fct))
  p6b <- ggplot(chg,aes(LMIC_group,fold,fill=LMIC_group))+geom_col(width=0.55,colour="white",linewidth=0.3)+
    geom_text(aes(label=paste0(formatC(fold,format="f",digits=1),"x")),vjust=-0.3,size=2.5,fontface="bold")+
    scale_fill_manual(values=income_pal,guide="none")+scale_y_continuous(expand=expansion(mult=c(0,0.15)))+
    scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+labs(x=NULL,y="VLW fold change\n(2023 vs 1990)")+theme_nm()
  pdf(file.path(ie_dir,"Figure6_ASR_foldchange.pdf"),width=9,height=3.8)
  print(p6a|p6b+plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))); dev.off()
  if (ie==ie_base) {
    write_csv(asr_t,file.path(r3_dir,"R3_unweighted_country_ASR_means.csv"))
    write_csv(asr_t,file.path(submission_table_dir,
      "Supplementary Table 9_unweighted country ASR means.csv"))
  }

  vlw_2050 <- fc_all %>% filter(year==2050)
  cat("   VLW 2023 =",formatC(tot_all,format="f",digits=2),"B | age-sum =",
      formatC(tot_age,format="f",digits=2),"B (diff",formatC(tot_age-tot_all,format="e",digits=2),") | ",
      "M+F =",formatC(tot_sexsum,format="f",digits=2),"B\n")
  cat("   2050 =",fmt_ui(vlw_2050$V,vlw_2050$Vl,vlw_2050$Vh,2),"B | KW p =",
      formatC(kt$p.value,format="e",digits=2),"\n")
}

# ======================================================================
# 4. CROSS-IE SENSITIVITY (valuation-parameter uncertainty, reviewer M3)
# ======================================================================
ie_summary <- map_dfr(ie_values, function(ie) {
  d <- compute_allages(ie) %>% filter(year==2023,sex_name=="Both")
  d %>% summarise(IE=ie, Countries=n(),
    VLW=sum(VLW),VLW_u=sum(VLW_u),Wp=weighted.mean(pct,GDP_tot))
})
ie_tbl <- ie_summary %>% transmute(`Income Elasticity`=IE,
  `VLW base 3% (billion constant-2023 USD; point estimate)`=round(VLW,2),
  `VLW undiscounted (billion constant-2023 USD)`=formatC(VLW_u,format="f",digits=2),
  `VLW/GDP (%; point estimate)`=round(Wp,4))
write_csv(ie_tbl, file.path(out_root,"CrossIE_sensitivity_summary.csv"))
write_csv(ie_tbl,file.path(submission_table_dir,
  "Main Table 6_income-elasticity sensitivity.csv"))

# headline valuation-sensitivity range (IE 0.5–1.5) for the abstract/Table 1 note
ie_range <- ie_summary %>% summarise(
  base_IE1 = VLW[IE==1.0],
  low_IE15 = VLW[IE==1.5],
  high_IE05= VLW[IE==0.5])
write_csv(ie_range, file.path(out_root,"Headline_valuation_sensitivity_range.csv"))

ie_bar <- map_dfr(ie_values, function(ie) {
  compute_allages(ie) %>% filter(year==2023,sex_name=="Both") %>% group_by(LMIC_group) %>%
    summarise(V=sum(VLW),.groups="drop") %>%
    mutate(IE=paste0("IE = ",format(ie,nsmall=1)))
}) %>% mutate(LMIC_group=factor(LMIC_group,levels=income_fct))
ie_pal3 <- c("IE = 0.5"=pal$orange,"IE = 1.0"=pal$blue,"IE = 1.5"=pal$teal)
p_ie <- ggplot(ie_bar,aes(LMIC_group,V,fill=IE))+
  geom_col(position=position_dodge(0.7),width=0.6,colour="white",linewidth=0.2)+
  scale_fill_manual(values=ie_pal3,name="Income\nElasticity")+scale_y_continuous(expand=expansion(mult=c(0,0.1)))+
  scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+labs(x=NULL,y="VLW (billion USD)")+
  theme_nm()+theme(legend.position=c(0.15,0.8))
pdf(file.path(out_root,"CrossIE_sensitivity_bar.pdf"),width=5.5,height=3.8); print(p_ie); dev.off()

# Copy the final manuscript and supplementary figures to one upload-ready folder.
# Source filenames retain the analysis numbering; destination filenames follow
# the numbering used in the revised manuscript.
submission_figure_map <- c(
  "Figure 1_2023 snapshot.pdf" = file.path(out_root,"IE1","Figure1_2023_snapshot.pdf"),
  "Figure 2_trends.pdf" = file.path(out_root,"IE1","Figure2_trends.pdf"),
  "Figure 3_unweighted mean country ASR and fold change.pdf" = file.path(out_root,"IE1","Figure6_ASR_foldchange.pdf"),
  "Figure 4_maps.pdf" = file.path(out_root,"IE1","Figure3_maps.pdf"),
  "Figure 5_age specific.pdf" = file.path(out_root,"IE1","Figure4_age_specific.pdf"),
  "Figure 6_forecast 2050.pdf" = file.path(out_root,"IE1","Figure5_forecast_2050.pdf"),
  "Figure 7_IE sensitivity.pdf" = file.path(out_root,"CrossIE_sensitivity_bar.pdf"),
  "Supplementary Figure 1_IE0.5 snapshot.pdf" = file.path(out_root,"IE05","Figure1_2023_snapshot.pdf"),
  "Supplementary Figure 2_IE0.5 trends.pdf" = file.path(out_root,"IE05","Figure2_trends.pdf"),
  "Supplementary Figure 3_IE0.5 unweighted mean country ASR.pdf" = file.path(out_root,"IE05","Figure6_ASR_foldchange.pdf"),
  "Supplementary Figure 4_IE0.5 maps.pdf" = file.path(out_root,"IE05","Figure3_maps.pdf"),
  "Supplementary Figure 5_IE0.5 age specific.pdf" = file.path(out_root,"IE05","Figure4_age_specific.pdf"),
  "Supplementary Figure 6_IE0.5 forecast.pdf" = file.path(out_root,"IE05","Figure5_forecast_2050.pdf"),
  "Supplementary Figure 7_IE1.5 snapshot.pdf" = file.path(out_root,"IE15","Figure1_2023_snapshot.pdf"),
  "Supplementary Figure 8_IE1.5 trends.pdf" = file.path(out_root,"IE15","Figure2_trends.pdf"),
  "Supplementary Figure 9_IE1.5 unweighted mean country ASR.pdf" = file.path(out_root,"IE15","Figure6_ASR_foldchange.pdf"),
  "Supplementary Figure 10_IE1.5 maps.pdf" = file.path(out_root,"IE15","Figure3_maps.pdf"),
  "Supplementary Figure 11_IE1.5 age specific.pdf" = file.path(out_root,"IE15","Figure4_age_specific.pdf"),
  "Supplementary Figure 12_IE1.5 forecast.pdf" = file.path(out_root,"IE15","Figure5_forecast_2050.pdf")
)
missing_submission_figures <- submission_figure_map[!file.exists(submission_figure_map)]
if (length(missing_submission_figures) > 0L) {
  stop("Missing source figure(s): ",paste(missing_submission_figures,collapse=", "))
}
copy_ok <- file.copy(unname(submission_figure_map),
                     file.path(submission_fig_dir,names(submission_figure_map)),
                     overwrite=TRUE)
if (!all(copy_ok)) stop("Failed to assemble one or more R3 submission figures.")

# R3 Comment 1: without matched GBD draws, aggregate 95% UIs are not estimable.
historical_interval_policy <- tibble(
  reporting_level=c("GBD source row (country-sex or country-age)",
                    "Any sum across countries, sexes, or ages"),
  interval_status=c("GBD-reported marginal 95% UI may be retained at the original reporting level",
                    "No 95% UI reported because matched draws are unavailable"),
  prohibited_operation=c("None","Do not sum marginal 2.5th and 97.5th percentiles"),
  manuscript_action=c("Country-level source intervals may be described as GBD-reported",
                      "Report point estimates and separate IE=0.5-1.5 valuation sensitivity only"))
write_csv(historical_interval_policy,file.path(r3_dir,"R3_historical_aggregate_interval_policy.csv"))

# R3 minor point: quantify the DALY burden in the six GDP-excluded countries.
daly_128_2023 <- df_daly_all %>% filter(year==2023,sex_id %in% c(1,2)) %>%
  inner_join(df_lmic %>% select(location_id,location_name,LMIC_group),
             by=c("location_id","location_name")) %>%
  group_by(location_id,location_name,LMIC_group) %>%
  summarise(DALY=sum(DALY),.groups="drop")
excluded_ids <- setdiff(df_lmic$location_id,df_econ$location_id)
excluded_country_burden <- daly_128_2023 %>% filter(location_id %in% excluded_ids) %>%
  group_by(LMIC_group) %>% mutate(income_group_total_DALYs=sum(
    daly_128_2023$DALY[daly_128_2023$LMIC_group==first(LMIC_group)]),
    percent_of_income_group_DALYs=100*DALY/income_group_total_DALYs) %>% ungroup() %>%
  mutate(all_128_LMIC_DALYs=sum(daly_128_2023$DALY),
         percent_of_all_128_LMIC_DALYs=100*DALY/all_128_LMIC_DALYs,
         VLW_status="Not estimable because 2023 GDP inputs required for VSL transfer are missing")
excluded_summary <- tibble(
  location_id=NA_real_,location_name="All six excluded countries",LMIC_group="Combined",
  DALY=sum(excluded_country_burden$DALY),income_group_total_DALYs=NA_real_,
  percent_of_income_group_DALYs=NA_real_,all_128_LMIC_DALYs=sum(daly_128_2023$DALY),
  percent_of_all_128_LMIC_DALYs=100*sum(excluded_country_burden$DALY)/sum(daly_128_2023$DALY),
  VLW_status="Not estimable because 2023 GDP inputs required for VSL transfer are missing")
write_csv(bind_rows(excluded_country_burden,excluded_summary),
          file.path(r3_dir,"R3_excluded_country_DALY_burden.csv"))
write_csv(bind_rows(excluded_country_burden,excluded_summary),
          file.path(submission_table_dir,
            "Supplementary Table 7_excluded-country DALY burden.csv"))

submission_table_notes <- c(
  "# R3 revised tables",
  "",
  "This directory is generated by scripts/run_LMIC_Pancreatic_VLW_R3.R.",
  "It contains the six main manuscript tables and nine supplementary tables",
  "under the numbering and titles used in the revised manuscript.",
  "",
  "Key reporting policies:",
  "- Historical aggregate values are point estimates; marginal GBD lower/upper bounds are not summed.",
  "- DALY is forecast first and then monetised with fixed 2023 income-group effective VSLYs.",
  "- Reported prediction intervals are explicit 95% PIs.",
  "- All-LMIC PIs use 50,000 joint paths preserving empirical subgroup residual correlation.",
  "- Monetary results are in billion constant-2023 USD unless otherwise stated.",
  "- IE = 0.5, 1.0, and 1.5 are valuation scenarios, not statistical uncertainty bounds."
)
writeLines(submission_table_notes,file.path(submission_table_dir,"README.md"))

# ======================================================================
# 5. RECONCILIATION + PROVENANCE + SESSION INFO  (reviewers M1/M5/M6, m1/m3/m4)
# ======================================================================
recon_tbl <- bind_rows(recon_log)
write_csv(recon_tbl, file.path(diag_dir,"Reconciliation_checks.csv"))
write_csv(bind_rows(ets_diag_all), file.path(diag_dir,"Forecast_diagnostics_all_IE.csv"))

cov <- tibble(
  item=c("LMIC countries in 204_with_LMIC.csv (LMIC==1)",
         "  ... with matched 2023 World Bank GDP (PPP)",
         "  ... also with sex-specific 2023 HALE (analysed)",
         "Countries excluded for missing GDP",
         "Countries excluded for missing HALE"),
  n=c(n_lmic_total, n_with_gdp, length(usable_ids),
      n_lmic_total - n_with_gdp, n_with_gdp - length(usable_ids)))
write_csv(cov, file.path(diag_dir,"Country_coverage.csv"))

prov <- c(
  "# Data provenance (reviewer m1)",
  "",
  "- Disease burden: GBD 2023 (Global Burden of Disease Study 2023), pancreatic cancer DALYs,",
  "  Number + Age-standardized Rate, 1990-2023, both sexes and sex-specific, by 5-year age group.",
  "  Source files: data_raw/gbd_daly_yearly/measure2_DALYs_year1990.csv through year2023.csv",
  "  (annual exports from the IHME GBD Results Tool; the canonical workflow reads these files directly).",
  "- HALE: GBD 2023 health-adjusted life expectancy, 2023, by sex and age. Source: data_raw/external_metadata/HALE.csv.",
  "- GDP per capita & total (PPP, current international $), 2023: World Bank World Development Indicators",
  "  (NY.GDP.PCAP.PP.CD, NY.GDP.MKTP.PP.CD). Source: data_raw/external_metadata/gdp.csv.",
  "- Income classification (LIC/LMIC/UMIC) & LMIC flag: data_raw/external_metadata/204_with_LMIC.csv (World Bank FY24).",
  "- VSL reference: US DOT 2023 guidance, VSL = US$13.2 million; US GDP pc (PPP) 2023 = US$82,304.62.",
  "",
  paste0("Extraction/analysis date: ", format(Sys.Date())),
  "",
  "# Method note (reviewers M1, M2, m2, m5)",
  "- Single VSLY definition: VSLY_i = VSL_i / annuity(HALE_i, r), annuity(T,r) = (1-(1+r)^-T)/r,",
  "  with T_i = HALE_i (sex-specific, all-ages) and base r = 0.03. An undiscounted variant (r = 0,",
  "  VSLY = VSL_i / HALE_i) is reported as sensitivity.",
  "- VLW = VSLY x DALY, applied uniformly to every age; age/sex/income totals are obtained by",
  "  summation, so age-specific VLW (manuscript Table 4; generated as Table6_age_specific.csv)",
  "  sums exactly to the headline total (Table 1), and",
  "  both-sex = male + female (Tables 1, 2).",
  "- Because VSLY is uniform across ages, age-specific VLW is proportional to age-specific DALYs;",
  "  the earlier 'VLW peaks at 70-74' result was an artefact of an inconsistent age denominator and",
  "  is not reported.",
  "",
  "# R3 forecast and uncertainty policy",
  "- Historical aggregate 95% UIs are not reported because matched GBD draws are unavailable; marginal bounds are never summed.",
  "- DALY is the primary projection estimand. Projected DALYs are monetized with fixed 2023 group effective VSLY values.",
  "- ETS and non-seasonal ARIMA are both projected to 2050. All forecast calls set level=95 explicitly.",
  "- All-LMIC PIs use joint simulation with empirical residual correlation; subgroup endpoints are never summed.",
  "- The income-group rate panel is the unweighted mean of country-specific age-standardized rates.",
  "",
  "# Software (reviewer m3)",
  paste0("- ", R.version.string),
  paste0("- forecast ", as.character(packageVersion("forecast")),
         "; tidyverse ", as.character(packageVersion("tidyverse")),
         "; sf ", as.character(packageVersion("sf")),
         "; sandwich ",as.character(packageVersion("sandwich"))),
  "- Forecast diagnostics: outputs/results_VLW/diagnostics/Forecast_diagnostics_all_IE.csv")
writeLines(prov, file.path(diag_dir,"PROVENANCE_and_METHODS.md"))
capture.output(sessionInfo(), file=file.path(diag_dir,"sessionInfo.txt"))

cat("\n================ RECONCILIATION ================\n")
print(recon_tbl)
cat("\nALL COMPLETE. Output:", out_root, "\n")
