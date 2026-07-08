################################################################################
# LMIC Pancreatic Cancer — Value of Lost Welfare (VLW) Analysis  —  v2 (CONSISTENT)
# GBD 2023 · 1990-2023 · Forecast to 2050 · IE = 0.5, 1.0, 1.5
#
# v2 rewrite addresses reviewer (iScience ISCIENCE-D-26-05974) statistical points:
#   M1/M2 : ONE consistent VSLY definition (uniform discounted-annuity VSLY).
#           VSLY_i = VSL_i / annuity(HALE_i, r),  annuity(T,r)=(1-(1+r)^-T)/r
#           Applied uniformly to every DALY; age/sex/income all aggregate from the
#           country×sex(×age) level -> Table 4/6 (age) sums EXACTLY to Table 1 total.
#   M6    : Both-sex = Male + Female (derived by summation, never from Both-HALE).
#   M3    : reports income-elasticity range (IE 0.5–1.5) ALONGSIDE the DALY-only
#           95% UI, and relabels the DALY-only interval explicitly.
#   M4    : Kruskal-Wallis actually run + reported (H, df, p, eps^2) + post-hoc.
#   M5    : ETS diagnostics (params, AICc, Ljung-Box) saved; all-LMIC total
#           forecast = SUM of income-group forecasts (reconciles to components).
#   m2    : discounting via annuity on T_i = HALE_i (explicitly defined); an
#           undiscounted (r=0) variant is reported as sensitivity.
#   m3/m4 : sessionInfo + package versions + country-coverage log written out.
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
daly_file  <- file.path(base, "data_processed/merged.csv")
hale_file  <- file.path(base, "data_raw/external_metadata/HALE.csv")
gdp_file   <- file.path(base, "data_raw/external_metadata/gdp.csv")
lmic_file  <- file.path(base, "data_raw/external_metadata/204_with_LMIC.csv")
world_file <- file.path(base, "data_raw/external_metadata/df_world2.geojson")
out_root   <- file.path(base, "outputs/results_VLW")
diag_dir   <- file.path(out_root, "diagnostics")
dir.create(out_root, showWarnings = FALSE, recursive = TRUE)
dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)

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
  str_replace_all(" "," ") %>% iconv(from="UTF-8",to="ASCII//TRANSLIT") %>% str_squish()
fmt_ui <- function(v,lo,hi,d=1,s=1) paste0(
  formatC(v/s,format="f",digits=d,big.mark=",")," (",
  formatC(lo/s,format="f",digits=d,big.mark=","),"–",
  formatC(hi/s,format="f",digits=d,big.mark=","),")")

std_ages <- c("<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
  "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
  "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
  "75-79 years","80-84 years","85-89 years","90-94 years","95+ years")
age_mids <- c(2.5,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87,92,97)
age_lookup <- tibble(age_name=std_ages, age_mid=age_mids, age_order=seq_along(std_ages))

cat("================================================================\n")
cat("  Pancreatic LMIC VLW  v2 (consistent)  · IE=0.5/1.0/1.5\n")
cat("================================================================\n\n")

# ======================================================================
# 1. READ DATA
# ======================================================================
cat("[1] Reading data ...\n")

df_lmic_raw <- read_csv(lmic_file, show_col_types=FALSE)
df_lmic <- df_lmic_raw %>% filter(LMIC==1) %>%
  select(location_id,location_name,LMIC_group) %>%
  mutate(wb=ifelse(location_name %in% names(gbd_to_wb),gbd_to_wb[location_name],location_name),
         wb_n=norm_name(wb))

df_gdp <- read_csv(gdp_file,show_col_types=FALSE) %>%
  filter(year==2023,!is.na(NY.GDP.PCAP.PP.CD),!is.na(NY.GDP.MKTP.PP.CD)) %>%
  select(country,GDP_pc=NY.GDP.PCAP.PP.CD,GDP_tot=NY.GDP.MKTP.PP.CD) %>%
  mutate(cn=norm_name(country))

df_econ <- df_lmic %>% left_join(df_gdp,by=c("wb_n"="cn")) %>%
  filter(!is.na(GDP_pc)) %>% select(location_id,location_name,LMIC_group,GDP_pc,GDP_tot)
lmic_ids <- df_econ$location_id

# coverage log (reviewer m4)
n_lmic_total <- nrow(df_lmic)
n_with_gdp   <- nrow(df_econ)

df_daly_all <- read_csv(daly_file,show_col_types=FALSE) %>%
  filter(cause_name=="Pancreatic cancer",
         measure_name=="DALYs (Disability-Adjusted Life Years)",
         metric_name=="Number", age_name=="All ages",
         location_id %in% lmic_ids) %>%
  select(location_id,location_name,sex_id,sex_name,year,DALY=val,lower,upper)

df_daly_age <- read_csv(daly_file,show_col_types=FALSE) %>%
  filter(cause_name=="Pancreatic cancer",
         measure_name=="DALYs (Disability-Adjusted Life Years)",
         metric_name=="Number", age_name %in% std_ages,
         location_id %in% lmic_ids) %>%
  select(location_id,location_name,sex_id,sex_name,age_name,year,
         DALY_age=val,lower_age=lower,upper_age=upper)

df_daly_rate <- read_csv(daly_file,show_col_types=FALSE) %>%
  filter(cause_name=="Pancreatic cancer",
         measure_name=="DALYs (Disability-Adjusted Life Years)",
         metric_name=="Rate", age_name=="Age-standardized",
         location_id %in% lmic_ids) %>%
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
    summarise(DALY=sum(DALY),lower=sum(lower),upper=sum(upper),
              VLW=sum(VLW),VLW_lo=sum(VLW_lo),VLW_hi=sum(VLW_hi),
              VLW_u=sum(VLW_u),VLW_u_lo=sum(VLW_u_lo),VLW_u_hi=sum(VLW_u_hi),
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
    left_join(age_lookup, by="age_name") %>%
    mutate(VLW_a=VSLY*DALY_age/1e9, VLW_a_lo=VSLY*lower_age/1e9, VLW_a_hi=VSLY*upper_age/1e9)
  both <- base %>%
    group_by(location_id,LMIC_group,age_name,age_mid,age_order,year) %>%
    summarise(DALY_age=sum(DALY_age),lower_age=sum(lower_age),upper_age=sum(upper_age),
              VLW_a=sum(VLW_a),VLW_a_lo=sum(VLW_a_lo),VLW_a_hi=sum(VLW_a_hi),.groups="drop") %>%
    mutate(sex_id=3L, sex_name="Both")
  bind_rows(
    base %>% select(location_id,LMIC_group,sex_id,sex_name,age_name,age_mid,age_order,
                    year,DALY_age,lower_age,upper_age,VLW_a,VLW_a_lo,VLW_a_hi),
    both)
}

# ── ETS fit + diagnostics helper (reviewer M5) ────────────────────────────────
fh <- 2050 - 2023
ets_fit <- function(series, label) {
  ts1 <- ts(series, start=1990, frequency=1)
  fit <- ets(ts1, model="AAN", damped=TRUE)
  fc  <- forecast(fit, h=fh, level=95)
  p   <- fit$par
  gv  <- function(nm) if (nm %in% names(p)) unname(p[nm]) else NA_real_
  lb  <- tryCatch(Box.test(residuals(fit), lag=10, type="Ljung-Box",
                           fitdf=min(length(p),9))$p.value, error=function(e) NA_real_)
  rmse<- tryCatch(unname(accuracy(fit)[1,"RMSE"]), error=function(e) NA_real_)
  diag <- tibble(series=label, method=fit$method,
                 alpha=gv("alpha"), beta=gv("beta"), phi=gv("phi"),
                 sigma2=fit$sigma2, AIC=fit$aic, AICc=fit$aicc, BIC=fit$bic,
                 RMSE=rmse, LjungBox_p=lb)
  list(fc=fc, diag=diag)
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
      D=sum(DALY),Dl=sum(lower),Dh=sum(upper),
      V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi),
      Vu=sum(VLW_u),Vul=sum(VLW_u_lo),Vuh=sum(VLW_u_hi),
      Wp=weighted.mean(pct,GDP_tot),Wpl=weighted.mean(pct_lo,GDP_tot),
      Wph=weighted.mean(pct_hi,GDP_tot), mH=mean(HALE),.groups="drop")
  }

  # Table 1 — income summary (+ IE valuation-sensitivity range only on base IE)
  t1_core <- bind_rows(
    mk_sum(d23,"LMIC_group") %>% arrange(factor(LMIC_group,levels=income_fct)),
    mk_sum(d23) %>% mutate(LMIC_group="All LMICs"))
  t1 <- t1_core %>% transmute(`Income Group`=LMIC_group, Countries=N,
    `DALYs (thousands)`=fmt_ui(D,Dl,Dh,1,1e3),
    `VLW (billion USD) [DALY 95% UI]`=fmt_ui(V,Vl,Vh,2),
    `VLW undiscounted r=0 (billion USD)`=fmt_ui(Vu,Vul,Vuh,2),
    `VLW/GDP (%) [DALY 95% UI]`=fmt_ui(Wp,Wpl,Wph,4),
    `Mean HALE`=formatC(mH,format="f",digits=1))
  write_csv(t1, file.path(ie_dir,"Table1_income_summary.csv"))

  # Table 2 — sex × income (Male+Female sum to Both by construction)
  t2 <- d23_all %>% group_by(sex_name,LMIC_group) %>%
    summarise(N=n(),D=sum(DALY),Dl=sum(lower),Dh=sum(upper),
      V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi),
      Vu=sum(VLW_u),Vul=sum(VLW_u_lo),Vuh=sum(VLW_u_hi),.groups="drop") %>%
    transmute(Sex=sex_name,`Income Group`=LMIC_group,Countries=N,
      `DALYs (thousands)`=fmt_ui(D,Dl,Dh,1,1e3),
      `VLW (billion USD)`=fmt_ui(V,Vl,Vh,2),
      `VLW undiscounted (billion USD)`=fmt_ui(Vu,Vul,Vuh,2))
  write_csv(t2, file.path(ie_dir,"Table2_sex_income.csv"))

  # Table 3 — all countries ranked by VLW/GDP
  t3 <- d23 %>% arrange(desc(pct)) %>% mutate(Rank=row_number()) %>%
    transmute(Rank,Country=location_name,`Income Group`=LMIC_group,
      `GDP pc (PPP)`=formatC(GDP_pc,format="f",digits=0,big.mark=","),
      `DALYs (thousands)`=fmt_ui(DALY,lower,upper,1,1e3),
      `VLW (billion USD)`=fmt_ui(VLW,VLW_lo,VLW_hi,3),
      `VLW/GDP (%)`=fmt_ui(pct,pct_lo,pct_hi,4))
  write_csv(t3, file.path(ie_dir,"Table3_all_LMIC_countries.csv"))

  # Table 4 — country × sex full
  t4 <- d23_all %>% transmute(Country=location_name,Sex=sex_name,`Income Group`=LMIC_group,
    `DALYs`=fmt_ui(DALY,lower,upper,0),
    `VLW (billion USD)`=fmt_ui(VLW,VLW_lo,VLW_hi,3),
    `VLW undiscounted (billion USD)`=fmt_ui(VLW_u,VLW_u_lo,VLW_u_hi,3),
    `VLW/GDP (%)`=fmt_ui(pct,pct_lo,pct_hi,4))
  write_csv(t4, file.path(ie_dir,"Table4_country_full.csv"))

  # Table 5 — temporal
  t5 <- df %>% filter(sex_name=="Both",year %in% c(1990,2000,2010,2023)) %>%
    group_by(LMIC_group,year) %>%
    summarise(D=sum(DALY),Dl=sum(lower),Dh=sum(upper),
      V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi),.groups="drop") %>%
    transmute(`Income Group`=LMIC_group,Year=year,
      `DALYs (thousands)`=fmt_ui(D,Dl,Dh,1,1e3),
      `VLW (billion USD)`=fmt_ui(V,Vl,Vh,2)) %>%
    arrange(factor(`Income Group`,levels=income_fct),Year)
  write_csv(t5, file.path(ie_dir,"Table5_temporal.csv"))

  # Table 6 — age-specific (now SUMS to Table 1 total)
  age_both <- df_av %>% filter(sex_name=="Both") %>%
    group_by(age_name,age_mid,age_order) %>%
    summarise(D=sum(DALY_age),Dl=sum(lower_age),Dh=sum(upper_age),
      V=sum(VLW_a),Vl=sum(VLW_a_lo),Vh=sum(VLW_a_hi),.groups="drop") %>%
    arrange(age_order)
  t6 <- age_both %>% transmute(`Age Group`=age_name,
      `DALYs`=fmt_ui(D,Dl,Dh,0),
      `VLW (billion USD)`=fmt_ui(V,Vl,Vh,3),
      `Share of total VLW (%)`=formatC(V/sum(V)*100,format="f",digits=1))
  write_csv(t6, file.path(ie_dir,"Table6_age_specific.csv"))

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

  # ════════ FIGURES ════════
  # Figure 1 — 2023 snapshot
  bar1 <- d23 %>% group_by(LMIC_group) %>%
    summarise(V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi),.groups="drop") %>%
    mutate(LMIC_group=factor(LMIC_group,levels=income_fct))
  p1a <- ggplot(bar1,aes(LMIC_group,V,fill=LMIC_group))+
    geom_col(width=0.6,colour="white",linewidth=0.3)+
    geom_errorbar(aes(ymin=Vl,ymax=Vh),width=0.12,linewidth=0.3)+
    scale_fill_manual(values=income_pal,guide="none")+
    scale_y_continuous(expand=expansion(mult=c(0,0.1)))+
    scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+labs(x=NULL,y="VLW (billion USD)")+theme_nm()
  p1b <- ggplot(d23,aes(factor(LMIC_group,income_fct),pct,fill=LMIC_group))+
    geom_boxplot(width=0.5,outlier.shape=NA,alpha=0.5,linewidth=0.3)+
    geom_jitter(aes(colour=LMIC_group),width=0.12,size=0.6,alpha=0.5)+
    scale_fill_manual(values=income_pal,guide="none")+scale_colour_manual(values=income_pal,guide="none")+
    scale_y_continuous(labels=\(x) paste0(x,"%"))+scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+
    labs(x=NULL,y="VLW / GDP (%)")+theme_nm()
  sex_bar <- d23_all %>% filter(sex_name %in% c("Male","Female")) %>%
    group_by(sex_name,LMIC_group) %>% summarise(V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi),.groups="drop") %>%
    mutate(LMIC_group=factor(LMIC_group,levels=income_fct))
  p1c <- ggplot(sex_bar,aes(LMIC_group,V,fill=sex_name))+
    geom_col(position=position_dodge(0.65),width=0.55,colour="white",linewidth=0.2)+
    geom_errorbar(aes(ymin=Vl,ymax=Vh),position=position_dodge(0.65),width=0.12,linewidth=0.25)+
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
    summarise(V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi),.groups="drop")
  p2a <- ggplot(tot_t,aes(year,V))+geom_ribbon(aes(ymin=Vl,ymax=Vh),fill=pal$blue,alpha=0.12)+
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
      breaks=c(0,0.01,0.1,1,10,100),labels=c("0","0.01","0.1","1","10","100"),name="VLW\n(billion)")+theme_nm_map()
  p3b <- ggplot(w)+geom_sf(aes(fill=pct),colour="grey40",linewidth=0.06)+
    scale_fill_gradientn(colours=map_red,na.value="grey92",trans="log1p",
      breaks=c(0,0.01,0.05,0.1,0.5,1),labels=c("0","0.01","0.05","0.1","0.5","1"),name="VLW/GDP\n(%)")+theme_nm_map()
  w2 <- df_world %>% left_join(d23 %>% select(location_id,LMIC_group),by="location_id")
  p3c <- ggplot(w2)+geom_sf(aes(fill=LMIC_group),colour="grey40",linewidth=0.06)+
    scale_fill_manual(values=income_pal,na.value="grey92",name="Income Group")+theme_nm_map()
  pdf(file.path(ie_dir,"Figure3_maps.pdf"),width=8,height=11)
  print(p3a/p3b/p3c+plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))); dev.off()

  # Figure 4 — age-specific (VLW now proportional to DALYs)
  age_all <- df_av %>% filter(sex_name=="Both") %>% group_by(age_name,age_mid,age_order) %>%
    summarise(V=sum(VLW_a),Vl=sum(VLW_a_lo),Vh=sum(VLW_a_hi),.groups="drop") %>% arrange(age_order)
  p4a <- ggplot(age_all,aes(age_mid,V))+geom_ribbon(aes(ymin=Vl,ymax=Vh),fill=pal$teal,alpha=0.18)+
    geom_line(colour=pal$teal,linewidth=0.7)+geom_point(colour=pal$teal,size=1.2)+
    scale_x_continuous(breaks=seq(0,100,10))+labs(x="Age (years)",y="VLW (billion USD)")+theme_nm()
  age_daly <- df_av %>% filter(sex_name=="Both") %>% group_by(age_name,age_mid,age_order) %>%
    summarise(D=sum(DALY_age)/1e3,Dl=sum(lower_age)/1e3,Dh=sum(upper_age)/1e3,.groups="drop") %>% arrange(age_order)
  p4b <- ggplot(age_daly,aes(age_mid,D))+geom_ribbon(aes(ymin=Dl,ymax=Dh),fill=pal$purple,alpha=0.15)+
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

  # ════════ FORECAST to 2050 (reconciled: total = sum of income groups) ════════
  fc_inc_raw <- df %>% filter(sex_name=="Both") %>% group_by(LMIC_group,year) %>%
    summarise(V=sum(VLW),D=sum(DALY),.groups="drop")
  inc_fc <- list(); diags <- list()
  for (g in income_fct) {
    s <- fc_inc_raw %>% filter(LMIC_group==g) %>% arrange(year)
    fv <- ets_fit(s$V, paste0(ie_tag,"|VLW|",g)); fd <- ets_fit(s$D, paste0(ie_tag,"|DALY|",g))
    diags[[length(diags)+1]] <- fv$diag; diags[[length(diags)+1]] <- fd$diag
    inc_fc[[g]] <- bind_rows(
      s %>% mutate(Vl=NA_real_,Vh=NA_real_,Dl=NA_real_,Dh=NA_real_,type="Observed"),
      tibble(LMIC_group=g,year=2024:2050,V=as.numeric(fv$fc$mean),
             Vl=as.numeric(fv$fc$lower[,1]),Vh=as.numeric(fv$fc$upper[,1]),
             D=as.numeric(fd$fc$mean),Dl=as.numeric(fd$fc$lower[,1]),Dh=as.numeric(fd$fc$upper[,1]),
             type="Forecast"))
  }
  fc_inc <- bind_rows(inc_fc)
  # all-LMIC total = SUM of income-group forecasts (reconciles, reviewer M5)
  fc_all <- fc_inc %>% group_by(year,type) %>%
    summarise(V=sum(V),Vl=sum(Vl),Vh=sum(Vh),D=sum(D),Dl=sum(Dl),Dh=sum(Dh),.groups="drop")
  # sex forecasts (Both = Male+Female)
  fc_sex_raw <- df %>% filter(sex_name %in% c("Male","Female")) %>% group_by(sex_name,year) %>%
    summarise(V=sum(VLW),.groups="drop")
  sex_fc <- list()
  for (sx in c("Male","Female")) {
    s <- fc_sex_raw %>% filter(sex_name==sx) %>% arrange(year)
    fv <- ets_fit(s$V, paste0(ie_tag,"|VLW|",sx)); diags[[length(diags)+1]] <- fv$diag
    sex_fc[[sx]] <- bind_rows(
      s %>% mutate(Vl=NA_real_,Vh=NA_real_,type="Observed"),
      tibble(sex_name=sx,year=2024:2050,V=as.numeric(fv$fc$mean),
             Vl=as.numeric(fv$fc$lower[,1]),Vh=as.numeric(fv$fc$upper[,1]),type="Forecast"))
  }
  fc_sex <- bind_rows(sex_fc)
  ets_diag_all[[ie_tag]] <- bind_rows(diags)
  write_csv(bind_rows(diags), file.path(ie_dir,"Stats_ETS_diagnostics.csv"))

  # Forecast tables
  fc_yrs <- c(2025,2030,2035,2040,2045,2050)
  t7a <- fc_all %>% filter(year %in% fc_yrs) %>% transmute(Year=year,Group="All LMICs (=sum of income groups)",
      `VLW (billion USD, 95% PI)`=fmt_ui(V,Vl,Vh,2),
      `DALYs (thousands, 95% PI)`=fmt_ui(D,Dl,Dh,1,1e3))
  t7b <- fc_inc %>% filter(year %in% fc_yrs,type=="Forecast") %>% transmute(Year=year,Group=LMIC_group,
      `VLW (billion USD, 95% PI)`=fmt_ui(V,Vl,Vh,2),
      `DALYs (thousands, 95% PI)`=fmt_ui(D,Dl,Dh,1,1e3))
  write_csv(bind_rows(t7a,t7b) %>% arrange(Year,Group), file.path(ie_dir,"Table7_forecast_summary.csv"))
  t8 <- fc_all %>% filter(type=="Forecast") %>% transmute(Year=year,
      `VLW (billion USD, 95% PI)`=fmt_ui(V,Vl,Vh,2),
      `DALYs (thousands, 95% PI)`=fmt_ui(D,Dl,Dh,1,1e3))
  write_csv(t8, file.path(ie_dir,"Table8_forecast_annual.csv"))

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

  # Figure 6 — ASR + fold change
  asr_t <- df_daly_rate %>% filter(sex_name=="Both",location_id %in% lmic_ids) %>%
    left_join(df_econ %>% select(location_id,LMIC_group),by="location_id") %>%
    group_by(LMIC_group,year) %>% summarise(R=mean(rate),.groups="drop") %>%
    mutate(LMIC_group=factor(LMIC_group,levels=income_fct))
  p6a <- ggplot(asr_t,aes(year,R,colour=LMIC_group))+geom_line(linewidth=0.6)+
    scale_colour_manual(values=income_pal,name="Income Group")+scale_x_continuous(breaks=seq(1990,2023,5))+
    labs(x="Year",y="Age-standardized DALY rate\n(per 100,000)")+theme_nm()+theme(legend.position=c(0.72,0.22))
  chg <- df %>% filter(sex_name=="Both",year %in% c(1990,2023)) %>% group_by(LMIC_group,year) %>%
    summarise(V=sum(VLW),.groups="drop") %>% pivot_wider(names_from=year,values_from=V,names_prefix="y") %>%
    mutate(fold=y2023/y1990, LMIC_group=factor(LMIC_group,levels=income_fct))
  p6b <- ggplot(chg,aes(LMIC_group,fold,fill=LMIC_group))+geom_col(width=0.55,colour="white",linewidth=0.3)+
    geom_text(aes(label=paste0(formatC(fold,format="f",digits=1),"×")),vjust=-0.3,size=2.5,fontface="bold")+
    scale_fill_manual(values=income_pal,guide="none")+scale_y_continuous(expand=expansion(mult=c(0,0.15)))+
    scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+labs(x=NULL,y="VLW fold change\n(2023 vs 1990)")+theme_nm()
  pdf(file.path(ie_dir,"Figure6_ASR_foldchange.pdf"),width=9,height=3.8)
  print(p6a|p6b+plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))); dev.off()

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
    VLW=sum(VLW),VLW_lo=sum(VLW_lo),VLW_hi=sum(VLW_hi),
    VLW_u=sum(VLW_u),
    Wp=weighted.mean(pct,GDP_tot),Wpl=weighted.mean(pct_lo,GDP_tot),Wph=weighted.mean(pct_hi,GDP_tot))
})
ie_tbl <- ie_summary %>% transmute(`Income Elasticity`=IE,
  `VLW base 3% (billion USD) [DALY 95% UI]`=fmt_ui(VLW,VLW_lo,VLW_hi,2),
  `VLW undiscounted (billion USD)`=formatC(VLW_u,format="f",digits=2),
  `VLW/GDP (%)`=fmt_ui(Wp,Wpl,Wph,4))
write_csv(ie_tbl, file.path(out_root,"CrossIE_sensitivity_summary.csv"))

# headline valuation-sensitivity range (IE 0.5–1.5) for the abstract/Table 1 note
ie_range <- ie_summary %>% summarise(
  base_IE1 = VLW[IE==1.0],
  low_IE15 = VLW[IE==1.5],
  high_IE05= VLW[IE==0.5])
write_csv(ie_range, file.path(out_root,"Headline_valuation_sensitivity_range.csv"))

ie_bar <- map_dfr(ie_values, function(ie) {
  compute_allages(ie) %>% filter(year==2023,sex_name=="Both") %>% group_by(LMIC_group) %>%
    summarise(V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi),.groups="drop") %>%
    mutate(IE=paste0("IE = ",format(ie,nsmall=1)))
}) %>% mutate(LMIC_group=factor(LMIC_group,levels=income_fct))
ie_pal3 <- c("IE = 0.5"=pal$orange,"IE = 1.0"=pal$blue,"IE = 1.5"=pal$teal)
p_ie <- ggplot(ie_bar,aes(LMIC_group,V,fill=IE))+
  geom_col(position=position_dodge(0.7),width=0.6,colour="white",linewidth=0.2)+
  geom_errorbar(aes(ymin=Vl,ymax=Vh),position=position_dodge(0.7),width=0.12,linewidth=0.25)+
  scale_fill_manual(values=ie_pal3,name="Income\nElasticity")+scale_y_continuous(expand=expansion(mult=c(0,0.1)))+
  scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+labs(x=NULL,y="VLW (billion USD)")+
  theme_nm()+theme(legend.position=c(0.15,0.8))
pdf(file.path(out_root,"CrossIE_sensitivity_bar.pdf"),width=5.5,height=3.8); print(p_ie); dev.off()

# ======================================================================
# 5. RECONCILIATION + PROVENANCE + SESSION INFO  (reviewers M1/M5/M6, m1/m3/m4)
# ======================================================================
recon_tbl <- bind_rows(recon_log)
write_csv(recon_tbl, file.path(diag_dir,"Reconciliation_checks.csv"))
write_csv(bind_rows(ets_diag_all), file.path(diag_dir,"ETS_diagnostics_all_IE.csv"))

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
  "  Source file: Pancreatic/merged.csv (extracted from IHME GBD Results Tool).",
  "- HALE: GBD 2023 health-adjusted life expectancy, 2023, by sex and age. Source: DM/LMICDM/HALE.csv.",
  "- GDP per capita & total (PPP, current international $), 2023: World Bank World Development Indicators",
  "  (NY.GDP.PCAP.PP.CD, NY.GDP.MKTP.PP.CD). Source: DM/LMICDM/gdp.csv.",
  "- Income classification (LIC/LMIC/UMIC) & LMIC flag: DM/LMICDM/204_with_LMIC.csv (World Bank FY24).",
  "- VSL reference: US DOT 2023 guidance, VSL = US$13.2 million; US GDP pc (PPP) 2023 = US$82,304.62.",
  "",
  paste0("Extraction/analysis date: ", format(Sys.Date())),
  "",
  "# Method note (reviewers M1, M2, m2, m5)",
  "- Single VSLY definition: VSLY_i = VSL_i / annuity(HALE_i, r), annuity(T,r) = (1-(1+r)^-T)/r,",
  "  with T_i = HALE_i (sex-specific, all-ages) and base r = 0.03. An undiscounted variant (r = 0,",
  "  VSLY = VSL_i / HALE_i) is reported as sensitivity.",
  "- VLW = VSLY x DALY, applied uniformly to every age; age/sex/income totals are obtained by",
  "  summation, so age-specific VLW (Table 6) sums exactly to the headline total (Table 1), and",
  "  both-sex = male + female (Tables 1, 2).",
  "- Because VSLY is uniform across ages, age-specific VLW is proportional to age-specific DALYs;",
  "  the earlier 'VLW peaks at 70-74' result was an artefact of an inconsistent age denominator and",
  "  is not reported.",
  "",
  "# Software (reviewer m3)",
  paste0("- ", R.version.string),
  paste0("- forecast ", as.character(packageVersion("forecast")),
         "; tidyverse ", as.character(packageVersion("tidyverse")),
         "; sf ", as.character(packageVersion("sf"))),
  "- ETS: forecast::ets(model='AAN', damped=TRUE); diagnostics in diagnostics/ETS_diagnostics_all_IE.csv")
writeLines(prov, file.path(diag_dir,"PROVENANCE_and_METHODS.md"))
capture.output(sessionInfo(), file=file.path(diag_dir,"sessionInfo.txt"))

cat("\n================ RECONCILIATION ================\n")
print(recon_tbl)
cat("\nALL COMPLETE. Output:", out_root, "\n")
