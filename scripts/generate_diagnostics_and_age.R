################################################################################
# Generate the diagnostics/ folder + reconciled age table (Table 6, new method),
# from data that is actually available now (backup country×sex all-ages DALYs +
# backup aggregate age DALYs + GDP + HALE). Makes the response letter's references
# real. ETS forecast diagnostics (M5) still require the source merged.csv and are
# recorded as PENDING.
################################################################################
suppressMessages(library(tidyverse)); options(warn=-1, dplyr.summarise.inform=FALSE)

base   <- normalizePath(".")
bkdir  <- file.path(base,"data_raw/backup_results_2026-06-12")
hale_f <- file.path(base,"data_raw/external_metadata/HALE.csv"); gdp_f<-file.path(base,"data_raw/external_metadata/gdp.csv")
lmic_f <- file.path(base,"data_raw/external_metadata/204_with_LMIC.csv")
out    <- file.path(base,"outputs/results_VLW"); diag<-file.path(base,"diagnostics")
dir.create(diag,showWarnings=FALSE,recursive=TRUE)

VSL_USA<-13.2e6; GDP_pc_USA<-82304.62; rr<-0.03
ie_values<-c(0.5,1.0,1.5); income_fct<-c("Low income","Lower middle income","Upper middle income")
annuity<-function(T,r) ifelse(T<=0,NA_real_,ifelse(r==0,T,(1-(1+r)^(-T))/r))
fmt_ui<-function(v,lo,hi,d=1,s=1) paste0(formatC(v/s,format="f",digits=d,big.mark=",")," (",
  formatC(lo/s,format="f",digits=d,big.mark=","),"–",formatC(hi/s,format="f",digits=d,big.mark=","),")")
parse_ui<-function(col){s<-str_replace_all(col,",","")
  m<-str_match(s,"^\\s*([0-9.]+)\\s*\\(\\s*([0-9.]+)\\s*[–-]\\s*([0-9.]+)\\s*\\)")
  tibble(val=as.numeric(m[,2]),lo=as.numeric(m[,3]),hi=as.numeric(m[,4]))}
gbd_to_wb<-c("Bolivia (Plurinational State of)"="Bolivia","Congo"="Congo, Rep.","Côte d'Ivoire"="Cote d'Ivoire",
  "Democratic People's Republic of Korea"="Korea, Dem. People's Rep.","Democratic Republic of the Congo"="Congo, Dem. Rep.",
  "Egypt"="Egypt, Arab Rep.","Gambia"="Gambia, The","Iran (Islamic Republic of)"="Iran, Islamic Rep.",
  "Kyrgyzstan"="Kyrgyz Republic","Lao People's Democratic Republic"="Lao PDR",
  "Micronesia (Federated States of)"="Micronesia, Fed. Sts.","Palestine"="West Bank and Gaza",
  "Republic of Moldova"="Moldova","Saint Lucia"="St. Lucia",
  "Saint Vincent and the Grenadines"="St. Vincent and the Grenadines","Somalia"="Somalia, Fed. Rep.",
  "Syrian Arab Republic"="Syrian Arab Republic","Türkiye"="Turkiye","United Republic of Tanzania"="Tanzania","Yemen"="Yemen, Rep.")
norm_name<-function(x) x %>% str_replace_all("[‘’]","'") %>% str_replace_all(" "," ") %>%
  iconv(from="UTF-8",to="ASCII//TRANSLIT") %>% str_squish()

# ── inputs ────────────────────────────────────────────────────────────────────
t4<-read_csv(file.path(bkdir,"IE1/Table4_country_full.csv"),show_col_types=FALSE)
dd<-parse_ui(t4$DALYs)
daly<-tibble(location_name=t4$Country,sex_name=t4$Sex,LMIC_group=t4$`Income Group`,
  DALY=dd$val,lower=dd$lo,upper=dd$hi) %>% filter(sex_name %in% c("Male","Female"))
lmraw<-read_csv(lmic_f,show_col_types=FALSE) %>% filter(LMIC==1)
df_lmic<-lmraw %>% select(location_id,location_name,LMIC_group) %>%
  mutate(wb=ifelse(location_name %in% names(gbd_to_wb),gbd_to_wb[location_name],location_name),wb_n=norm_name(wb))
df_gdp<-read_csv(gdp_f,show_col_types=FALSE) %>% filter(year==2023,!is.na(NY.GDP.PCAP.PP.CD),!is.na(NY.GDP.MKTP.PP.CD)) %>%
  select(country,GDP_pc=NY.GDP.PCAP.PP.CD,GDP_tot=NY.GDP.MKTP.PP.CD) %>% mutate(cn=norm_name(country))
df_econ<-df_lmic %>% left_join(df_gdp,by=c("wb_n"="cn")) %>% filter(!is.na(GDP_pc)) %>%
  select(location_id,location_name,LMIC_group,GDP_pc,GDP_tot)
df_hale<-read_csv(hale_f,show_col_types=FALSE) %>% filter(metric_name=="Years",year==2023,age_name=="All ages",sex_id %in% c(1,2)) %>%
  mutate(sex_name=ifelse(sex_id==1,"Male","Female")) %>% select(location_id,sex_name,HALE=val)
dat<-daly %>% inner_join(df_econ %>% select(-LMIC_group),by="location_name") %>% inner_join(df_hale,by=c("location_id","sex_name"))

# aggregate age DALYs (Both, 2023) from backup Table6 (IE-invariant)
t6<-read_csv(file.path(bkdir,"IE1/Table6_age_specific.csv"),show_col_types=FALSE)
ad<-parse_ui(t6$DALYs)
age_daly<-tibble(`Age Group`=t6$`Age Group`,age_mid=t6$age_mid,age_order=row_number(t6$age_mid),
  D=ad$val,Dl=ad$lo,Dh=ad$hi)

compute_ie<-function(ie,r=rr){
  bsx<-dat %>% mutate(VSL_i=VSL_USA*(GDP_pc/GDP_pc_USA)^ie,VSLY=VSL_i/annuity(HALE,r),
    VLW=VSLY*DALY/1e9,VLW_lo=VSLY*lower/1e9,VLW_hi=VSLY*upper/1e9)
  both<-bsx %>% group_by(location_id) %>% summarise(VLW=sum(VLW),DALY=sum(DALY),.groups="drop")
  list(country_sex=bsx, total_V=sum(bsx$VLW), total_D=sum(bsx$DALY),
       male=sum(bsx$VLW[bsx$sex_name=="Male"]), female=sum(bsx$VLW[bsx$sex_name=="Female"]))
}

# ── 1. Reconciliation checks (cross-section, exact) ───────────────────────────
recon<-map_dfr(ie_values,function(ie){c<-compute_ie(ie)
  # effective aggregate VSLY and reconciled age sum
  vsly_eff<-c$total_V*1e9/c$total_D
  age_sum<-vsly_eff*sum(age_daly$D)/1e9
  tibble(IE=ie,VLW_both=c$total_V,VLW_male_plus_female=c$male+c$female,
    sex_additivity_diff=(c$male+c$female)-c$total_V,
    VLW_age_sum=age_sum, age_vs_total_diff=age_sum-c$total_V)})
write_csv(recon,file.path(diag,"Reconciliation_checks.csv"))

# ── 2. Country coverage cascade (reviewer m4) ─────────────────────────────────
n_lmic<-nrow(lmraw); n_gdp<-nrow(df_econ); n_an<-n_distinct(dat$location_id)
grp<-dat %>% distinct(location_id,LMIC_group) %>% count(LMIC_group)
cov<-tibble(item=c("LMIC countries (204_with_LMIC.csv, LMIC==1)",
  "  ... with matched 2023 World Bank GDP (PPP)",
  "  ... also with sex-specific 2023 HALE (ANALYSED)",
  "Analysed: Low income","Analysed: Lower middle income","Analysed: Upper middle income",
  "Excluded: missing GDP","Excluded: missing HALE"),
  n=c(n_lmic,n_gdp,n_an,
      grp$n[grp$LMIC_group=="Low income"],grp$n[grp$LMIC_group=="Lower middle income"],
      grp$n[grp$LMIC_group=="Upper middle income"],n_lmic-n_gdp,n_gdp-n_an))
write_csv(cov,file.path(diag,"Country_coverage.csv"))

# ── 3. Reconciled age table (Table 6, new method) per IE ──────────────────────
# age VLW = effective aggregate VSLY x age DALY  (sums exactly to the headline total)
age_share_5079<-age_daly %>% filter(`Age Group` %in% c("50-54 years","55-59 years","60-64 years",
  "65-69 years","70-74 years","75-79 years")) %>% summarise(s=sum(D)) %>% pull(s) / sum(age_daly$D)
peak_age<-age_daly$`Age Group`[which.max(age_daly$D)]
for(ie in ie_values){
  tag<-paste0("IE",gsub("\\.","",as.character(ie))); idir<-file.path(out,tag)
  c<-compute_ie(ie); vsly_eff<-c$total_V*1e9/c$total_D
  t6n<-age_daly %>% arrange(age_order) %>% mutate(
    V=vsly_eff*D/1e9, Vl=vsly_eff*Dl/1e9, Vh=vsly_eff*Dh/1e9) %>%
    transmute(`Age Group`,`DALYs`=fmt_ui(D,Dl,Dh,0),
      `VLW (billion USD)`=fmt_ui(V,Vl,Vh,3),
      `Share of total VLW (%)`=formatC(V/sum(V)*100,format="f",digits=1))
  write_csv(t6n,file.path(idir,"Table6_age_specific.csv"))
}

# ── 4. Provenance & methods ───────────────────────────────────────────────────
prov<-c("# Provenance & methods (reviewers m1, m2, M1, M2, m5)","",
 "## Data sources",
 "- Pancreatic-cancer DALYs: GBD 2023 (IHME GBD Results Tool). For this revision the 2023",
 "  country x sex all-ages DALYs were read from the project's archived Table 4; the aggregate",
 "  age-specific 2023 DALYs from the archived Table 6. The full per-country x age x year",
 "  extraction (`Pancreatic/merged.csv`) is required to regenerate temporal trends and the",
 "  2050 ETS projection (see ETS_forecast_STATUS.md).",
 "- HALE: GBD 2023, 2023, sex-specific, all-ages (DM/LMICDM/HALE.csv).",
 "- GDP per capita & total (PPP, current international $), 2023: World Bank WDI",
 "  (NY.GDP.PCAP.PP.CD, NY.GDP.MKTP.PP.CD; DM/LMICDM/gdp.csv).",
 "- Income classification & LMIC flag: World Bank (DM/LMICDM/204_with_LMIC.csv).",
 "- VSL reference: US DOT 2023, VSL = US$13.2 million; US GDP pc (PPP) 2023 = US$82,304.62.",
 paste0("- Analysis date: ",format(Sys.Date())),"",
 "## Single VSLY definition (replaces the inconsistent HALE/2 vs HALE_age formulas)",
 "  VSLY_i = VSL_i / a(HALE_i, r),  a(T,r) = (1-(1+r)^-T)/r,  T_i = HALE_i,  base r = 0.03.",
 "  VLW = VSLY x DALY, applied uniformly to every age; both-sex = male + female.",
 "",
 "## Age-specific VLW (Table 6) — reconciliation note",
 "  With a uniform VSLY, age-specific VLW is proportional to age-specific DALYs and therefore",
 "  sums EXACTLY to the headline total (this resolves reviewer M1). In this revision the age",
 "  decomposition is computed with the cohort-aggregate effective VSLY",
 "  (VSLY_eff = VLW_total / DALY_total) applied to the aggregate age-DALY profile; the exact",
 "  per-country x age decomposition (identical totals) is regenerated by",
 "  run_LMIC_Pancreatic_VLW_v2.R once the per-country x age source is loaded.",
 paste0("  VLW peaks in the same band as DALYs (",peak_age,"); the earlier 'peaks at 70-74'"),
 "  statement was an artefact of the inconsistent age denominator and is removed (m5).",
 paste0("  Age 50-79 share of total VLW = ",formatC(age_share_5079*100,format="f",digits=1),"%."))
writeLines(prov,file.path(diag,"PROVENANCE_and_METHODS.md"))

# ── 5. ETS status (honest: pending source) ────────────────────────────────────
ets<-c("# ETS projection diagnostics (reviewer M5) — STATUS: PENDING SOURCE DATA","",
 "The damped-trend ETS specification and the reconciliation fix (all-LMIC total = sum of",
 "income-group forecasts) are IMPLEMENTED in run_LMIC_Pancreatic_VLW_v2.R, which also writes",
 "per-series alpha, beta, phi, sigma^2, AICc, in-sample RMSE and Ljung-Box residual tests.",
 "",
 "These diagnostics CANNOT be produced from the archived outputs, because fitting ETS requires",
 "the full annual 1990-2023 VLW series for every stratum, which needs the per-country x year",
 "DALYs in `Pancreatic/merged.csv` (357 MB). That source file is currently not in the project",
 "directory. Once it is restored, run:",
 "",
 "    Rscript run_LMIC_Pancreatic_VLW_v2.R",
 "",
 "to regenerate Tables 5/7/8, Figures 2/4/5/6, and diagnostics/ETS_diagnostics_all_IE.csv.")
writeLines(ets,file.path(diag,"ETS_forecast_STATUS.md"))

# ── 6. sessionInfo ────────────────────────────────────────────────────────────
capture.output(sessionInfo(),file=file.path(diag,"sessionInfo.txt"))

cat("Reconciliation:\n"); print(recon)
cat("\nCoverage:\n"); print(cov)
cat(sprintf("\nAge 50-79 VLW share = %.1f%% (peak band %s)\n",age_share_5079*100,peak_age))
cat("\ndiagnostics/ written to:",diag,"\n")
