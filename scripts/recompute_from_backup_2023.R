################################################################################
# Partial recompute (2023 all-ages cross-section) from PRESERVED DALYs
# Source of DALYs: results_VLW_backup_2026-06-12/IE1/Table4_country_full.csv
#   (country x sex, all-ages 2023; DALYs are IE-invariant, so one copy suffices)
# Valuation: NEW consistent uniform VSLY  =  VSL_i / annuity(HALE_i, r)
# Produces corrected Tables 1-4 + Kruskal-Wallis + headline + IE range.
# (Age / trend / forecast need per-country age & annual DALYs -> require source merged.csv.)
################################################################################
suppressMessages(library(tidyverse))
options(warn=-1, dplyr.summarise.inform=FALSE)

base       <- normalizePath(".")
bk_t4      <- file.path(base,"data_raw/backup_results_2026-06-12/IE1/Table4_country_full.csv")
hale_file  <- file.path(base,"data_raw/external_metadata/HALE.csv")
gdp_file   <- file.path(base,"data_raw/external_metadata/gdp.csv")
lmic_file  <- file.path(base,"data_raw/external_metadata/204_with_LMIC.csv")
out_dir    <- file.path(base,"outputs/results_VLW/v2_partial_2023")
dir.create(out_dir, showWarnings=FALSE, recursive=TRUE)

VSL_USA<-13.2e6; GDP_pc_USA<-82304.62; discount_rate<-0.03
ie_values<-c(0.5,1.0,1.5); income_fct<-c("Low income","Lower middle income","Upper middle income")
annuity<-function(T,r) ifelse(T<=0,NA_real_,ifelse(r==0,T,(1-(1+r)^(-T))/r))
fmt_ui<-function(v,lo,hi,d=1,s=1) paste0(formatC(v/s,format="f",digits=d,big.mark=",")," (",
  formatC(lo/s,format="f",digits=d,big.mark=","),"–",formatC(hi/s,format="f",digits=d,big.mark=","),")")

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

# ── 1. Parse preserved DALYs (country x sex, 2023) ────────────────────────────
parse_ui<-function(col){
  s<-str_replace_all(col,",","")
  m<-str_match(s,"^\\s*([0-9.]+)\\s*\\(\\s*([0-9.]+)\\s*[–-]\\s*([0-9.]+)\\s*\\)")
  tibble(val=as.numeric(m[,2]),lo=as.numeric(m[,3]),hi=as.numeric(m[,4]))
}
t4<-read_csv(bk_t4,show_col_types=FALSE)
dchk<-parse_ui(t4$DALYs)
daly<-tibble(location_name=t4$Country, sex_name=t4$Sex, LMIC_group=t4$`Income Group`,
             DALY=dchk$val, lower=dchk$lo, upper=dchk$hi) %>%
  filter(sex_name %in% c("Male","Female"))   # derive Both = M+F later
cat("Parsed DALYs rows (M/F):",nrow(daly)," NA:",sum(is.na(daly$DALY)),"\n")

# ── 2. Map to location_id, join GDP + sex-specific HALE ────────────────────────
df_lmic<-read_csv(lmic_file,show_col_types=FALSE) %>% filter(LMIC==1) %>%
  select(location_id,location_name,LMIC_group) %>%
  mutate(wb=ifelse(location_name %in% names(gbd_to_wb),gbd_to_wb[location_name],location_name),wb_n=norm_name(wb))
df_gdp<-read_csv(gdp_file,show_col_types=FALSE) %>%
  filter(year==2023,!is.na(NY.GDP.PCAP.PP.CD),!is.na(NY.GDP.MKTP.PP.CD)) %>%
  select(country,GDP_pc=NY.GDP.PCAP.PP.CD,GDP_tot=NY.GDP.MKTP.PP.CD) %>% mutate(cn=norm_name(country))
df_econ<-df_lmic %>% left_join(df_gdp,by=c("wb_n"="cn")) %>% filter(!is.na(GDP_pc)) %>%
  select(location_id,location_name,LMIC_group,GDP_pc,GDP_tot)
df_hale<-read_csv(hale_file,show_col_types=FALSE) %>%
  filter(metric_name=="Years",year==2023,age_name=="All ages",sex_id %in% c(1,2)) %>%
  mutate(sex_name=ifelse(sex_id==1,"Male","Female")) %>% select(location_id,sex_name,HALE=val)

dat<-daly %>% inner_join(df_econ %>% select(-LMIC_group),by="location_name") %>%
  inner_join(df_hale,by=c("location_id","sex_name"))
cat("Countries matched (GDP+HALE):",n_distinct(dat$location_id),"\n\n")

# ── 3. Compute consistent VLW for each IE ─────────────────────────────────────
compute_ie<-function(ie,r=discount_rate){
  base_sx<-dat %>% mutate(
    VSL_i=VSL_USA*(GDP_pc/GDP_pc_USA)^ie,
    VSLY=VSL_i/annuity(HALE,r), VSLY_u=VSL_i/HALE,
    VLW=VSLY*DALY/1e9, VLW_lo=VSLY*lower/1e9, VLW_hi=VSLY*upper/1e9,
    VLW_u=VSLY_u*DALY/1e9, VLW_u_lo=VSLY_u*lower/1e9, VLW_u_hi=VSLY_u*upper/1e9)
  both<-base_sx %>% group_by(location_id,location_name,LMIC_group,GDP_pc,GDP_tot) %>%
    summarise(DALY=sum(DALY),lower=sum(lower),upper=sum(upper),
      VLW=sum(VLW),VLW_lo=sum(VLW_lo),VLW_hi=sum(VLW_hi),
      VLW_u=sum(VLW_u),VLW_u_lo=sum(VLW_u_lo),VLW_u_hi=sum(VLW_u_hi),HALE=mean(HALE),.groups="drop") %>%
    mutate(sex_name="Both")
  bind_rows(base_sx %>% select(names(both)), both) %>%
    mutate(pct=VLW*1e9/GDP_tot*100,pct_lo=VLW_lo*1e9/GDP_tot*100,pct_hi=VLW_hi*1e9/GDP_tot*100)
}

ie_headline<-list()
for(ie in ie_values){
  ie_tag<-paste0("IE",gsub("\\.","",as.character(ie))); ie_dir<-file.path(out_dir,ie_tag)
  dir.create(ie_dir,showWarnings=FALSE,recursive=TRUE)
  df<-compute_ie(ie); d23<-df %>% filter(sex_name=="Both"); d23_all<-df

  mk_sum<-function(data,gv=NULL){ g<-if(!is.null(gv)) data %>% group_by(across(all_of(gv))) else data
    g %>% summarise(N=n(),D=sum(DALY),Dl=sum(lower),Dh=sum(upper),V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi),
      Vu=sum(VLW_u),Vul=sum(VLW_u_lo),Vuh=sum(VLW_u_hi),
      Wp=weighted.mean(pct,GDP_tot),Wpl=weighted.mean(pct_lo,GDP_tot),Wph=weighted.mean(pct_hi,GDP_tot),mH=mean(HALE),.groups="drop") }
  t1<-bind_rows(mk_sum(d23,"LMIC_group") %>% arrange(factor(LMIC_group,levels=income_fct)),
                mk_sum(d23) %>% mutate(LMIC_group="All LMICs")) %>%
    transmute(`Income Group`=LMIC_group,Countries=N,`DALYs (thousands)`=fmt_ui(D,Dl,Dh,1,1e3),
      `VLW (billion USD) [DALY 95% UI]`=fmt_ui(V,Vl,Vh,2),
      `VLW undiscounted r=0 (billion USD)`=fmt_ui(Vu,Vul,Vuh,2),
      `VLW/GDP (%) [DALY 95% UI]`=fmt_ui(Wp,Wpl,Wph,4),`Mean HALE`=formatC(mH,format="f",digits=1))
  write_csv(t1,file.path(ie_dir,"Table1_income_summary.csv"))

  t2<-d23_all %>% group_by(sex_name,LMIC_group) %>%
    summarise(N=n(),D=sum(DALY),Dl=sum(lower),Dh=sum(upper),V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi),
      Vu=sum(VLW_u),Vul=sum(VLW_u_lo),Vuh=sum(VLW_u_hi),.groups="drop") %>%
    transmute(Sex=sex_name,`Income Group`=LMIC_group,Countries=N,`DALYs (thousands)`=fmt_ui(D,Dl,Dh,1,1e3),
      `VLW (billion USD)`=fmt_ui(V,Vl,Vh,2),`VLW undiscounted (billion USD)`=fmt_ui(Vu,Vul,Vuh,2))
  write_csv(t2,file.path(ie_dir,"Table2_sex_income.csv"))

  t3<-d23 %>% arrange(desc(pct)) %>% mutate(Rank=row_number()) %>%
    transmute(Rank,Country=location_name,`Income Group`=LMIC_group,
      `GDP pc (PPP)`=formatC(GDP_pc,format="f",digits=0,big.mark=","),
      `DALYs (thousands)`=fmt_ui(DALY,lower,upper,1,1e3),`VLW (billion USD)`=fmt_ui(VLW,VLW_lo,VLW_hi,3),
      `VLW/GDP (%)`=fmt_ui(pct,pct_lo,pct_hi,4))
  write_csv(t3,file.path(ie_dir,"Table3_all_LMIC_countries.csv"))

  t4o<-d23_all %>% transmute(Country=location_name,Sex=sex_name,`Income Group`=LMIC_group,
    `DALYs`=fmt_ui(DALY,lower,upper,0),`VLW (billion USD)`=fmt_ui(VLW,VLW_lo,VLW_hi,3),
    `VLW undiscounted (billion USD)`=fmt_ui(VLW_u,VLW_u_lo,VLW_u_hi,3),`VLW/GDP (%)`=fmt_ui(pct,pct_lo,pct_hi,4))
  write_csv(t4o,file.path(ie_dir,"Table4_country_full.csv"))

  # Kruskal-Wallis (M4)
  kdat<-d23 %>% mutate(grp=factor(LMIC_group,levels=income_fct))
  kt<-kruskal.test(pct~grp,data=kdat); n_k<-nrow(kdat); k_g<-length(unique(kdat$grp))
  ph<-suppressWarnings(pairwise.wilcox.test(kdat$pct,kdat$grp,p.adjust.method="bonferroni"))
  write_csv(tibble(test="Kruskal-Wallis VLW/GDP across income groups",H=unname(kt$statistic),
    df=unname(kt$parameter),p_value=kt$p.value,n=n_k,epsilon2=unname(kt$statistic)/(n_k-1),
    note="income-defined groups; descriptive (circularity acknowledged)"),
    file.path(ie_dir,"Stats_KruskalWallis.csv"))
  write_csv(as.data.frame(ph$p.value) %>% rownames_to_column("group"),
    file.path(ie_dir,"Stats_KW_posthoc.csv"))

  # Reconciliation: sex additivity (M6) + age sums to total (M1, by construction)
  sexsum<-sum(d23_all %>% filter(sex_name %in% c("Male","Female")) %>% pull(VLW))
  both_t<-sum(d23$VLW)
  ie_headline[[ie_tag]]<-tibble(IE=ie,VLW_both=both_t,VLW_male_plus_female=sexsum,
    sex_diff=sexsum-both_t, VLW_undisc=sum(d23$VLW_u),
    VLWGDP_wmean=weighted.mean(d23$pct,d23$GDP_tot),KW_p=kt$p.value)
  cat(sprintf("  %s: VLW(Both)=%.2f B | M+F=%.2f B (diff %.2e) | undisc=%.2f B | KW p=%.1e\n",
    ie_tag,both_t,sexsum,sexsum-both_t,sum(d23$VLW_u),kt$p.value))
}

hl<-bind_rows(ie_headline)
write_csv(hl,file.path(out_dir,"Headline_and_reconciliation.csv"))
# IE valuation-sensitivity range (M3)
rng<-hl %>% summarise(base_IE1=VLW_both[IE==1.0],low_IE15=VLW_both[IE==1.5],high_IE05=VLW_both[IE==0.5])
write_csv(rng,file.path(out_dir,"Valuation_sensitivity_range_IE.csv"))

cat("\n================ HEADLINE (NEW consistent method) ================\n")
print(hl)
cat("\nValuation-sensitivity range (IE 0.5-1.5), VLW Both 2023:\n"); print(rng)
cat("\nWritten to:",out_dir,"\n")
