################################################################################
# LMIC Pancreatic Cancer — Value of Lost Welfare (VLW) Analysis
# GBD 2023 · 1990-2023 · Forecast to 2050 · IE = 0.5, 1.0, 1.5
# Full output per IE: maps, trends, bars, age, sex, forecast
# Nature Medicine–style
################################################################################

library(tidyverse)
library(sf)
library(patchwork)
library(scales)
library(forecast)

options(warn = -1, dplyr.summarise.inform = FALSE)

# ── Paths ─────────────────────────────────────────────────────────────────────
# Reproducibility-package layout: run from reviseR2/code with
# Rscript scripts/run_LMIC_Pancreatic_VLW.R
base       <- normalizePath(".")
daly_file  <- file.path(base, "data_processed/merged.csv")
hale_file  <- file.path(base, "data_raw/external_metadata/HALE.csv")
gdp_file   <- file.path(base, "data_raw/external_metadata/gdp.csv")
lmic_file  <- file.path(base, "data_raw/external_metadata/204_with_LMIC.csv")
world_file <- file.path(base, "data_raw/external_metadata/df_world2.geojson")
out_root   <- file.path(base, "outputs/results_VLW")
dir.create(out_root, showWarnings = FALSE, recursive = TRUE)

# ── USA 2023 Reference ───────────────────────────────────────��───────────────
VSL_USA       <- 13.2e6
GDP_pc_USA    <- 82304.62
discount_rate <- 0.03
ie_values     <- c(0.5, 1.0, 1.5)

# ── Palette ───────────────────────────────────────────────────────────────────
pal <- list(
  blue="#1B4F72", red="#C0392B", teal="#117A65",
  orange="#E67E22", grey="#5D6D7E", purple="#6C3483", pink="#CB4335"
)
income_pal <- c("Low income"="#C0392B","Lower middle income"="#E67E22","Upper middle income"="#117A65")
sex_pal    <- c("Male"="#1B4F72","Female"="#C0392B","Both"="#5D6D7E")
income_fct <- c("Low income","Lower middle income","Upper middle income")

map_blue <- c("#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1","#6BAED6","#4292C6","#2171B5","#084594")
map_red  <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#99000D")

# ── Themes ────────────────────────────────────────────────────────────────────
theme_nm <- function(bs=7.5) {
  theme_minimal(base_size=bs) %+replace% theme(
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
}
theme_nm_map <- function(bs=7.5) {
  theme_void(base_size=bs) %+replace% theme(
    plot.title=element_blank(),
    plot.tag=element_text(size=10,face="bold",hjust=0),
    legend.position="right",
    legend.title=element_text(size=bs-0.5,face="bold"),
    legend.text=element_text(size=bs-1),
    legend.key.size=unit(0.3,"cm"),
    plot.margin=margin(2,2,2,2,"pt"))
}

# ── Helpers ───────────────────────────────────────────────────────────────────
gbd_to_wb <- c(
  "Bolivia (Plurinational State of)"="Bolivia","Congo"="Congo, Rep.",
  "Côte d'Ivoire"="Cote d'Ivoire",
  "Democratic People's Republic of Korea"="Korea, Dem. People's Rep.",
  "Democratic Republic of the Congo"="Congo, Dem. Rep.",
  "Egypt"="Egypt, Arab Rep.","Gambia"="Gambia, The",
  "Iran (Islamic Republic of)"="Iran, Islamic Rep.",
  "Kyrgyzstan"="Kyrgyz Republic",
  "Lao People's Democratic Republic"="Lao PDR",
  "Micronesia (Federated States of)"="Micronesia, Fed. Sts.",
  "Palestine"="West Bank and Gaza","Republic of Moldova"="Moldova",
  "Saint Lucia"="St. Lucia",
  "Saint Vincent and the Grenadines"="St. Vincent and the Grenadines",
  "Somalia"="Somalia, Fed. Rep.","Syrian Arab Republic"="Syrian Arab Republic",
  "Türkiye"="Turkiye","United Republic of Tanzania"="Tanzania",
  "Yemen"="Yemen, Rep.")

norm_name <- function(x) x %>% str_replace_all("[\u2018\u2019]","'") %>%
  str_replace_all("\u00a0"," ") %>% iconv(from="UTF-8",to="ASCII//TRANSLIT") %>% str_squish()

fmt_ui <- function(v,lo,hi,d=1,s=1) paste0(
  formatC(v/s,format="f",digits=d,big.mark=",")," (",
  formatC(lo/s,format="f",digits=d,big.mark=","),"\u2013",
  formatC(hi/s,format="f",digits=d,big.mark=","),")")

disc_factor <- function(yrs,r=0.03) ifelse(yrs<=0,0,(1-(1+r)^(-yrs))/(r*yrs))

std_ages <- c("<5 years","5-9 years","10-14 years","15-19 years",
  "20-24 years","25-29 years","30-34 years","35-39 years",
  "40-44 years","45-49 years","50-54 years","55-59 years",
  "60-64 years","65-69 years","70-74 years","75-79 years",
  "80-84 years","85-89 years","90-94 years","95+ years")
age_mids <- c(2.5,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87,92,97)
age_lookup <- tibble(age_name=std_ages, age_mid=age_mids, age_order=seq_along(std_ages))

cat("================================================================\n")
cat("  Pancreatic Cancer LMIC VLW · IE=0.5/1.0/1.5 · Full Pipeline\n")
cat("================================================================\n\n")

# ======================================================================
# 1. READ DATA
# ======================================================================
cat("[1] Reading data ...\n")

df_lmic <- read_csv(lmic_file,show_col_types=FALSE) %>% filter(LMIC==1) %>%
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
cat("   LMIC countries:", length(lmic_ids), "\n")

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

df_hale_age <- read_csv(hale_file,show_col_types=FALSE) %>%
  filter(metric_name=="Years",year==2023,age_name %in% std_ages) %>%
  select(location_id,sex_id,age_name,HALE_age=val)

df_world <- st_read(world_file,quiet=TRUE)
df_world$location_id <- as.numeric(df_world$location_id)

cat("   Data loaded.\n\n")

# ======================================================================
# 2. VLW COMPUTATION FUNCTION
# ======================================================================

compute_vlw_allages <- function(ie) {
  df_daly_all %>%
    left_join(df_econ,by=c("location_id","location_name")) %>%
    left_join(df_hale_all,by=c("location_id","sex_id")) %>%
    filter(!is.na(GDP_pc),!is.na(HALE),HALE>0) %>%
    mutate(
      IE=ie,
      VSL_i=VSL_USA*(GDP_pc/GDP_pc_USA)^ie,
      VSLY=VSL_i/(HALE/2),
      rem=HALE*0.4, dfac=disc_factor(rem,discount_rate),
      VLW=VSLY*DALY/1e9, VLW_lo=VSLY*lower/1e9, VLW_hi=VSLY*upper/1e9,
      VLW_d=VLW*dfac, VLW_d_lo=VLW_lo*dfac, VLW_d_hi=VLW_hi*dfac,
      pct=VLW*1e9/GDP_tot*100, pct_lo=VLW_lo*1e9/GDP_tot*100, pct_hi=VLW_hi*1e9/GDP_tot*100,
      pct_d=VLW_d*1e9/GDP_tot*100, pct_d_lo=VLW_d_lo*1e9/GDP_tot*100, pct_d_hi=VLW_d_hi*1e9/GDP_tot*100)
}

compute_vlw_age <- function(ie) {
  df_daly_age %>% filter(year==2023) %>%
    left_join(age_lookup,by="age_name") %>%
    left_join(df_hale_age,by=c("location_id","sex_id","age_name")) %>%
    left_join(df_econ,by="location_id") %>%
    filter(!is.na(GDP_pc),!is.na(HALE_age),HALE_age>0) %>%
    mutate(
      IE=ie,
      VSL_i=VSL_USA*(GDP_pc/GDP_pc_USA)^ie,
      VSLY_a=VSL_i/HALE_age, dfac=disc_factor(HALE_age,discount_rate),
      VLW_a=VSLY_a*DALY_age/1e9, VLW_a_lo=VSLY_a*lower_age/1e9, VLW_a_hi=VSLY_a*upper_age/1e9,
      VLW_a_d=VLW_a*dfac)
}

# ======================================================================
# 3. LOOP OVER IE VALUES — FULL OUTPUT PER IE
# ======================================================================

for (ie in ie_values) {
  ie_tag <- paste0("IE", gsub("\\.","",as.character(ie)))
  ie_lab <- format(ie, nsmall=1)
  ie_dir <- file.path(out_root, ie_tag)
  dir.create(ie_dir, showWarnings=FALSE, recursive=TRUE)

  cat("================================================================\n")
  cat("  Processing IE =", ie_lab, "\n")
  cat("================================================================\n")

  # ── Compute ──
  cat("  [A] Computing VLW ...\n")
  df <- compute_vlw_allages(ie)
  df_av <- compute_vlw_age(ie)
  d23 <- df %>% filter(year==2023, sex_name=="Both")
  d23_all <- df %>% filter(year==2023)

  # ════════════════════════════════════════════════════════════════════
  # TABLES
  # ════════════════════════════════════════════════════════════════════
  cat("  [B] Tables ...\n")

  # T1: income group summary
  mk_sum <- function(data, gv=NULL) {
    g <- if(!is.null(gv)) data %>% group_by(across(all_of(gv))) else data
    g %>% summarise(N=n(),
      D=sum(DALY),Dl=sum(lower),Dh=sum(upper),
      V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi),
      Vd=sum(VLW_d),Vdl=sum(VLW_d_lo),Vdh=sum(VLW_d_hi),
      Wp=weighted.mean(pct,GDP_tot),Wpl=weighted.mean(pct_lo,GDP_tot),
      Wph=weighted.mean(pct_hi,GDP_tot), mH=mean(HALE),.groups="drop")
  }
  t1 <- bind_rows(
    mk_sum(d23,"LMIC_group") %>% arrange(factor(LMIC_group,levels=income_fct)),
    mk_sum(d23) %>% mutate(LMIC_group="All LMICs")
  ) %>% transmute(`Income Group`=LMIC_group, Countries=N,
    `DALYs (thousands)`=fmt_ui(D,Dl,Dh,1,1e3),
    `VLW (billion USD)`=fmt_ui(V,Vl,Vh,2),
    `VLW discounted (billion USD)`=fmt_ui(Vd,Vdl,Vdh,2),
    `VLW/GDP (%)`=fmt_ui(Wp,Wpl,Wph,4),
    `Mean HALE`=formatC(mH,format="f",digits=1))
  write_csv(t1, file.path(ie_dir,"Table1_income_summary.csv"))

  # T2: sex × income
  t2 <- d23_all %>% group_by(sex_name,LMIC_group) %>%
    summarise(N=n(),D=sum(DALY),Dl=sum(lower),Dh=sum(upper),
      V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi),
      Vd=sum(VLW_d),Vdl=sum(VLW_d_lo),Vdh=sum(VLW_d_hi)) %>%
    transmute(Sex=sex_name,`Income Group`=LMIC_group,Countries=N,
      `DALYs (thousands)`=fmt_ui(D,Dl,Dh,1,1e3),
      `VLW (billion USD)`=fmt_ui(V,Vl,Vh,2),
      `VLW discounted (billion USD)`=fmt_ui(Vd,Vdl,Vdh,2))
  write_csv(t2, file.path(ie_dir,"Table2_sex_income.csv"))

  # T3: all LMIC countries
  t3 <- d23 %>% arrange(desc(pct)) %>% mutate(Rank=row_number()) %>%
    transmute(Rank,Country=location_name,`Income Group`=LMIC_group,
      `GDP pc (PPP)`=formatC(GDP_pc,format="f",digits=0,big.mark=","),
      `DALYs (thousands)`=fmt_ui(DALY,lower,upper,1,1e3),
      `VLW (billion USD)`=fmt_ui(VLW,VLW_lo,VLW_hi,3),
      `VLW/GDP (%)`=fmt_ui(pct,pct_lo,pct_hi,4))
  write_csv(t3, file.path(ie_dir,"Table3_all_LMIC_countries.csv"))

  # T4: full country × sex
  t4 <- d23_all %>% transmute(Country=location_name,Sex=sex_name,
    `Income Group`=LMIC_group,
    `DALYs`=fmt_ui(DALY,lower,upper,0),
    `VLW (billion USD)`=fmt_ui(VLW,VLW_lo,VLW_hi,3),
    `VLW discounted (billion USD)`=fmt_ui(VLW_d,VLW_d_lo,VLW_d_hi,3),
    `VLW/GDP (%)`=fmt_ui(pct,pct_lo,pct_hi,4))
  write_csv(t4, file.path(ie_dir,"Table4_country_full.csv"))

  # T5: temporal
  t5 <- df %>% filter(sex_name=="Both",year %in% c(1990,2000,2010,2023)) %>%
    group_by(LMIC_group,year) %>%
    summarise(D=sum(DALY),Dl=sum(lower),Dh=sum(upper),
      V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi)) %>%
    transmute(`Income Group`=LMIC_group,Year=year,
      `DALYs (thousands)`=fmt_ui(D,Dl,Dh,1,1e3),
      `VLW (billion USD)`=fmt_ui(V,Vl,Vh,2)) %>%
    arrange(factor(`Income Group`,levels=income_fct),Year)
  write_csv(t5, file.path(ie_dir,"Table5_temporal.csv"))

  # T6: age-specific VLW
  t6 <- df_av %>% filter(sex_name=="Both") %>%
    group_by(age_name,age_mid,age_order) %>%
    summarise(D=sum(DALY_age),Dl=sum(lower_age),Dh=sum(upper_age),
      V=sum(VLW_a),Vl=sum(VLW_a_lo),Vh=sum(VLW_a_hi)) %>%
    arrange(age_order) %>%
    transmute(`Age Group`=age_name,
      `DALYs`=fmt_ui(D,Dl,Dh,0),
      `VLW (billion USD)`=fmt_ui(V,Vl,Vh,3))
  write_csv(t6, file.path(ie_dir,"Table6_age_specific.csv"))

  cat("     Tables 1-6 saved.\n")

  # ════════════════════════════════════════════════════════════════════
  # FIGURE 1: 2023 SNAPSHOT (a-d)
  # ════════════════════════════════════════════════════════════════════
  cat("  [C] Figures ...\n")

  bar1 <- d23 %>% group_by(LMIC_group) %>%
    summarise(V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi)) %>%
    mutate(LMIC_group=factor(LMIC_group,levels=income_fct))

  p1a <- ggplot(bar1,aes(LMIC_group,V,fill=LMIC_group))+
    geom_col(width=0.6,colour="white",linewidth=0.3)+
    geom_errorbar(aes(ymin=Vl,ymax=Vh),width=0.12,linewidth=0.3)+
    scale_fill_manual(values=income_pal,guide="none")+
    scale_y_continuous(expand=expansion(mult=c(0,0.1)))+
    scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+
    labs(x=NULL,y="VLW (billion USD)")+theme_nm()

  p1b <- ggplot(d23,aes(factor(LMIC_group,income_fct),pct,fill=LMIC_group))+
    geom_boxplot(width=0.5,outlier.shape=NA,alpha=0.5,linewidth=0.3)+
    geom_jitter(aes(colour=LMIC_group),width=0.12,size=0.6,alpha=0.5)+
    scale_fill_manual(values=income_pal,guide="none")+
    scale_colour_manual(values=income_pal,guide="none")+
    scale_y_continuous(labels=\(x) paste0(x,"%"))+
    scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+
    labs(x=NULL,y="VLW / GDP (%)")+theme_nm()

  sex_bar <- df %>% filter(year==2023,sex_name %in% c("Male","Female")) %>%
    group_by(sex_name,LMIC_group) %>%
    summarise(V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi)) %>%
    mutate(LMIC_group=factor(LMIC_group,levels=income_fct))

  p1c <- ggplot(sex_bar,aes(LMIC_group,V,fill=sex_name))+
    geom_col(position=position_dodge(0.65),width=0.55,colour="white",linewidth=0.2)+
    geom_errorbar(aes(ymin=Vl,ymax=Vh),position=position_dodge(0.65),width=0.12,linewidth=0.25)+
    scale_fill_manual(values=sex_pal,name="Sex")+
    scale_y_continuous(expand=expansion(mult=c(0,0.1)))+
    scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+
    labs(x=NULL,y="VLW (billion USD)")+theme_nm()+theme(legend.position=c(0.15,0.85))

  p1d <- ggplot(d23,aes(GDP_pc,pct,colour=LMIC_group,size=DALY))+
    geom_point(alpha=0.6,stroke=0.2)+
    scale_x_log10(labels=dollar_format(),breaks=c(1000,3000,10000,30000))+
    scale_y_continuous(labels=\(x) paste0(x,"%"))+
    scale_colour_manual(values=income_pal,name="Income Group")+
    scale_size_continuous(name="DALYs",range=c(1.5,9),
      breaks=c(5000,20000,100000,500000),labels=c("5K","20K","100K","500K"))+
    labs(x="GDP per capita (PPP, USD)",y="VLW / GDP (%)")+theme_nm()+
    guides(colour=guide_legend(order=1,override.aes=list(size=2.5)),size=guide_legend(order=2))

  panel1 <- (p1a|p1b)/(p1c|p1d)+
    plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))
  pdf(file.path(ie_dir,"Figure1_2023_snapshot.pdf"),width=8.5,height=7); print(panel1); dev.off()

  # ════════════════════════════════════════════════════════════════════
  # FIGURE 2: TEMPORAL TRENDS (a-d)
  # ════════════════════════════════════════════════════════════════════

  tot_t <- df %>% filter(sex_name=="Both") %>% group_by(year) %>%
    summarise(V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi))

  p2a <- ggplot(tot_t,aes(year,V))+
    geom_ribbon(aes(ymin=Vl,ymax=Vh),fill=pal$blue,alpha=0.12)+
    geom_line(colour=pal$blue,linewidth=0.7)+
    scale_x_continuous(breaks=seq(1990,2023,5))+
    scale_y_continuous(expand=expansion(mult=c(0,0.05)))+
    labs(x="Year",y="Total VLW (billion USD)")+theme_nm()

  inc_t <- df %>% filter(sex_name=="Both") %>% group_by(LMIC_group,year) %>%
    summarise(V=sum(VLW)) %>% mutate(LMIC_group=factor(LMIC_group,levels=income_fct))

  p2b <- ggplot(inc_t,aes(year,V,colour=LMIC_group))+
    geom_line(linewidth=0.6)+
    geom_point(data=inc_t %>% filter(year %in% c(1990,2000,2010,2023)),size=1.2)+
    scale_colour_manual(values=income_pal,name="Income Group")+
    scale_x_continuous(breaks=seq(1990,2023,5))+
    labs(x="Year",y="VLW (billion USD)")+theme_nm()+
    theme(legend.position=c(0.28,0.78))

  daly_t <- df %>% filter(sex_name=="Both") %>% group_by(LMIC_group,year) %>%
    summarise(D=sum(DALY)/1e6) %>% mutate(LMIC_group=factor(LMIC_group,levels=income_fct))

  p2c <- ggplot(daly_t,aes(year,D,colour=LMIC_group))+
    geom_line(linewidth=0.6)+
    scale_colour_manual(values=income_pal,name="Income Group")+
    scale_x_continuous(breaks=seq(1990,2023,5))+
    labs(x="Year",y="DALYs (millions)")+theme_nm()+
    theme(legend.position=c(0.28,0.78))

  sex_t <- df %>% filter(sex_name %in% c("Male","Female")) %>%
    group_by(sex_name,year) %>% summarise(V=sum(VLW))

  p2d <- ggplot(sex_t,aes(year,V,colour=sex_name))+
    geom_line(linewidth=0.6)+
    scale_colour_manual(values=sex_pal,name="Sex")+
    scale_x_continuous(breaks=seq(1990,2023,5))+
    labs(x="Year",y="VLW (billion USD)")+theme_nm()+
    theme(legend.position=c(0.2,0.82))

  panel2 <- (p2a|p2b)/(p2c|p2d)+
    plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))
  pdf(file.path(ie_dir,"Figure2_trends.pdf"),width=9,height=7); print(panel2); dev.off()

  # ════════════════════════════════════════════════════════════════════
  # FIGURE 3: MAPS (a-c)
  # ════════════════════════════════════════════════════════════════════

  md <- d23 %>% select(location_id,VLW,pct,LMIC_group)
  w <- df_world %>% left_join(md,by="location_id")

  p3a <- ggplot(w)+geom_sf(aes(fill=VLW),colour="grey40",linewidth=0.06)+
    scale_fill_gradientn(colours=map_blue,na.value="grey92",trans="log1p",
      breaks=c(0,0.01,0.1,1,10,100),labels=c("0","0.01","0.1","1","10","100"),
      name="VLW\n(billion)")+theme_nm_map()

  p3b <- ggplot(w)+geom_sf(aes(fill=pct),colour="grey40",linewidth=0.06)+
    scale_fill_gradientn(colours=map_red,na.value="grey92",trans="log1p",
      breaks=c(0,0.01,0.05,0.1,0.5,1),labels=c("0","0.01","0.05","0.1","0.5","1"),
      name="VLW/GDP\n(%)")+theme_nm_map()

  w2 <- df_world %>% left_join(d23 %>% select(location_id,LMIC_group),by="location_id")
  p3c <- ggplot(w2)+geom_sf(aes(fill=LMIC_group),colour="grey40",linewidth=0.06)+
    scale_fill_manual(values=income_pal,na.value="grey92",name="Income Group")+theme_nm_map()

  panel3 <- p3a/p3b/p3c+
    plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))
  pdf(file.path(ie_dir,"Figure3_maps.pdf"),width=8,height=11); print(panel3); dev.off()

  # ════════════════════════════════════════════════════════════════════
  # FIGURE 4: AGE-SPECIFIC (a-d)
  # ════════════════════════════════════════════════════════════════════

  age_all <- df_av %>% filter(sex_name=="Both") %>%
    group_by(age_name,age_mid,age_order) %>%
    summarise(V=sum(VLW_a),Vl=sum(VLW_a_lo),Vh=sum(VLW_a_hi)) %>% arrange(age_order)

  p4a <- ggplot(age_all,aes(age_mid,V))+
    geom_ribbon(aes(ymin=Vl,ymax=Vh),fill=pal$teal,alpha=0.18)+
    geom_line(colour=pal$teal,linewidth=0.7)+geom_point(colour=pal$teal,size=1.2)+
    scale_x_continuous(breaks=seq(0,100,10))+
    labs(x="Age (years)",y="VLW (billion USD)")+theme_nm()

  age_daly <- df_daly_age %>% filter(year==2023,sex_name=="Both",location_id %in% lmic_ids) %>%
    left_join(age_lookup,by="age_name") %>%
    group_by(age_name,age_mid,age_order) %>%
    summarise(D=sum(DALY_age)/1e3,Dl=sum(lower_age)/1e3,Dh=sum(upper_age)/1e3) %>% arrange(age_order)

  p4b <- ggplot(age_daly,aes(age_mid,D))+
    geom_ribbon(aes(ymin=Dl,ymax=Dh),fill=pal$purple,alpha=0.15)+
    geom_line(colour=pal$purple,linewidth=0.7)+geom_point(colour=pal$purple,size=1.2)+
    scale_x_continuous(breaks=seq(0,100,10))+
    labs(x="Age (years)",y="DALYs (thousands)")+theme_nm()

  age_sex <- df_av %>% filter(sex_name %in% c("Male","Female")) %>%
    group_by(sex_name,age_name,age_mid,age_order) %>%
    summarise(V=sum(VLW_a)) %>% arrange(age_order)

  p4c <- ggplot(age_sex,aes(age_mid,V,colour=sex_name))+
    geom_line(linewidth=0.6)+geom_point(size=1)+
    scale_colour_manual(values=sex_pal,name="Sex")+
    scale_x_continuous(breaks=seq(0,100,10))+
    labs(x="Age (years)",y="VLW (billion USD)")+theme_nm()+
    theme(legend.position=c(0.15,0.85))

  age_inc <- df_av %>% filter(sex_name=="Both") %>%
    group_by(LMIC_group,age_name,age_mid,age_order) %>%
    summarise(V=sum(VLW_a)) %>% arrange(age_order) %>%
    mutate(LMIC_group=factor(LMIC_group,levels=income_fct))

  p4d <- ggplot(age_inc,aes(age_mid,V,colour=LMIC_group))+
    geom_line(linewidth=0.6)+geom_point(size=1)+
    scale_colour_manual(values=income_pal,name="Income Group")+
    scale_x_continuous(breaks=seq(0,100,10))+
    labs(x="Age (years)",y="VLW (billion USD)")+theme_nm()+
    theme(legend.position=c(0.2,0.78))

  panel4 <- (p4a|p4b)/(p4c|p4d)+
    plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))
  pdf(file.path(ie_dir,"Figure4_age_specific.pdf"),width=9,height=7); print(panel4); dev.off()

  # ════════════════════════════════════════════════════════════════════
  # FIGURE 5: FORECAST TO 2050 (a-d)
  # ════════════════════════════════════════════════════════════════════
  cat("  [D] Forecasting to 2050 ...\n")
  fh <- 2050 - 2023

  # total
  fc_tot <- df %>% filter(sex_name=="Both") %>% group_by(year) %>%
    summarise(V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi),
              D=sum(DALY),Dl=sum(lower),Dh=sum(upper))

  ts_v <- ts(fc_tot$V,start=1990,frequency=1)
  pv <- forecast(ets(ts_v,model="AAN",damped=TRUE),h=fh,level=95)
  ts_d <- ts(fc_tot$D,start=1990,frequency=1)
  pd <- forecast(ets(ts_d,model="AAN",damped=TRUE),h=fh,level=95)

  fc_all <- bind_rows(
    fc_tot %>% mutate(type="Observed"),
    tibble(year=2024:2050,V=as.numeric(pv$mean),
           Vl=as.numeric(pv$lower[,1]),Vh=as.numeric(pv$upper[,1]),
           D=as.numeric(pd$mean),Dl=as.numeric(pd$lower[,1]),Dh=as.numeric(pd$upper[,1]),
           type="Forecast"))

  # by income
  fc_inc_raw <- df %>% filter(sex_name=="Both") %>%
    group_by(LMIC_group,year) %>% summarise(V=sum(VLW),D=sum(DALY))

  fc_inc <- fc_inc_raw %>% group_by(LMIC_group) %>% group_modify(~{
    tv <- ts(.x$V,start=1990,frequency=1)
    fv <- forecast(ets(tv,model="AAN",damped=TRUE),h=fh,level=95)
    td <- ts(.x$D,start=1990,frequency=1)
    fd <- forecast(ets(td,model="AAN",damped=TRUE),h=fh,level=95)
    bind_rows(
      .x %>% mutate(Vl=NA_real_,Vh=NA_real_,Dl=NA_real_,Dh=NA_real_,type="Observed"),
      tibble(year=2024:2050,V=as.numeric(fv$mean),
             Vl=as.numeric(fv$lower[,1]),Vh=as.numeric(fv$upper[,1]),
             D=as.numeric(fd$mean),Dl=as.numeric(fd$lower[,1]),Dh=as.numeric(fd$upper[,1]),
             type="Forecast"))
  }) %>% ungroup()

  # by sex
  fc_sex_raw <- df %>% filter(sex_name %in% c("Male","Female")) %>%
    group_by(sex_name,year) %>% summarise(V=sum(VLW))

  fc_sex <- fc_sex_raw %>% group_by(sex_name) %>% group_modify(~{
    tv <- ts(.x$V,start=1990,frequency=1)
    fv <- forecast(ets(tv,model="AAN",damped=TRUE),h=fh,level=95)
    bind_rows(
      .x %>% mutate(Vl=NA_real_,Vh=NA_real_,type="Observed"),
      tibble(year=2024:2050,V=as.numeric(fv$mean),
             Vl=as.numeric(fv$lower[,1]),Vh=as.numeric(fv$upper[,1]),
             type="Forecast"))
  }) %>% ungroup()

  # Forecast tables
  fc_yrs <- c(2025,2030,2035,2040,2045,2050)

  t7a <- fc_all %>% filter(year %in% fc_yrs) %>%
    transmute(Year=year,Group="All LMICs",
      `VLW (billion USD, 95% CI)`=fmt_ui(V,Vl,Vh,2),
      `DALYs (thousands, 95% CI)`=fmt_ui(D,Dl,Dh,1,1e3))

  t7b <- fc_inc %>% filter(year %in% fc_yrs,type=="Forecast") %>%
    transmute(Year=year,Group=LMIC_group,
      `VLW (billion USD, 95% CI)`=fmt_ui(V,Vl,Vh,2),
      `DALYs (thousands, 95% CI)`=fmt_ui(D,Dl,Dh,1,1e3))

  t7 <- bind_rows(t7a,t7b) %>% arrange(Year,Group)
  write_csv(t7, file.path(ie_dir,"Table7_forecast_summary.csv"))

  t8 <- fc_all %>% filter(type=="Forecast") %>%
    transmute(Year=year,
      `VLW (billion USD, 95% CI)`=fmt_ui(V,Vl,Vh,2),
      `DALYs (thousands, 95% CI)`=fmt_ui(D,Dl,Dh,1,1e3))
  write_csv(t8, file.path(ie_dir,"Table8_forecast_annual.csv"))

  cat("     Tables 7-8 saved.\n")

  # ── Forecast plots ──
  p5a <- ggplot(fc_all,aes(year,V))+
    geom_ribbon(data=fc_all %>% filter(type=="Observed"),
      aes(ymin=Vl,ymax=Vh),fill=pal$blue,alpha=0.1)+
    geom_ribbon(data=fc_all %>% filter(type=="Forecast"),
      aes(ymin=Vl,ymax=Vh),fill=pal$red,alpha=0.12)+
    geom_line(data=fc_all %>% filter(type=="Observed"),colour=pal$blue,linewidth=0.7)+
    geom_line(data=fc_all %>% filter(type=="Forecast"),colour=pal$red,linewidth=0.7)+
    geom_vline(xintercept=2023,linetype="dashed",colour="grey50",linewidth=0.3)+
    annotate("text",x=2024,y=max(fc_all$Vh,na.rm=TRUE)*0.95,
      label="Forecast \u2192",hjust=0,size=2.2,colour="grey40")+
    scale_x_continuous(breaks=seq(1990,2050,10))+
    scale_y_continuous(expand=expansion(mult=c(0,0.05)))+
    labs(x="Year",y="Total VLW (billion USD)")+theme_nm()

  p5b <- ggplot(fc_all,aes(year,D/1e6))+
    geom_ribbon(data=fc_all %>% filter(type=="Observed"),
      aes(ymin=Dl/1e6,ymax=Dh/1e6),fill=pal$purple,alpha=0.1)+
    geom_ribbon(data=fc_all %>% filter(type=="Forecast"),
      aes(ymin=Dl/1e6,ymax=Dh/1e6),fill=pal$orange,alpha=0.12)+
    geom_line(data=fc_all %>% filter(type=="Observed"),colour=pal$purple,linewidth=0.7)+
    geom_line(data=fc_all %>% filter(type=="Forecast"),colour=pal$orange,linewidth=0.7)+
    geom_vline(xintercept=2023,linetype="dashed",colour="grey50",linewidth=0.3)+
    scale_x_continuous(breaks=seq(1990,2050,10))+
    scale_y_continuous(expand=expansion(mult=c(0,0.05)))+
    labs(x="Year",y="DALYs (millions)")+theme_nm()

  fc_inc_plot <- bind_rows(
    fc_inc_raw %>% mutate(Vl=NA,Vh=NA,type="Observed"),
    fc_inc %>% filter(type=="Forecast")
  ) %>% mutate(LMIC_group=factor(LMIC_group,levels=income_fct))

  p5c <- ggplot(fc_inc_plot,aes(year,V,colour=LMIC_group))+
    geom_ribbon(data=fc_inc_plot %>% filter(type=="Forecast"),
      aes(ymin=Vl,ymax=Vh,fill=LMIC_group),alpha=0.08,colour=NA)+
    geom_line(data=fc_inc_plot %>% filter(type=="Observed"),linewidth=0.6)+
    geom_line(data=fc_inc_plot %>% filter(type=="Forecast"),linewidth=0.6)+
    geom_vline(xintercept=2023,linetype="dashed",colour="grey50",linewidth=0.3)+
    scale_colour_manual(values=income_pal,name="Income Group")+
    scale_fill_manual(values=income_pal,guide="none")+
    scale_x_continuous(breaks=seq(1990,2050,10))+
    labs(x="Year",y="VLW (billion USD)")+theme_nm()+theme(legend.position=c(0.25,0.78))

  fc_sex_plot <- bind_rows(
    fc_sex_raw %>% mutate(Vl=NA,Vh=NA,type="Observed"),
    fc_sex %>% filter(type=="Forecast"))

  p5d <- ggplot(fc_sex_plot,aes(year,V,colour=sex_name))+
    geom_ribbon(data=fc_sex_plot %>% filter(type=="Forecast"),
      aes(ymin=Vl,ymax=Vh,fill=sex_name),alpha=0.08,colour=NA)+
    geom_line(data=fc_sex_plot %>% filter(type=="Observed"),linewidth=0.6)+
    geom_line(data=fc_sex_plot %>% filter(type=="Forecast"),linewidth=0.6)+
    geom_vline(xintercept=2023,linetype="dashed",colour="grey50",linewidth=0.3)+
    scale_colour_manual(values=sex_pal,name="Sex")+
    scale_fill_manual(values=sex_pal,guide="none")+
    scale_x_continuous(breaks=seq(1990,2050,10))+
    labs(x="Year",y="VLW (billion USD)")+theme_nm()+theme(legend.position=c(0.2,0.82))

  panel5 <- (p5a|p5b)/(p5c|p5d)+
    plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))
  pdf(file.path(ie_dir,"Figure5_forecast_2050.pdf"),width=9,height=7); print(panel5); dev.off()

  # ════════════════════════════════════════════════════════════════════
  # FIGURE 6: ASR TREND + FOLD CHANGE (a-b)
  # ════════════════════════════════════════════════════════════════════

  asr_t <- df_daly_rate %>% filter(sex_name=="Both",location_id %in% lmic_ids) %>%
    left_join(df_econ %>% select(location_id,LMIC_group),by="location_id") %>%
    group_by(LMIC_group,year) %>% summarise(R=mean(rate)) %>%
    mutate(LMIC_group=factor(LMIC_group,levels=income_fct))

  p6a <- ggplot(asr_t,aes(year,R,colour=LMIC_group))+
    geom_line(linewidth=0.6)+
    scale_colour_manual(values=income_pal,name="Income Group")+
    scale_x_continuous(breaks=seq(1990,2023,5))+
    labs(x="Year",y="Age-standardized DALY rate\n(per 100,000)")+
    theme_nm()+theme(legend.position=c(0.72,0.22))

  chg <- df %>% filter(sex_name=="Both",year %in% c(1990,2023)) %>%
    group_by(LMIC_group,year) %>% summarise(V=sum(VLW)) %>%
    pivot_wider(names_from=year,values_from=V,names_prefix="y") %>%
    mutate(fold=y2023/y1990, LMIC_group=factor(LMIC_group,levels=income_fct))

  p6b <- ggplot(chg,aes(LMIC_group,fold,fill=LMIC_group))+
    geom_col(width=0.55,colour="white",linewidth=0.3)+
    geom_text(aes(label=paste0(formatC(fold,format="f",digits=1),"\u00d7")),
      vjust=-0.3,size=2.5,fontface="bold")+
    scale_fill_manual(values=income_pal,guide="none")+
    scale_y_continuous(expand=expansion(mult=c(0,0.15)))+
    scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+
    labs(x=NULL,y="VLW fold change\n(2023 vs 1990)")+theme_nm()

  panel6 <- p6a|p6b+
    plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))
  pdf(file.path(ie_dir,"Figure6_ASR_foldchange.pdf"),width=9,height=3.8); print(panel6); dev.off()

  # ── Summary print ──
  vlw_2023 <- sum(d23$VLW)
  vlw_2050 <- fc_all %>% filter(year==2050)
  cat("  IE=",ie_lab,": VLW 2023 =",formatC(vlw_2023,format="f",digits=2),"B |",
      "2050 =",fmt_ui(vlw_2050$V,vlw_2050$Vl,vlw_2050$Vh,2),"B\n")
  cat("  Done. →", ie_dir, "\n\n")
}

# ======================================================================
# 4. CROSS-IE COMPARISON TABLE & FIGURE
# ======================================================================
cat("================================================================\n")
cat("  Cross-IE comparison\n")
cat("================================================================\n")

# Summary table across IE
ie_summary <- map_dfr(ie_values, function(ie) {
  d <- compute_vlw_allages(ie) %>% filter(year==2023,sex_name=="Both")
  d %>% summarise(IE=ie, Countries=n(),
    VLW=sum(VLW),VLW_lo=sum(VLW_lo),VLW_hi=sum(VLW_hi),
    VLW_d=sum(VLW_d),VLW_d_lo=sum(VLW_d_lo),VLW_d_hi=sum(VLW_d_hi),
    Wp=weighted.mean(pct,GDP_tot),Wpl=weighted.mean(pct_lo,GDP_tot),
    Wph=weighted.mean(pct_hi,GDP_tot))
}) %>% transmute(
  `Income Elasticity`=IE,
  `VLW (billion USD)`=fmt_ui(VLW,VLW_lo,VLW_hi,2),
  `VLW discounted (billion USD)`=fmt_ui(VLW_d,VLW_d_lo,VLW_d_hi,2),
  `VLW/GDP (%)`=fmt_ui(Wp,Wpl,Wph,4))

write_csv(ie_summary, file.path(out_root,"CrossIE_sensitivity_summary.csv"))
cat("  CrossIE table saved.\n")

# Cross-IE figure: bar by IE × income
ie_bar <- map_dfr(ie_values, function(ie) {
  compute_vlw_allages(ie) %>% filter(year==2023,sex_name=="Both") %>%
    group_by(LMIC_group) %>% summarise(V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi)) %>%
    mutate(IE=paste0("IE = ",format(ie,nsmall=1)))
}) %>% mutate(LMIC_group=factor(LMIC_group,levels=income_fct))

ie_pal3 <- c("IE = 0.5"=pal$orange,"IE = 1.0"=pal$blue,"IE = 1.5"=pal$teal)

p_ie <- ggplot(ie_bar,aes(LMIC_group,V,fill=IE))+
  geom_col(position=position_dodge(0.7),width=0.6,colour="white",linewidth=0.2)+
  geom_errorbar(aes(ymin=Vl,ymax=Vh),position=position_dodge(0.7),
    width=0.12,linewidth=0.25)+
  scale_fill_manual(values=ie_pal3,name="Income\nElasticity")+
  scale_y_continuous(expand=expansion(mult=c(0,0.1)))+
  scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+
  labs(x=NULL,y="VLW (billion USD)")+theme_nm()+
  theme(legend.position=c(0.15,0.8))

pdf(file.path(out_root,"CrossIE_sensitivity_bar.pdf"),width=5.5,height=3.8)
print(p_ie); dev.off()
cat("  CrossIE figure saved.\n\n")

cat("================================================================\n")
cat("  ALL COMPLETE.\n")
cat("  Output root:", out_root, "\n")
cat("================================================================\n")
