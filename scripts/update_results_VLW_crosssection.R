################################################################################
# UPDATE results_VLW in place — 2023 cross-section, CONSISTENT (v2) method
# Rebuilds the parts that only need country×sex all-ages DALYs (preserved in the
# backup Table4): Tables 1-4, Kruskal-Wallis, regression, Figure 1, Figure 3 maps,
# CrossIE summary + bar, regression figure. Overwrites the wrong files in
# results_VLW/IE{1,05,15} and results_VLW/.
# Time/age/forecast outputs (Table5/6/7/8, Fig2/4/5/6) are NOT touched here; they
# require the missing source merged.csv and are quarantined by the companion script.
################################################################################
suppressMessages({library(tidyverse);library(sf);library(patchwork);library(scales)})
options(warn=-1, dplyr.summarise.inform=FALSE)

base   <- normalizePath(".")
bk_t4  <- file.path(base,"data_raw/backup_results_2026-06-12/IE1/Table4_country_full.csv")
hale_f <- file.path(base,"data_raw/external_metadata/HALE.csv"); gdp_f<-file.path(base,"data_raw/external_metadata/gdp.csv")
lmic_f <- file.path(base,"data_raw/external_metadata/204_with_LMIC.csv"); world_f<-file.path(base,"data_raw/external_metadata/df_world2.geojson")
out    <- file.path(base,"outputs/results_VLW")

VSL_USA<-13.2e6; GDP_pc_USA<-82304.62; rr<-0.03
ie_values<-c(0.5,1.0,1.5); income_fct<-c("Low income","Lower middle income","Upper middle income")
annuity<-function(T,r) ifelse(T<=0,NA_real_,ifelse(r==0,T,(1-(1+r)^(-T))/r))
fmt_ui<-function(v,lo,hi,d=1,s=1) paste0(formatC(v/s,format="f",digits=d,big.mark=",")," (",
  formatC(lo/s,format="f",digits=d,big.mark=","),"–",formatC(hi/s,format="f",digits=d,big.mark=","),")")
pal<-list(blue="#1B4F72",red="#C0392B",teal="#117A65",orange="#E67E22",grey="#5D6D7E",purple="#6C3483")
income_pal<-c("Low income"="#C0392B","Lower middle income"="#E67E22","Upper middle income"="#117A65")
sex_pal<-c("Male"="#1B4F72","Female"="#C0392B","Both"="#5D6D7E")
map_blue<-c("#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1","#6BAED6","#4292C6","#2171B5","#084594")
map_red <-c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#99000D")
theme_nm<-function(bs=7.5) theme_minimal(base_size=bs) %+replace% theme(
  plot.title=element_blank(),axis.title=element_text(size=bs,face="bold",colour="grey20"),
  axis.text=element_text(size=bs-0.5,colour="grey30"),axis.line=element_line(colour="grey50",linewidth=0.3),
  legend.title=element_text(size=bs-0.5,face="bold"),legend.text=element_text(size=bs-1),
  legend.key.size=unit(0.3,"cm"),panel.grid.major=element_line(colour="grey93",linewidth=0.25),
  panel.grid.minor=element_blank(),plot.margin=margin(3,5,3,3,"pt"))
theme_map<-function(bs=7.5) theme_void(base_size=bs) %+replace% theme(legend.position="right",
  legend.title=element_text(size=bs-0.5,face="bold"),legend.text=element_text(size=bs-1),
  legend.key.size=unit(0.3,"cm"),plot.margin=margin(2,2,2,2,"pt"))

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
parse_ui<-function(col){s<-str_replace_all(col,",","")
  m<-str_match(s,"^\\s*([0-9.]+)\\s*\\(\\s*([0-9.]+)\\s*[–-]\\s*([0-9.]+)\\s*\\)")
  tibble(val=as.numeric(m[,2]),lo=as.numeric(m[,3]),hi=as.numeric(m[,4]))}
t4<-read_csv(bk_t4,show_col_types=FALSE); d<-parse_ui(t4$DALYs)
daly<-tibble(location_name=t4$Country,sex_name=t4$Sex,LMIC_group=t4$`Income Group`,
  DALY=d$val,lower=d$lo,upper=d$hi) %>% filter(sex_name %in% c("Male","Female"))
df_lmic<-read_csv(lmic_f,show_col_types=FALSE) %>% filter(LMIC==1) %>%
  select(location_id,location_name,LMIC_group) %>%
  mutate(wb=ifelse(location_name %in% names(gbd_to_wb),gbd_to_wb[location_name],location_name),wb_n=norm_name(wb))
df_gdp<-read_csv(gdp_f,show_col_types=FALSE) %>% filter(year==2023,!is.na(NY.GDP.PCAP.PP.CD),!is.na(NY.GDP.MKTP.PP.CD)) %>%
  select(country,GDP_pc=NY.GDP.PCAP.PP.CD,GDP_tot=NY.GDP.MKTP.PP.CD) %>% mutate(cn=norm_name(country))
df_econ<-df_lmic %>% left_join(df_gdp,by=c("wb_n"="cn")) %>% filter(!is.na(GDP_pc)) %>%
  select(location_id,location_name,LMIC_group,GDP_pc,GDP_tot)
df_hale<-read_csv(hale_f,show_col_types=FALSE) %>% filter(metric_name=="Years",year==2023,age_name=="All ages",sex_id %in% c(1,2)) %>%
  mutate(sex_name=ifelse(sex_id==1,"Male","Female")) %>% select(location_id,sex_name,HALE=val)
dat<-daly %>% inner_join(df_econ %>% select(-LMIC_group),by="location_name") %>% inner_join(df_hale,by=c("location_id","sex_name"))
df_world<-st_read(world_f,quiet=TRUE); df_world$location_id<-as.numeric(df_world$location_id)
cat("Countries:",n_distinct(dat$location_id),"\n")

compute_ie<-function(ie,r=rr){
  bsx<-dat %>% mutate(VSL_i=VSL_USA*(GDP_pc/GDP_pc_USA)^ie,VSLY=VSL_i/annuity(HALE,r),VSLY_u=VSL_i/HALE,
    VLW=VSLY*DALY/1e9,VLW_lo=VSLY*lower/1e9,VLW_hi=VSLY*upper/1e9,
    VLW_u=VSLY_u*DALY/1e9,VLW_u_lo=VSLY_u*lower/1e9,VLW_u_hi=VSLY_u*upper/1e9)
  both<-bsx %>% group_by(location_id,location_name,LMIC_group,GDP_pc,GDP_tot) %>%
    summarise(across(c(DALY,lower,upper,VLW,VLW_lo,VLW_hi,VLW_u,VLW_u_lo,VLW_u_hi),sum),HALE=mean(HALE),.groups="drop") %>%
    mutate(sex_name="Both")
  bind_rows(bsx %>% select(names(both)),both) %>%
    mutate(pct=VLW*1e9/GDP_tot*100,pct_lo=VLW_lo*1e9/GDP_tot*100,pct_hi=VLW_hi*1e9/GDP_tot*100)
}

ie_head<-list()
for(ie in ie_values){
  tag<-paste0("IE",gsub("\\.","",as.character(ie))); idir<-file.path(out,tag); dir.create(idir,showWarnings=FALSE,recursive=TRUE)
  df<-compute_ie(ie); d23<-df %>% filter(sex_name=="Both"); d23a<-df

  mk<-function(data,gv=NULL){g<-if(!is.null(gv)) data %>% group_by(across(all_of(gv))) else data
    g %>% summarise(N=n(),D=sum(DALY),Dl=sum(lower),Dh=sum(upper),V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi),
      Vu=sum(VLW_u),Vul=sum(VLW_u_lo),Vuh=sum(VLW_u_hi),
      Wp=weighted.mean(pct,GDP_tot),Wpl=weighted.mean(pct_lo,GDP_tot),Wph=weighted.mean(pct_hi,GDP_tot),mH=mean(HALE),.groups="drop")}
  write_csv(bind_rows(mk(d23,"LMIC_group") %>% arrange(factor(LMIC_group,levels=income_fct)),mk(d23) %>% mutate(LMIC_group="All LMICs")) %>%
    transmute(`Income Group`=LMIC_group,Countries=N,`DALYs (thousands)`=fmt_ui(D,Dl,Dh,1,1e3),
      `VLW (billion USD) [DALY 95% UI]`=fmt_ui(V,Vl,Vh,2),`VLW undiscounted r=0 (billion USD)`=fmt_ui(Vu,Vul,Vuh,2),
      `VLW/GDP (%) [DALY 95% UI]`=fmt_ui(Wp,Wpl,Wph,4),`Mean HALE`=formatC(mH,format="f",digits=1)),
    file.path(idir,"Table1_income_summary.csv"))
  write_csv(d23a %>% group_by(sex_name,LMIC_group) %>%
    summarise(N=n(),D=sum(DALY),Dl=sum(lower),Dh=sum(upper),V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi),
      Vu=sum(VLW_u),Vul=sum(VLW_u_lo),Vuh=sum(VLW_u_hi),.groups="drop") %>%
    transmute(Sex=sex_name,`Income Group`=LMIC_group,Countries=N,`DALYs (thousands)`=fmt_ui(D,Dl,Dh,1,1e3),
      `VLW (billion USD)`=fmt_ui(V,Vl,Vh,2),`VLW undiscounted (billion USD)`=fmt_ui(Vu,Vul,Vuh,2)),
    file.path(idir,"Table2_sex_income.csv"))
  write_csv(d23 %>% arrange(desc(pct)) %>% mutate(Rank=row_number()) %>%
    transmute(Rank,Country=location_name,`Income Group`=LMIC_group,`GDP pc (PPP)`=formatC(GDP_pc,format="f",digits=0,big.mark=","),
      `DALYs (thousands)`=fmt_ui(DALY,lower,upper,1,1e3),`VLW (billion USD)`=fmt_ui(VLW,VLW_lo,VLW_hi,3),
      `VLW/GDP (%)`=fmt_ui(pct,pct_lo,pct_hi,4)),file.path(idir,"Table3_all_LMIC_countries.csv"))
  write_csv(d23a %>% transmute(Country=location_name,Sex=sex_name,`Income Group`=LMIC_group,
    `DALYs`=fmt_ui(DALY,lower,upper,0),`VLW (billion USD)`=fmt_ui(VLW,VLW_lo,VLW_hi,3),
    `VLW undiscounted (billion USD)`=fmt_ui(VLW_u,VLW_u_lo,VLW_u_hi,3),`VLW/GDP (%)`=fmt_ui(pct,pct_lo,pct_hi,4)),
    file.path(idir,"Table4_country_full.csv"))

  # Kruskal-Wallis + regression
  kd<-d23 %>% mutate(grp=factor(LMIC_group,levels=income_fct)); kt<-kruskal.test(pct~grp,data=kd)
  reg<-lm(log(pct)~log(GDP_pc),data=d23); rc<-summary(reg)$coefficients
  write_csv(tibble(test="Kruskal-Wallis VLW/GDP across income groups",H=unname(kt$statistic),df=unname(kt$parameter),
    p_value=kt$p.value,n=nrow(kd),epsilon2=unname(kt$statistic)/(nrow(kd)-1),
    reg_slope_logGDP=rc[2,1],reg_p=rc[2,4],reg_R2=summary(reg)$r.squared,
    note="groups income-defined (descriptive); regression is the non-circular gradient test"),
    file.path(idir,"Stats_KruskalWallis_and_regression.csv"))
  write_csv(as.data.frame(suppressWarnings(pairwise.wilcox.test(kd$pct,kd$grp,p.adjust.method="bonferroni"))$p.value) %>%
    rownames_to_column("group"),file.path(idir,"Stats_KW_posthoc.csv"))

  # Figure 1 — 2023 snapshot
  bar1<-d23 %>% group_by(LMIC_group) %>% summarise(V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi),.groups="drop") %>%
    mutate(LMIC_group=factor(LMIC_group,levels=income_fct))
  p1a<-ggplot(bar1,aes(LMIC_group,V,fill=LMIC_group))+geom_col(width=0.6,colour="white",linewidth=0.3)+
    geom_errorbar(aes(ymin=Vl,ymax=Vh),width=0.12,linewidth=0.3)+scale_fill_manual(values=income_pal,guide="none")+
    scale_y_continuous(expand=expansion(mult=c(0,0.1)))+scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+labs(x=NULL,y="VLW (billion USD)")+theme_nm()
  p1b<-ggplot(d23,aes(factor(LMIC_group,income_fct),pct,fill=LMIC_group))+geom_boxplot(width=0.5,outlier.shape=NA,alpha=0.5,linewidth=0.3)+
    geom_jitter(aes(colour=LMIC_group),width=0.12,size=0.6,alpha=0.5)+scale_fill_manual(values=income_pal,guide="none")+
    scale_colour_manual(values=income_pal,guide="none")+scale_y_continuous(labels=\(x) paste0(x,"%"))+
    scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+labs(x=NULL,y="VLW / GDP (%)")+theme_nm()
  sb<-d23a %>% filter(sex_name %in% c("Male","Female")) %>% group_by(sex_name,LMIC_group) %>%
    summarise(V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi),.groups="drop") %>% mutate(LMIC_group=factor(LMIC_group,levels=income_fct))
  p1c<-ggplot(sb,aes(LMIC_group,V,fill=sex_name))+geom_col(position=position_dodge(0.65),width=0.55,colour="white",linewidth=0.2)+
    geom_errorbar(aes(ymin=Vl,ymax=Vh),position=position_dodge(0.65),width=0.12,linewidth=0.25)+scale_fill_manual(values=sex_pal,name="Sex")+
    scale_y_continuous(expand=expansion(mult=c(0,0.1)))+scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+labs(x=NULL,y="VLW (billion USD)")+
    theme_nm()+theme(legend.position=c(0.15,0.85))
  p1d<-ggplot(d23,aes(GDP_pc,pct,colour=LMIC_group,size=DALY))+geom_point(alpha=0.6,stroke=0.2)+
    scale_x_log10(labels=dollar_format(),breaks=c(1000,3000,10000,30000))+scale_y_continuous(labels=\(x) paste0(x,"%"))+
    scale_colour_manual(values=income_pal,name="Income Group")+scale_size_continuous(name="DALYs",range=c(1.5,9),
      breaks=c(5000,20000,100000,500000),labels=c("5K","20K","100K","500K"))+labs(x="GDP per capita (PPP, USD)",y="VLW / GDP (%)")+
    theme_nm()+guides(colour=guide_legend(order=1,override.aes=list(size=2.5)),size=guide_legend(order=2))
  pdf(file.path(idir,"Figure1_2023_snapshot.pdf"),width=8.5,height=7)
  print((p1a|p1b)/(p1c|p1d)+plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))); dev.off()

  # Figure 3 — maps
  w<-df_world %>% left_join(d23 %>% select(location_id,VLW,pct,LMIC_group),by="location_id")
  p3a<-ggplot(w)+geom_sf(aes(fill=VLW),colour="grey40",linewidth=0.06)+scale_fill_gradientn(colours=map_blue,na.value="grey92",
    trans="log1p",breaks=c(0,0.01,0.1,1,10,100),labels=c("0","0.01","0.1","1","10","100"),name="VLW\n(billion)")+theme_map()
  p3b<-ggplot(w)+geom_sf(aes(fill=pct),colour="grey40",linewidth=0.06)+scale_fill_gradientn(colours=map_red,na.value="grey92",
    trans="log1p",breaks=c(0,0.01,0.05,0.1,0.5,1),labels=c("0","0.01","0.05","0.1","0.5","1"),name="VLW/GDP\n(%)")+theme_map()
  p3c<-ggplot(df_world %>% left_join(d23 %>% select(location_id,LMIC_group),by="location_id"))+
    geom_sf(aes(fill=LMIC_group),colour="grey40",linewidth=0.06)+scale_fill_manual(values=income_pal,na.value="grey92",name="Income Group")+theme_map()
  pdf(file.path(idir,"Figure3_maps.pdf"),width=8,height=11)
  print(p3a/p3b/p3c+plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))); dev.off()

  # Regression figure (supports R1.3)
  pr<-ggplot(d23,aes(GDP_pc,pct,colour=LMIC_group))+geom_point(alpha=0.6,size=1.1)+
    geom_smooth(method="lm",se=TRUE,colour=pal$blue,linewidth=0.5,fill="grey80")+
    scale_x_log10(labels=dollar_format())+scale_y_log10(labels=\(x) paste0(x,"%"))+
    scale_colour_manual(values=income_pal,name="Income Group")+
    labs(x="GDP per capita (PPP, USD, log)",y="VLW / GDP (%, log)")+theme_nm()+theme(legend.position=c(0.22,0.82))
  pdf(file.path(idir,"FigureSx_income_gradient_regression.pdf"),width=5,height=4); print(pr); dev.off()

  ie_head[[tag]]<-tibble(IE=ie,VLW=sum(d23$VLW),VLW_lo=sum(d23$VLW_lo),VLW_hi=sum(d23$VLW_hi),
    VLW_u=sum(d23$VLW_u),Wp=weighted.mean(d23$pct,d23$GDP_tot),Wpl=weighted.mean(d23$pct_lo,d23$GDP_tot),Wph=weighted.mean(d23$pct_hi,d23$GDP_tot))
  cat(sprintf("  %s done: VLW=%.2f B\n",tag,sum(d23$VLW)))
}

# CrossIE summary + bar (root)
hl<-bind_rows(ie_head)
write_csv(hl %>% transmute(`Income Elasticity`=IE,`VLW base 3% (billion USD) [DALY 95% UI]`=fmt_ui(VLW,VLW_lo,VLW_hi,2),
  `VLW undiscounted (billion USD)`=formatC(VLW_u,format="f",digits=2),`VLW/GDP (%)`=fmt_ui(Wp,Wpl,Wph,4)),
  file.path(out,"CrossIE_sensitivity_summary.csv"))
ie_bar<-map_dfr(ie_values,function(ie) compute_ie(ie) %>% filter(sex_name=="Both") %>% group_by(LMIC_group) %>%
  summarise(V=sum(VLW),Vl=sum(VLW_lo),Vh=sum(VLW_hi),.groups="drop") %>% mutate(IE=paste0("IE = ",format(ie,nsmall=1)))) %>%
  mutate(LMIC_group=factor(LMIC_group,levels=income_fct))
pdf(file.path(out,"CrossIE_sensitivity_bar.pdf"),width=5.5,height=3.8)
print(ggplot(ie_bar,aes(LMIC_group,V,fill=IE))+geom_col(position=position_dodge(0.7),width=0.6,colour="white",linewidth=0.2)+
  geom_errorbar(aes(ymin=Vl,ymax=Vh),position=position_dodge(0.7),width=0.12,linewidth=0.25)+
  scale_fill_manual(values=c("IE = 0.5"=pal$orange,"IE = 1.0"=pal$blue,"IE = 1.5"=pal$teal),name="Income\nElasticity")+
  scale_y_continuous(expand=expansion(mult=c(0,0.1)))+scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+
  labs(x=NULL,y="VLW (billion USD)")+theme_nm()+theme(legend.position=c(0.15,0.8))); dev.off()
cat("CrossIE updated. DONE.\n")
