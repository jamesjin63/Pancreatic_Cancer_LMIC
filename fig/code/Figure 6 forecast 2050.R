#!/usr/bin/env Rscript
# ==============================================================================
# Figure 6 forecast 2050.pdf
#
# Manuscript figure: Figure 6
# Valuation scenario: IE = 1.0
# Plotting code    : scripts/run_LMIC_Pancreatic_VLW_v2.R lines 759-788; forecast objects from lines 612-647
# Shared preamble  : _common.R (= canonical script lines 1-348)
# Output           : fig/Figure 6 forecast 2050.pdf
#
# Only two things differ from the canonical script: (1) the two-space for-loop indent is
# removed; (2) the pdf() output path becomes OUT. The plotting logic is character-identical.
#
# Run on its own:  Rscript "fig/code/Figure 6 forecast 2050.R"
# ==============================================================================

# R's commandArgs() encodes spaces in the --file= path as "~+~". This directory path contains
# spaces, so that encoding has to be reversed before the path can be used.
.self_path <- function() {
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a) != 1L) return(normalizePath("."))
  dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", a), fixed = TRUE)))
}
FIG_CODE_DIR <- .self_path()
source(file.path(FIG_CODE_DIR, "_common.R"))

# ---- Scenario and data preparation (canonical script lines 356-366) ----
ie      <- 1.0
ie_tag  <- paste0("IE", gsub("\\.", "", as.character(ie)))
ie_lab  <- format(ie, nsmall = 1)

df    <- compute_allages(ie)            # base case (3% discount)
df_av <- compute_age(ie)
d23   <- df %>% filter(year==2023, sex_name=="Both")
d23_all <- df %>% filter(year==2023)

OUT <- file.path(FIG_DIR, "Figure 6 forecast 2050.pdf")

# ---- Forecast objects (canonical script lines 612-647) ----
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

# ---- Plot (canonical script lines 759-788) ----
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
pdf(OUT,width=9,height=7)
print((p5a|p5b)/(p5c|p5d)+plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))); dev.off()

cat("Written: ", OUT, "\n", sep="")
