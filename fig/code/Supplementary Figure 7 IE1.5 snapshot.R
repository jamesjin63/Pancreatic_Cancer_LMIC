#!/usr/bin/env Rscript
# ==============================================================================
# Supplementary Figure 7 IE1.5 snapshot.pdf
#
# Manuscript figure: Supplementary Figure 7
# Valuation scenario: IE = 1.5
# Plotting code    : scripts/run_LMIC_Pancreatic_VLW_v2.R lines 512-546
# Shared preamble  : _common.R (= canonical script lines 1-454)
# Output           : fig/Supplementary Figure 7 IE1.5 snapshot.pdf
#
# Only two things differ from the canonical script: (1) the two-space for-loop indent is
# removed; (2) the pdf() output path becomes OUT. The plotting logic is character-identical.
#
# Run on its own:  Rscript "fig/code/Supplementary Figure 7 IE1.5 snapshot.R"
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

# ---- Scenario and data preparation (canonical script lines 366-369) ----
ie      <- 1.5
ie_tag  <- paste0("IE", gsub("\\.", "", as.character(ie)))
ie_lab  <- format(ie, nsmall = 1)

df    <- compute_allages(ie)            # base case (3% discount)
df_av <- compute_age(ie)
d23   <- df %>% filter(year==2023, sex_name=="Both")
d23_all <- df %>% filter(year==2023)

OUT <- file.path(FIG_DIR, "Supplementary Figure 7 IE1.5 snapshot.pdf")

# ---- Plot (canonical script lines 512-546) ----
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
pdf(OUT,width=8.5,height=7)
print((p1a|p1b)/(p1c|p1d)+plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))); dev.off()

cat("Written: ", OUT, "\n", sep="")
