#!/usr/bin/env Rscript
# ==============================================================================
# Figure 3 unweighted mean country ASR and fold change.pdf
#
# Manuscript figure: Figure 3
# Valuation scenario: IE = 1.0
# Plotting code    : scripts/run_LMIC_Pancreatic_VLW_v2.R lines 794-811
# Shared preamble  : _common.R (= canonical script lines 1-454)
# Output           : fig/Figure 3 unweighted mean country ASR and fold change.pdf
#
# Only two things differ from the canonical script: (1) the two-space for-loop indent is
# removed; (2) the pdf() output path becomes OUT. The plotting logic is character-identical.
#
# Run on its own:  Rscript "fig/code/Figure 3 unweighted mean country ASR and fold change.R"
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
ie      <- 1.0
ie_tag  <- paste0("IE", gsub("\\.", "", as.character(ie)))
ie_lab  <- format(ie, nsmall = 1)

df    <- compute_allages(ie)            # base case (3% discount)
df_av <- compute_age(ie)
d23   <- df %>% filter(year==2023, sex_name=="Both")
d23_all <- df %>% filter(year==2023)

OUT <- file.path(FIG_DIR, "Figure 3 unweighted mean country ASR and fold change.pdf")

# ---- Plot (canonical script lines 794-811) ----
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
pdf(OUT,width=9,height=3.8)
print(p6a|p6b+plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))); dev.off()

cat("Written: ", OUT, "\n", sep="")
