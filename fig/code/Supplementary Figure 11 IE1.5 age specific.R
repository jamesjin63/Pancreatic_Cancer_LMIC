#!/usr/bin/env Rscript
# ==============================================================================
# Supplementary Figure 11 IE1.5 age specific.pdf
#
# Manuscript figure: Supplementary Figure 11
# Valuation scenario: IE = 1.5
# Plotting code    : scripts/run_LMIC_Pancreatic_VLW_v2.R lines 590-613
# Shared preamble  : _common.R (= canonical script lines 1-352)
# Output           : fig/Supplementary Figure 11 IE1.5 age specific.pdf
#
# Only two things differ from the canonical script: (1) the two-space for-loop indent is
# removed; (2) the pdf() output path becomes OUT. The plotting logic is character-identical.
#
# Run on its own:  Rscript "fig/code/Supplementary Figure 11 IE1.5 age specific.R"
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

OUT <- file.path(FIG_DIR, "Supplementary Figure 11 IE1.5 age specific.pdf")

# ---- Plot (canonical script lines 590-613) ----
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
pdf(OUT,width=9,height=7)
print((p4a|p4b)/(p4c|p4d)+plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))); dev.off()

cat("Written: ", OUT, "\n", sep="")
