#!/usr/bin/env Rscript
# ==============================================================================
# Figure 2 trends.pdf
#
# Manuscript figure: Figure 2
# Valuation scenario: IE = 1.0
# Plotting code    : scripts/run_LMIC_Pancreatic_VLW_v2.R lines 548-571
# Shared preamble  : _common.R (= canonical script lines 1-352)
# Output           : fig/Figure 2 trends.pdf
#
# Only two things differ from the canonical script: (1) the two-space for-loop indent is
# removed; (2) the pdf() output path becomes OUT. The plotting logic is character-identical.
#
# Run on its own:  Rscript "fig/code/Figure 2 trends.R"
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

OUT <- file.path(FIG_DIR, "Figure 2 trends.pdf")

# ---- Plot (canonical script lines 548-571) ----
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
pdf(OUT,width=9,height=7)
print((p2a|p2b)/(p2c|p2d)+plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))); dev.off()

cat("Written: ", OUT, "\n", sep="")
