#!/usr/bin/env Rscript
# ==============================================================================
# Figure 7 IE sensitivity.pdf
#
# Manuscript figure: Figure 7
# Valuation scenario: IE = 0.5 / 1.0 / 1.5 (all three shown together)
# Plotting code    : scripts/run_LMIC_Pancreatic_VLW_v2.R lines 849-860 (outside the IE loop)
# Shared preamble  : _common.R (= canonical script lines 1-454)
# Output           : fig/Figure 7 IE sensitivity.pdf
#
# Only two things differ from the canonical script: (1) the two-space for-loop indent is
# removed; (2) the pdf() output path becomes OUT. The plotting logic is character-identical.
#
# Run on its own:  Rscript "fig/code/Figure 7 IE sensitivity.R"
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

OUT <- file.path(FIG_DIR, "Figure 7 IE sensitivity.pdf")

# ---- Plot (canonical script lines 849-860) ----
ie_bar <- map_dfr(ie_values, function(ie) {
  compute_allages(ie) %>% filter(year==2023,sex_name=="Both") %>% group_by(LMIC_group) %>%
    summarise(V=sum(VLW),.groups="drop") %>%
    mutate(IE=paste0("IE = ",format(ie,nsmall=1)))
}) %>% mutate(LMIC_group=factor(LMIC_group,levels=income_fct))
ie_pal3 <- c("IE = 0.5"=pal$orange,"IE = 1.0"=pal$blue,"IE = 1.5"=pal$teal)
p_ie <- ggplot(ie_bar,aes(LMIC_group,V,fill=IE))+
  geom_col(position=position_dodge(0.7),width=0.6,colour="white",linewidth=0.2)+
  scale_fill_manual(values=ie_pal3,name="Income\nElasticity")+scale_y_continuous(expand=expansion(mult=c(0,0.1)))+
  scale_x_discrete(labels=c("LIC","LMIC","UMIC"))+labs(x=NULL,y="VLW (billion USD)")+
  theme_nm()+theme(legend.position=c(0.15,0.8))
pdf(OUT,width=5.5,height=3.8); print(p_ie); dev.off()

cat("Written: ", OUT, "\n", sep="")
