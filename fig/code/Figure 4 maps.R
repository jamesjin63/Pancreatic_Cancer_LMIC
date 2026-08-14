#!/usr/bin/env Rscript
# ==============================================================================
# Figure 4 maps.pdf
#
# Manuscript figure: Figure 4
# Valuation scenario: IE = 1.0
# Plotting code    : scripts/run_LMIC_Pancreatic_VLW_v2.R lines 573-588
# Shared preamble  : _common.R (= canonical script lines 1-352)
# Output           : fig/Figure 4 maps.pdf
#
# Only two things differ from the canonical script: (1) the two-space for-loop indent is
# removed; (2) the pdf() output path becomes OUT. The plotting logic is character-identical.
#
# Run on its own:  Rscript "fig/code/Figure 4 maps.R"
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

OUT <- file.path(FIG_DIR, "Figure 4 maps.pdf")

# ---- Plot (canonical script lines 573-588) ----
# Figure 3 — maps
md <- d23 %>% select(location_id,VLW,pct,LMIC_group)
w  <- df_world %>% left_join(md,by="location_id")
p3a <- ggplot(w)+geom_sf(aes(fill=VLW),colour="grey40",linewidth=0.06)+
  scale_fill_gradientn(colours=map_blue,na.value="grey92",trans="log1p",
    breaks=c(0,0.1,1,10,100),labels=c("0","0.1","1","10","100"),name="VLW\n(billion)",
    guide=guide_colourbar(barheight=grid::unit(34,"mm"),title.position="top"))+theme_nm_map()
p3b <- ggplot(w)+geom_sf(aes(fill=pct),colour="grey40",linewidth=0.06)+
  scale_fill_gradientn(colours=map_red,na.value="grey92",trans="log1p",
    breaks=c(0,0.1,0.5,1),labels=c("0","0.1","0.5","1"),name="VLW/GDP\n(%)",
    guide=guide_colourbar(barheight=grid::unit(34,"mm"),title.position="top"))+theme_nm_map()
w2 <- df_world %>% left_join(d23 %>% select(location_id,LMIC_group),by="location_id")
p3c <- ggplot(w2)+geom_sf(aes(fill=LMIC_group),colour="grey40",linewidth=0.06)+
  scale_fill_manual(values=income_pal,na.value="grey92",name="Income Group")+theme_nm_map()
pdf(OUT,width=8,height=11)
print(p3a/p3b/p3c+plot_annotation(tag_levels="a")&theme(plot.tag=element_text(size=10,face="bold"))); dev.off()

cat("Written: ", OUT, "\n", sep="")
