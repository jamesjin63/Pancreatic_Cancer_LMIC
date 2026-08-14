# fig/ — Manuscript figures and their per-figure code

This folder holds every manuscript figure under the name used in the manuscript, together with a
standalone R script for each one.

- **19 PDFs**: 7 main figures (Figure 1–7) and 12 supplementary figures (Supplementary Figure 1–12)
- **19 R scripts** in `code/`, each named after the figure it produces
- Verified: output of these standalone scripts is byte-for-byte identical to the output of the full
  pipeline `run_LMIC_Pancreatic_VLW_R3.R` (after zeroing the PDF creation timestamps)

```
fig/
├── README.md                                          <- this file
├── Figure 1 2023 snapshot.pdf                         ┐
├── …                                                  ├ 19 figures (manuscript naming)
├── Supplementary Figure 12 IE1.5 forecast.pdf         ┘
└── code/
    ├── _common.R                                      shared preamble (canonical script lines 1–348)
    ├── make_all_figures.R                             regenerate all 19 in one command
    ├── Figure 1 2023 snapshot.R                       ┐
    ├── …                                              ├ 19 per-figure scripts
    └── Supplementary Figure 12 IE1.5 forecast.R       ┘
```

---

## Running

Regenerate everything (about 67 s; each figure runs in its own R process):

```bash
cd code
Rscript "fig/code/make_all_figures.R"
```

Regenerate a single figure:

```bash
Rscript "fig/code/Figure 4 maps.R"
```

Figures are written to `fig/`. The scripts locate their own directory, so they can be invoked from
any working directory.

---

## Figure-to-code map

The three income-elasticity scenarios share one set of plotting code:
**IE = 1.0 → main figures**, **IE = 0.5 → Supplementary Figures 1–6**,
**IE = 1.5 → Supplementary Figures 7–12**.
"Canonical script lines" refer to `scripts/run_LMIC_Pancreatic_VLW_v2.R`; "analysis name" is the
original filename used inside that script.

### Main figures

| Manuscript figure | Script | IE | Canonical lines | Analysis name |
|---|---|---|---|---|
| Figure 1 · 2023 snapshot | `Figure 1 2023 snapshot.R` | 1.0 | 509–543 | `IE1/Figure1_2023_snapshot.pdf` |
| Figure 2 · trends | `Figure 2 trends.R` | 1.0 | 545–568 | `IE1/Figure2_trends.pdf` |
| Figure 3 · unweighted mean country ASR & fold change | `Figure 3 unweighted mean country ASR and fold change.R` | 1.0 | 790–807 | `IE1/Figure6_ASR_foldchange.pdf` |
| Figure 4 · maps | `Figure 4 maps.R` | 1.0 | 570–585 | `IE1/Figure3_maps.pdf` |
| Figure 5 · age specific | `Figure 5 age specific.R` | 1.0 | 587–610 | `IE1/Figure4_age_specific.pdf` |
| Figure 6 · forecast 2050 | `Figure 6 forecast 2050.R` | 1.0 | 612–647 + 759–788 | `IE1/Figure5_forecast_2050.pdf` |
| Figure 7 · IE sensitivity | `Figure 7 IE sensitivity.R` | all three | 845–856 | `CrossIE_sensitivity_bar.pdf` |

> Note that manuscript numbering and analysis numbering are **not** in the same order: manuscript
> Figure 3 uses `Figure6` from the script, Figure 4 uses `Figure3`, Figure 5 uses `Figure4`, and
> Figure 6 uses `Figure5`. The renaming map is `submission_figure_map` at canonical lines 861–881.

### Supplementary figures (IE = 0.5)

| Manuscript figure | Script | Canonical lines | Analysis name |
|---|---|---|---|
| Supp Fig 1 · IE0.5 snapshot | `Supplementary Figure 1 IE0.5 snapshot.R` | 509–543 | `IE05/Figure1_2023_snapshot.pdf` |
| Supp Fig 2 · IE0.5 trends | `Supplementary Figure 2 IE0.5 trends.R` | 545–568 | `IE05/Figure2_trends.pdf` |
| Supp Fig 3 · IE0.5 unweighted mean country ASR | `Supplementary Figure 3 IE0.5 unweighted mean country ASR.R` | 790–807 | `IE05/Figure6_ASR_foldchange.pdf` |
| Supp Fig 4 · IE0.5 maps | `Supplementary Figure 4 IE0.5 maps.R` | 570–585 | `IE05/Figure3_maps.pdf` |
| Supp Fig 5 · IE0.5 age specific | `Supplementary Figure 5 IE0.5 age specific.R` | 587–610 | `IE05/Figure4_age_specific.pdf` |
| Supp Fig 6 · IE0.5 forecast | `Supplementary Figure 6 IE0.5 forecast.R` | 612–647 + 759–788 | `IE05/Figure5_forecast_2050.pdf` |

### Supplementary figures (IE = 1.5)

| Manuscript figure | Script | Canonical lines | Analysis name |
|---|---|---|---|
| Supp Fig 7 · IE1.5 snapshot | `Supplementary Figure 7 IE1.5 snapshot.R` | 509–543 | `IE15/Figure1_2023_snapshot.pdf` |
| Supp Fig 8 · IE1.5 trends | `Supplementary Figure 8 IE1.5 trends.R` | 545–568 | `IE15/Figure2_trends.pdf` |
| Supp Fig 9 · IE1.5 unweighted mean country ASR | `Supplementary Figure 9 IE1.5 unweighted mean country ASR.R` | 790–807 | `IE15/Figure6_ASR_foldchange.pdf` |
| Supp Fig 10 · IE1.5 maps | `Supplementary Figure 10 IE1.5 maps.R` | 570–585 | `IE15/Figure3_maps.pdf` |
| Supp Fig 11 · IE1.5 age specific | `Supplementary Figure 11 IE1.5 age specific.R` | 587–610 | `IE15/Figure4_age_specific.pdf` |
| Supp Fig 12 · IE1.5 forecast | `Supplementary Figure 12 IE1.5 forecast.R` | 612–647 + 759–788 | `IE15/Figure5_forecast_2050.pdf` |

---

## Structure of each script

```r
FIG_CODE_DIR <- .self_path()
source(file.path(FIG_CODE_DIR, "_common.R"))   # canonical script lines 1-348

ie <- 1.0                                       # scenario
df <- compute_allages(ie); df_av <- compute_age(ie)
d23 <- ...; d23_all <- ...                      # canonical script lines 363-366

OUT <- file.path(FIG_DIR, "<manuscript figure name>.pdf")
<plotting code, verbatim from the canonical script>   # only the pdf() path becomes OUT
```

`_common.R` locates the `code/` directory and `setwd()`s into it (the canonical script reads
`data_raw/` by relative path), then executes canonical script lines 1–348 verbatim: paths,
constants (`VSL_USA` = 13.2e6, `GDP_pc_USA` = 82304.62, `discount_rate` = 0.03), palettes and
themes, helper functions, data loading, VLW computation, and the forecast helpers.

**Only two things differ from the canonical script**: the two-space `for`-loop indent is removed,
and the `pdf()` output path becomes `OUT`. The plotting logic is character-identical, which is why
the outputs match byte for byte.

---

## Reproducibility

The figures are deterministic; every stochastic step has an explicit seed:

| Location | Seed | Affects |
|---|---|---|
| `run_..._v2.R:520` `position_jitter(..., seed=20260724L)` | fixed | jitter of the points in panel b of Figure 1 / Supp Figs 1 and 7 |
| `:617` `joint_income_projection(..., seed=20260724L)` | fixed | ETS joint simulation (50,000 paths) |
| `:618` `joint_income_projection(..., seed=20260725L)` | fixed | ARIMA sensitivity analysis |

**Environment:** R 4.2.2 / aarch64-apple-darwin20 / macOS 14.5;
tidyverse 2.0.0, ggplot2 3.5.1, sf 1.0-16, forecast 8.21.1, patchwork 1.2.0, scales 1.3.0.
