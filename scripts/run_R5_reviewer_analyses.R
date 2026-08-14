#!/usr/bin/env Rscript
# ==============================================================================
# run_R5_reviewer_analyses.R - additional analyses requested at the R5 review round
#
# Addresses three of Reviewer #3's major comments:
#   Comment 2: rolling-origin validation (multiple origins x multiple horizons), naive and drift
#              benchmarks, horizon-specific errors, empirical 95% PI coverage, and a prespecified
#              model-selection rule
#   Comment 3: tabulated sex-specific 2050 projections and reconciliation with the primary estimand
#   Comment 6: exclusion fractions by income group, on both a country count and a DALY basis
#
# Data and functions are reused from run_LMIC_Pancreatic_VLW_v2.R lines 1-348; no computation
# logic is reimplemented here.
#
# Run:     Rscript scripts/run_R5_reviewer_analyses.R
# Output:  outputs/R5/
# ==============================================================================

.self <- function() {
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a) != 1L) return(normalizePath("."))
  dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", a), fixed = TRUE)))
}
CODE_DIR <- normalizePath(file.path(.self(), ".."))
setwd(CODE_DIR)

# -- Load the canonical script preamble (lines 1-348) ------------------------------------------
.src <- readLines(file.path("scripts", "run_LMIC_Pancreatic_VLW_v2.R"))
eval(parse(text = paste(.src[1:348], collapse = "\n")), envir = globalenv())

R5_DIR <- file.path("outputs", "R5")
dir.create(R5_DIR, showWarnings = FALSE, recursive = TRUE)

# ══════════════════════════════════════════════════════════════════════════════
# 0. Build the eight validated series (identical to Supplementary Table 5)
# ══════════════════════════════════════════════════════════════════════════════
df_base <- compute_allages(ie_base)

inc_raw <- df_base %>% filter(sex_name == "Both") %>%
  group_by(LMIC_group, year) %>% summarise(V = sum(VLW), D = sum(DALY), .groups = "drop")
sex_raw <- df_base %>% filter(sex_name %in% c("Male", "Female")) %>%
  group_by(sex_name, year) %>% summarise(V = sum(VLW), .groups = "drop")

series_list <- list()
for (g in income_fct) {
  s <- inc_raw %>% filter(LMIC_group == g) %>% arrange(year)
  series_list[[paste(g, "DALY")]] <- list(series = s$D, label = g, outcome = "DALY")
  series_list[[paste(g, "VLW")]]  <- list(series = s$V, label = g, outcome = "VLW")
}
for (sx in c("Male", "Female")) {
  s <- sex_raw %>% filter(sex_name == sx) %>% arrange(year)
  series_list[[paste(sx, "VLW")]] <- list(series = s$V, label = sx, outcome = "VLW")
}
stopifnot(length(series_list) == 8L, all(lengths(lapply(series_list, `[[`, "series")) == 34L))
cat("[0] Eight series built (1990-2023, 34 observations each)\n\n")

# ══════════════════════════════════════════════════════════════════════════════
# 1. Comment 2 - rolling-origin validation
# ══════════════════════════════════════════════════════════════════════════════
# Design (prespecified, not chosen after seeing results):
#   minimum training window = 20 years (1990-2009)  ->  origins = 2009, 2010, ..., 2022 (14)
#   horizons h = 1..6, truncated at each origin by the 2023 end of data (h <= 2023 - origin)
#   four models: ETS(AAN, damped) / ARIMA(auto, non-seasonal) / Naive (random walk) /
#                Drift (random walk with drift)
#   intervals: level = 95 throughout, stated explicitly
MIN_TRAIN <- 20L
Y0 <- 1990L; Y1 <- 2023L
HMAX <- 6L
origins <- (Y0 + MIN_TRAIN - 1L):(Y1 - 1L)          # 2009…2022

fit_forecast <- function(train, model, h) {
  y <- ts(train, start = Y0, frequency = 1)
  fc <- switch(model,
    "ETS"   = forecast(ets(y, model = "AAN", damped = TRUE), h = h, level = forecast_level),
    "ARIMA" = forecast(auto.arima(y, seasonal = FALSE, stepwise = FALSE,
                                  approximation = FALSE), h = h, level = forecast_level),
    "Naive" = rwf(y, h = h, drift = FALSE, level = forecast_level),
    "Drift" = rwf(y, h = h, drift = TRUE,  level = forecast_level))
  tibble(h = seq_len(h), pred = as.numeric(fc$mean),
         lo = as.numeric(fc$lower[, 1]), hi = as.numeric(fc$upper[, 1]))
}

cat("[1] Rolling-origin validation: 8 series x ", length(origins), " origins x 4 models ...\n", sep = "")
roll <- list(); k <- 0L
for (nm in names(series_list)) {
  S <- series_list[[nm]]
  for (o in origins) {
    n_tr <- o - Y0 + 1L
    h_max <- min(HMAX, Y1 - o)
    train <- S$series[1:n_tr]
    actual <- S$series[(n_tr + 1L):(n_tr + h_max)]
    for (m in c("ETS", "ARIMA", "Naive", "Drift")) {
      f <- fit_forecast(train, m, h_max)
      k <- k + 1L
      roll[[k]] <- tibble(series = S$label, outcome = S$outcome, model = m,
                          origin = o, h = f$h, actual = actual,
                          pred = f$pred, lo = f$lo, hi = f$hi)
    }
  }
  cat("    ", nm, " done\n", sep = "")
}
roll <- bind_rows(roll) %>%
  mutate(err = actual - pred, abs_err = abs(err), ape = abs(err / actual) * 100,
         covered = as.integer(actual >= lo & actual <= hi))
write_csv(roll, file.path(R5_DIR, "R5_rolling_origin_raw.csv"))

# 1a - errors and coverage by horizon (pooled over all series and origins, by model)
by_h <- roll %>% group_by(model, h) %>%
  summarise(n_origins = n_distinct(origin), n_obs = n(),
            MAE = mean(abs_err), RMSE = sqrt(mean(err^2)), MAPE = mean(ape),
            coverage_95 = mean(covered) * 100, .groups = "drop") %>%
  arrange(model, h)
write_csv(by_h, file.path(R5_DIR, "R5_rolling_origin_by_horizon.csv"))

# 1b - by series x model (pooled over horizons 1-6)
by_series <- roll %>% group_by(series, outcome, model) %>%
  summarise(n_obs = n(), MAE = mean(abs_err), RMSE = sqrt(mean(err^2)),
            MAPE = mean(ape), coverage_95 = mean(covered) * 100, .groups = "drop") %>%
  arrange(series, outcome, model)
write_csv(by_series, file.path(R5_DIR, "R5_rolling_origin_by_series.csv"))

# 1c - overall model ranking plus skill relative to the naive benchmark (MASE-style ratio)
naive_mae <- roll %>% filter(model == "Naive") %>%
  group_by(series, outcome, h, origin) %>% summarise(nm = mean(abs_err), .groups = "drop")
skill <- roll %>% left_join(naive_mae, by = c("series", "outcome", "h", "origin")) %>%
  group_by(model) %>%
  summarise(MAPE = mean(ape), MAE_rel_naive = mean(abs_err / nm),
            coverage_95 = mean(covered) * 100, .groups = "drop") %>% arrange(MAPE)
write_csv(skill, file.path(R5_DIR, "R5_rolling_origin_model_ranking.csv"))

# 1d - full-sample residual adequacy (Ljung-Box), used by the model-selection rule
resid_ok <- map_dfr(names(series_list), function(nm) {
  S <- series_list[[nm]]
  map_dfr(c("ETS", "ARIMA"), function(m)
    fit_series(S$series, model = m, label = S$label) $ diag %>%
      mutate(outcome = S$outcome, .after = series))
})
write_csv(resid_ok, file.path(R5_DIR, "R5_full_sample_residual_adequacy.csv"))

cat("\n  -- By horizon (all series pooled) --\n"); print(as.data.frame(by_h), row.names = FALSE)
cat("\n  -- Overall model ranking --\n"); print(as.data.frame(skill), row.names = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# 2. Comment 3 - tabulated sex-specific 2050 projections and reconciliation
# ══════════════════════════════════════════════════════════════════════════════
cat("\n[2] Sex-specific 2050 projections and reconciliation ...\n")
vsly_2023 <- inc_raw %>% filter(year == 2023) %>%
  transmute(LMIC_group, VSLY_effective = V * 1e9 / D)
primary <- joint_income_projection(inc_raw, vsly_2023, "ETS", seed = 20260724L)

sex_fc <- map_dfr(c("Male", "Female"), function(sx) {
  s <- sex_raw %>% filter(sex_name == sx) %>% arrange(year)
  z <- fit_series(s$V, "ETS", label = paste("VLW", sx, sep = "|"))
  i <- 2050 - 2023
  tibble(Sex = sx, Year = 2050, Estimand = "Direct sex-specific VLW ETS forecast (secondary)",
         VLW_billion = as.numeric(z$fc$mean[i]),
         VLW_lower_95_PI = as.numeric(z$fc$lower[i, 1]),
         VLW_upper_95_PI = as.numeric(z$fc$upper[i, 1]),
         Ljung_Box_p = z$diag$Ljung_Box_p,
         residual_autocorrelation_signal = z$diag$residual_autocorrelation_signal)
})
prim_2050 <- primary$all %>% filter(Year == 2050)
sex_sum <- sum(sex_fc$VLW_billion)
recon <- tibble(
  Quantity = c("Primary: DALY-derived VLW, all LMICs (ETS)",
               "Secondary: direct sex-specific VLW ETS, Male + Female",
               "Absolute difference", "Relative difference (%)"),
  Value_billion = c(prim_2050$VLW_billion, sex_sum,
                    sex_sum - prim_2050$VLW_billion,
                    (sex_sum - prim_2050$VLW_billion) / prim_2050$VLW_billion * 100))
write_csv(sex_fc, file.path(R5_DIR, "R5_sex_specific_2050.csv"))
write_csv(recon,  file.path(R5_DIR, "R5_sex_reconciliation_2050.csv"))
print(as.data.frame(sex_fc), row.names = FALSE)
print(as.data.frame(recon), row.names = FALSE)

# Sex gap over time: observed male:female ratio in 1990 and 2023, plus the 2050 projection
gap <- bind_rows(
  sex_raw %>% filter(year %in% c(1990, 2023)) %>%
    pivot_wider(names_from = sex_name, values_from = V) %>%
    transmute(Year = year, Male, Female, Male_to_Female = Male / Female, Basis = "Observed"),
  tibble(Year = 2050, Male = sex_fc$VLW_billion[sex_fc$Sex == "Male"],
         Female = sex_fc$VLW_billion[sex_fc$Sex == "Female"]) %>%
    mutate(Male_to_Female = Male / Female, Basis = "Secondary direct ETS forecast"))
write_csv(gap, file.path(R5_DIR, "R5_sex_gap_over_time.csv"))
print(as.data.frame(gap), row.names = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# 3. Comment 6 - exclusion fractions by income group
# ══════════════════════════════════════════════════════════════════════════════
cat("\n[3] Exclusion fractions by income group ...\n")
# 2023 DALYs for all 128 LMICs, including the excluded countries
d23_all128 <- df_daly_all %>% filter(year == 2023, sex_name == "Both") %>%
  left_join(df_lmic %>% select(location_id, LMIC_group), by = "location_id")
included <- df_econ$location_id

excl_tbl <- d23_all128 %>%
  mutate(status = ifelse(location_id %in% included, "Included", "Excluded")) %>%
  group_by(LMIC_group, status) %>%
  summarise(n = n(), DALY = sum(DALY), .groups = "drop") %>%
  pivot_wider(names_from = status, values_from = c(n, DALY), values_fill = 0) %>%
  mutate(n_eligible = n_Included + n_Excluded,
         DALY_total = DALY_Included + DALY_Excluded,
         pct_countries_excluded = n_Excluded / n_eligible * 100,
         pct_DALYs_excluded = DALY_Excluded / DALY_total * 100) %>%
  select(LMIC_group, n_eligible, n_Included, n_Excluded, pct_countries_excluded,
         DALY_total, DALY_Included, DALY_Excluded, pct_DALYs_excluded)
overall <- excl_tbl %>% summarise(
  LMIC_group = "All LMICs", n_eligible = sum(n_eligible), n_Included = sum(n_Included),
  n_Excluded = sum(n_Excluded), pct_countries_excluded = sum(n_Excluded)/sum(n_eligible)*100,
  DALY_total = sum(DALY_total), DALY_Included = sum(DALY_Included),
  DALY_Excluded = sum(DALY_Excluded), pct_DALYs_excluded = sum(DALY_Excluded)/sum(DALY_total)*100)
excl_tbl <- bind_rows(excl_tbl, overall)
write_csv(excl_tbl, file.path(R5_DIR, "R5_exclusion_fractions_by_income_group.csv"))
print(as.data.frame(excl_tbl), row.names = FALSE)

cat("\n==================== Done ====================\nOutput directory: ",
    normalizePath(R5_DIR), "\n", sep = "")
writeLines(capture.output(sessionInfo()), file.path(R5_DIR, "sessionInfo.txt"))

# ══════════════════════════════════════════════════════════════════════════════
# 4. Comment 4 - sensitivity of the fixed within-group composition assumption
# ══════════════════════════════════════════════════════════════════════════════
# Country-sex VSLY is built from 2023 GDP and HALE and is therefore constant over time, so drift
#     Vbar_g,t = sum_i,s VLW_{i,s,t} * 1e9 / sum_i,s DALY_{i,s,t}
# arises solely from shifts in within-group DALY composition. The 1990-2023 drift quantifies it.
cat("\n[4] Historical composition drift in the effective group VSLY ...\n")
vbar <- df_base %>% filter(sex_name == "Both") %>%
  group_by(LMIC_group, year) %>%
  summarise(Vbar = sum(VLW) * 1e9 / sum(DALY), .groups = "drop")
vbar_sens <- vbar %>% group_by(LMIC_group) %>%
  summarise(Vbar_1990 = Vbar[year == 1990], Vbar_2023 = Vbar[year == 2023],
            change_1990_2023_pct = (Vbar_2023 / Vbar_1990 - 1) * 100,
            annualised_drift_pct = ((Vbar_2023 / Vbar_1990)^(1 / 33) - 1) * 100,
            min = min(Vbar), max = max(Vbar),
            implied_27yr_drift_pct = ((Vbar_2023 / Vbar_1990)^(27 / 33) - 1) * 100,
            .groups = "drop")
write_csv(vbar, file.path(R5_DIR, "R5_effective_group_VSLY_by_year.csv"))
write_csv(vbar_sens, file.path(R5_DIR, "R5_fixed_composition_sensitivity.csv"))
print(as.data.frame(vbar_sens), row.names = FALSE)

# Effect on 2050 VLW of extrapolating the effective VSLY at the historical drift rate
adj <- vbar_sens %>% mutate(factor_2050 = (1 + annualised_drift_pct / 100)^27)
grp_2050 <- primary$groups %>% filter(Year == 2050) %>%
  select(LMIC_group = Group, DALY, VLW_billion) %>%
  left_join(adj %>% select(LMIC_group, factor_2050), by = "LMIC_group") %>%
  mutate(VLW_drift_adjusted = VLW_billion * factor_2050)
impact <- tibble(
  Scenario = c("Fixed 2023 effective VSLY (primary)",
               "Effective VSLY extrapolated at historical composition-drift rate",
               "Difference (%)"),
  VLW_2050_billion = c(sum(grp_2050$VLW_billion), sum(grp_2050$VLW_drift_adjusted),
                       (sum(grp_2050$VLW_drift_adjusted) / sum(grp_2050$VLW_billion) - 1) * 100))
write_csv(bind_rows(grp_2050 %>% mutate(across(everything(), as.character)),
                    impact %>% mutate(across(everything(), as.character))),
          file.path(R5_DIR, "R5_fixed_composition_impact_2050.csv"))
print(as.data.frame(impact), row.names = FALSE)
cat("\n[4] done\n")
