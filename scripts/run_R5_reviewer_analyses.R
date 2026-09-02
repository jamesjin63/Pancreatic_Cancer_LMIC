#!/usr/bin/env Rscript
# ==============================================================================
# run_R5_reviewer_analyses.R - the analyses Reviewer #3 asked for at the R5 round
#
# Two sets of comments were received on this manuscript, both from Reviewer #3, and both are
# answered here so that there is exactly one script for the R5 round.
#
# -- The numbered comments (sections 1-4) --------------------------------------------------
#   Comment 2: rolling-origin validation (multiple origins x multiple horizons), naive and drift
#              models, horizon-specific errors, empirical coverage of the nominal 95% range, and
#              a prespecified model-selection rule
#   Comment 3: tabulated sex-specific 2050 projections and reconciliation with the primary estimand
#   Comment 6: exclusion fractions by income group, on both a country count and a DALY basis
#   Comment 4: sensitivity of the fixed within-group composition assumption
#
# -- The Major/Minor points on the R5 revision (sections 1, 5-9) ----------------------------
#   Major 1: every evaluated method is a CANDIDATE model, selection is performed at the level of
#            the primary DALY series, and the COMPLETE AGGREGATE INTERVAL CONSTRUCTION is
#            evaluated at every rolling origin. The first two are steps 1d and 1f; the third is
#            section 5 and had never been evaluated before: the earlier rolling-origin work
#            validated single-series marginal intervals only, while the reported all-LMIC range
#            comes from a 50,000-draw joint simulation across the three income groups. Section 5
#            puts that construction through the same rolling-origin test, end to end.
#   Major 2: standard MASE is computed in section 1 and its definition, pooling rule and
#            reference are written out in section 9, so the code and the manuscript agree.
#   Major 3: section 6 reports the residual correlation matrix, its eigenvalues before and after
#            the positive-definite adjustment, the parameter treatment, the seeds and the
#            measured non-negativity behaviour. Section 7 makes the endpoint basis of the
#            composition-drift calculation explicit and bounds it against a full-series fit.
#   Minor 1: section 8 writes a data provenance manifest.
#   Minor 2: ARIMA coefficients and AICc are fixed in fit_series() and tabulated in step 1d.
#   Minor 3: Ljung-Box results are recorded as autocorrelation DETECTED or not, never as
#            established adequacy.
#   Minor 4: the sex-specific 2050 comparison is labelled descriptive in section 2.
#
# Data and functions are reused from the canonical script preamble; no computation logic is
# reimplemented here.
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
source(file.path("scripts", "_env.R"))

# -- Load the canonical script preamble ---------------------------------------------------------
.src <- readLines(file.path("scripts", "run_LMIC_Pancreatic_VLW_v2.R"))
# The preamble is everything before the "3. LOOP OVER IE" section header. Locating it by marker
# rather than by a hard-coded line number keeps this robust when the canonical script is edited.
.mark <- grep("^# 3\\. LOOP OVER IE", .src)
if (length(.mark) != 1L) stop("Could not locate the section-3 marker in the canonical script.")
.preamble <- .src[seq_len(.mark - 2L)]
eval(parse(text = paste(.preamble, collapse = "\n")), envir = globalenv())

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
    for (m in MODEL_SET) {
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

# -- MASE scale (Hyndman & Koehler 2006) --------------------------------------------------------
# The denominator is the in-sample mean absolute one-step change of the *training* window that
# ends at the origin, i.e. mean|y_t - y_{t-1}| over t = 2..n_train. Recomputing it at every origin
# keeps the scale strictly out of sample and makes MASE comparable across the DALY and VLW series,
# which differ in units by nine orders of magnitude. MASE < 1 means the model beats a one-step
# random walk fitted to the same training data.
mase_scale <- map_dfr(names(series_list), function(nm) {
  S <- series_list[[nm]]
  tibble(series = S$label, outcome = S$outcome, origin = origins,
         mase_scale = vapply(origins,
                             function(o) mean(abs(diff(S$series[1:(o - Y0 + 1L)]))),
                             numeric(1)))
})
roll <- roll %>%
  left_join(mase_scale, by = c("series", "outcome", "origin")) %>%
  mutate(scaled_err = abs_err / mase_scale)
stopifnot(!any(is.na(roll$scaled_err)))
write_csv(roll, file.path(R5_DIR, "R5_rolling_origin_raw.csv"))

# 1a - errors and coverage by horizon (pooled over all series and origins, by model)
# MAE and RMSE pool DALY counts with billion-USD values and are reported only for continuity with
# the earlier holdout table; MAPE and MASE are the unit-free quantities that should be compared.
by_h <- roll %>% group_by(model, h) %>%
  summarise(n_origins = n_distinct(origin), n_obs = n(),
            MAE = mean(abs_err), RMSE = sqrt(mean(err^2)), MAPE = mean(ape),
            MASE = mean(scaled_err), coverage_95 = mean(covered) * 100, .groups = "drop") %>%
  arrange(model, h)
write_csv(by_h, file.path(R5_DIR, "R5_rolling_origin_by_horizon.csv"))

# 1b - by series x model (pooled over horizons 1-6)
by_series <- roll %>% group_by(series, outcome, model) %>%
  summarise(n_origins = n_distinct(origin), n_obs = n(),
            MAE = mean(abs_err), RMSE = sqrt(mean(err^2)),
            MAPE = mean(ape), MASE = mean(scaled_err),
            coverage_95 = mean(covered) * 100, .groups = "drop") %>%
  mutate(Role = "Candidate") %>%
  arrange(series, outcome, model)
write_csv(by_series, file.path(R5_DIR, "R5_rolling_origin_by_series.csv"))

# 1c - overall model ranking: MASE, skill relative to the naive benchmark, and coverage
naive_mae <- roll %>% filter(model == "Naive") %>%
  group_by(series, outcome, h, origin) %>% summarise(nm = mean(abs_err), .groups = "drop")
skill <- roll %>% left_join(naive_mae, by = c("series", "outcome", "h", "origin")) %>%
  group_by(model) %>%
  summarise(n_obs = n(), MAPE = mean(ape), MASE = mean(scaled_err),
            MAE_rel_naive = mean(abs_err / nm),
            coverage_95 = mean(covered) * 100, .groups = "drop") %>% arrange(MAPE)
# Two win counts, which answer different questions and must not be conflated:
#   n_series_lowest_MAPE_of_4  - lowest MAPE among all four candidate models. Drift attains the
#                                lowest MAPE on four near-linear series. Major 1: this is
#                                reported plainly and is NOT used to disqualify anything; point
#                                accuracy is only one leg of the selection rule.
#   n_series_beats_ETS_MAPE    - retained for continuity with the R5 round, when only ETS and
#                                ARIMA were treated as candidates. It is a historical comparison
#                                and must NOT be quoted as "ARIMA is best among all methods".
wins4 <- by_series %>% group_by(series, outcome) %>% slice_min(MAPE, n = 1) %>% ungroup() %>%
  count(model, name = "n_series_lowest_MAPE_of_4")
wins2 <- by_series %>% filter(model %in% c("ETS", "ARIMA")) %>%
  group_by(series, outcome) %>% slice_min(MAPE, n = 1) %>% ungroup() %>%
  count(model, name = "n_series_beats_ETS_MAPE")
skill <- skill %>%
  left_join(wins4, by = "model") %>% left_join(wins2, by = "model") %>%
  mutate(across(starts_with("n_series"), ~ ifelse(is.na(.x), 0L, .x)),
         # Major 1: every evaluated method is a candidate. The column is written out so the
         # supplementary table states it rather than leaving it to be inferred.
         Role = "Candidate", .after = model)
write_csv(skill, file.path(R5_DIR, "R5_rolling_origin_model_ranking.csv"))

# 1e - submission-ready supplementary table (three stacked panels, one CSV) --------------------
fmt <- function(x, d) formatC(x, format = "f", digits = d, big.mark = "")
MODEL_ORDER <- c("ARIMA", "ETS", "Drift", "Naive")
# Major 1: every evaluated method is a candidate. The former Candidate/Benchmark split was an
# after-the-fact distinction and has been removed.
ROLE <- c(ARIMA = "Candidate", ETS = "Candidate",
          Drift = "Candidate", Naive = "Candidate")
panel_A <- by_h %>% transmute(
  Panel = "A. Pooled across the eight series, by forecast horizon",
  Model = model, Role = unname(ROLE[model]),
  Stratum = paste0("h = ", h, " year", ifelse(h == 1, "", "s"), " ahead"),
  Origins = n_origins, Forecasts = n_obs,
  MAPE = fmt(MAPE, 2), MASE = fmt(MASE, 3),
  `Empirical coverage of the nominal 95% range (%)` = fmt(coverage_95, 1)) %>%
  arrange(match(Model, MODEL_ORDER), Stratum)
panel_B <- by_series %>% transmute(
  Panel = "B. Pooled across horizons 1-6, by series",
  Model = model, Role = unname(ROLE[model]),
  Stratum = paste0(series, " - ", outcome),
  Origins = n_origins, Forecasts = n_obs,
  MAPE = fmt(MAPE, 2), MASE = fmt(MASE, 3),
  `Empirical coverage of the nominal 95% range (%)` = fmt(coverage_95, 1)) %>%
  arrange(Stratum, match(Model, MODEL_ORDER))
panel_C <- skill %>% transmute(
  Panel = "C. All series, origins and horizons pooled",
  Model = model, Role = unname(ROLE[model]), Stratum = "Overall",
  Origins = length(origins), Forecasts = n_obs,
  MAPE = fmt(MAPE, 2), MASE = fmt(MASE, 3),
  `Empirical coverage of the nominal 95% range (%)` = fmt(coverage_95, 1)) %>%
  arrange(match(Model, MODEL_ORDER))
roll_tab <- bind_rows(panel_A, panel_B, panel_C)
write_csv(roll_tab, file.path(R5_DIR, "R5_rolling_origin_supplementary_table.csv"))

# 1d - full-sample residual diagnostics for ALL FOUR candidate models
# Major 1: previously restricted to ETS and ARIMA, which is why Drift and Naive could only be
#   compared on point accuracy. Running the same Ljung-Box test on all four makes the residual
#   criterion applicable to every candidate, so the selection rule is genuinely uniform.
# Minor 3: the test can only fail to reject the null of no residual autocorrelation. The
#   column below is named for what is observed (autocorrelation detected: Yes/No) and must be
#   described that way in the manuscript; it is not evidence that residuals ARE adequate.
resid_ok <- map_dfr(names(series_list), function(nm) {
  S <- series_list[[nm]]
  map_dfr(MODEL_SET, function(m)
    fit_series(S$series, model = m, label = S$label) $ diag %>%
      mutate(outcome = S$outcome, .after = series))
})
write_csv(resid_ok, file.path(R5_DIR, "R5_full_sample_residual_adequacy.csv"))

# Minor 2: ARIMA coefficient estimates with standard errors, for the fitted-model table whose
#   caption claims to report parameter estimates.
arima_coefs <- map_dfr(names(series_list), function(nm) {
  S <- series_list[[nm]]
  fit_series(S$series, model = "ARIMA", label = S$label)$coefficients %>%
    mutate(outcome = S$outcome, .after = series)
})
# Print the coefficients under the names a reader of the fitted-model table expects: the
# forecast package returns ar1/ma2/drift, the table reports AR(1)/MA(2)/Drift.
arima_coefs <- arima_coefs %>%
  mutate(term = sub("^([a-z]+)([0-9]+)$", "\\U\\1\\E(\\2)", term, perl = TRUE),
         term = sub("^drift$", "Drift", term),
         term = sub("^intercept$", "Intercept", term))
write_csv(arima_coefs, file.path(R5_DIR, "R5_arima_coefficients.csv"))

# 1f - the prespecified selection rule, applied to the primary DALY series
# Rule, stated before the results were seen and unchanged since:
#   among candidate models whose residuals show no detected autocorrelation at the 5% level,
#   select the model with the lowest pooled rolling-origin MAPE over horizons 1-6.
# Major 1(b): the rule is applied per primary DALY series. All four models are eligible.
sel_pool <- by_series %>%
  left_join(resid_ok %>% select(series, outcome, model, Ljung_Box_p,
                                residual_autocorrelation_detected),
            by = c("series", "outcome", "model")) %>%
  mutate(eligible = residual_autocorrelation_detected == "No")
selection <- sel_pool %>% filter(outcome == "DALY") %>%
  group_by(series) %>%
  mutate(rank_MAPE_all_candidates = rank(MAPE, ties.method = "min"),
         selected = eligible & MAPE == min(MAPE[eligible])) %>%
  ungroup() %>%
  transmute(Series = series, Outcome = outcome, Model = model,
            Role = "Candidate",
            Ljung_Box_p, Residual_autocorrelation_detected = residual_autocorrelation_detected,
            Eligible_under_rule = ifelse(eligible, "Yes", "No"),
            MAPE, MASE, coverage_95, Rank_by_MAPE = rank_MAPE_all_candidates,
            Selected = ifelse(selected, "Yes", "No")) %>%
  arrange(Series, Rank_by_MAPE)
write_csv(selection, file.path(R5_DIR, "R5_model_selection_primary_DALY.csv"))
cat("\n  -- Prespecified selection on the primary DALY series (all four models eligible) --\n")
print(as.data.frame(selection %>% mutate(across(where(is.numeric), ~ round(.x, 4)))),
      row.names = FALSE)
sel_models <- selection %>% filter(Selected == "Yes")
cat("\n  Selected: ",
    paste(sprintf("%s -> %s", sel_models$Series, sel_models$Model), collapse = "; "),
    "\n", sep = "")

cat("\n  -- By horizon (all series pooled) --\n"); print(as.data.frame(by_h), row.names = FALSE)
cat("\n  -- Overall model ranking --\n"); print(as.data.frame(skill), row.names = FALSE)
cat("\n  -- Supplementary rolling-origin table:", nrow(roll_tab), "rows --\n")

# ══════════════════════════════════════════════════════════════════════════════
# 2. Comment 3 - tabulated sex-specific 2050 projections and reconciliation
# ══════════════════════════════════════════════════════════════════════════════
cat("\n[2] Sex-specific 2050 projections and reconciliation ...\n")
vsly_2023 <- inc_raw %>% filter(year == 2023) %>%
  transmute(LMIC_group, VSLY_effective = V * 1e9 / D)
# Major 1: the primary model is whatever the prespecified rule selects in step 1f, with all
# four methods eligible. The rule is: among candidates showing no detected residual
# autocorrelation at the 5% level, take the lowest pooled rolling-origin MAPE over horizons 1-6,
# applied per primary DALY series. Reading the selection here rather than hard-coding a model
# name means the script cannot silently disagree with its own selection table.
sel_primary <- unique(sel_models$Model)
if (length(sel_primary) != 1L)
  stop("The selection rule chose different models across the primary DALY series (",
       paste(sel_models$Model, collapse = ", "),
       "). The aggregate projection assumes one common model; resolve this before proceeding.")
cat("\n[2] Primary model selected by the prespecified rule: ", sel_primary, "\n", sep = "")
primary <- joint_income_projection(inc_raw, vsly_2023, sel_primary, seed = 20260725L)

sex_fc <- map_dfr(c("Male", "Female"), function(sx) {
  s <- sex_raw %>% filter(sex_name == sx) %>% arrange(year)
  z <- fit_series(s$V, sel_primary, label = paste("VLW", sx, sep = "|"))
  i <- 2050 - 2023
  # Minor 4: the sex-specific comparison is descriptive. Its rolling-origin coverage is the
  #   lowest of the eight validation series (Male 33.3%, Female 43.5%) and the two ranges
  #   overlap, so no inferential claim is attached to it here or in the manuscript.
  tibble(Sex = sx, Year = 2050,
         Estimand = paste0("Direct sex-specific VLW ", sel_primary,
                           " forecast (secondary; descriptive only, not an inferential comparison)"),
         VLW_billion = as.numeric(z$fc$mean[i]),
         VLW_scenario_low = as.numeric(z$fc$lower[i, 1]),
         VLW_scenario_high = as.numeric(z$fc$upper[i, 1]),
         Ljung_Box_p = z$diag$Ljung_Box_p,
         residual_autocorrelation_detected = z$diag$residual_autocorrelation_detected)
})
prim_2050 <- primary$all %>% filter(Year == 2050)
sex_sum <- sum(sex_fc$VLW_billion)
recon <- tibble(
  Quantity = c(paste0("Primary: DALY-derived VLW, all LMICs (", sel_primary, ")"),
               paste0("Secondary: direct sex-specific VLW ", sel_primary, ", Male + Female"),
               "Absolute difference", "Relative difference (%)"),
  Value_billion = c(prim_2050$VLW_billion, sex_sum,
                    sex_sum - prim_2050$VLW_billion,
                    (sex_sum - prim_2050$VLW_billion) / prim_2050$VLW_billion * 100))
# The sex-specific table reports the fitted order and one range, not two bounds in two columns.
sex_fc <- sex_fc %>%
  left_join(resid_ok %>% filter(model == "ARIMA", series %in% c("Male", "Female")) %>%
              select(Sex = series, Method = method), by = "Sex") %>%
  mutate(Scenario_range = sprintf("%.2f-%.2f", VLW_scenario_low, VLW_scenario_high),
         .after = VLW_scenario_high) %>%
  relocate(Method, .after = Sex)
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
    mutate(Male_to_Female = Male / Female,
           Basis = paste0("Secondary direct ", sel_primary, " forecast (descriptive)")))
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

# Name the excluded countries next to their count. Reviewer #3 asks which countries drop out and
# what they carry; a bare "5" does not answer that, and the names are already in the data.
excluded_names <- d23_all128 %>%
  filter(!location_id %in% included) %>%
  arrange(desc(DALY)) %>%
  group_by(LMIC_group) %>%
  summarise(names = paste(location_name, collapse = ", "), .groups = "drop")
and_list <- function(x) sub(",\\s*([^,]+)$", " and \\1", x)
excl_tbl <- excl_tbl %>%
  left_join(excluded_names, by = "LMIC_group") %>%
  mutate(Excluded_countries = ifelse(n_Excluded == 0 | is.na(names),
                                     as.character(n_Excluded),
                                     paste0(n_Excluded, " (", and_list(names), ")")),
         .after = n_Excluded) %>%
  select(-names)
write_csv(excl_tbl, file.path(R5_DIR, "R5_exclusion_fractions_by_income_group.csv"))
print(as.data.frame(excl_tbl), row.names = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# 4. Comment 4 - sensitivity of the fixed within-group composition assumption
# ══════════════════════════════════════════════════════════════════════════════
# Country-sex VSLY is built from 2023 GDP and HALE and is constant over time, so drift in
#     Vbar_g,t = sum_i,s VLW_{i,s,t} * 1e9 / sum_i,s DALY_{i,s,t}
# Vbar_g,t arises solely from shifts in within-group DALY composition. The 1990-2023 drift
# quantifies the assumption.
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

# ══════════════════════════════════════════════════════════════════════════════
# 5. Major 1 - rolling-origin evaluation of the COMPLETE aggregate interval construction
# ══════════════════════════════════════════════════════════════════════════════
# What is being tested. The published all-LMIC range is not a forecast interval from a single
# model. It is the 2.5th and 97.5th percentile of a 50,000-draw simulation that (i) fits the
# three income-group DALY series separately, (ii) reads each group's marginal forecast dispersion
# off its own interval, (iii) couples the groups through the fixed cross-sectional residual
# correlation, and (iv) sums the draws. Every one of those four steps can degrade coverage, and
# none of them was exercised by the earlier single-series rolling-origin validation.
#
# How it is tested. At each origin the whole construction is rebuilt from the training data only:
# models refitted, correlation re-estimated on training residuals, simulation rerun. The
# resulting aggregate range is then compared with the realised aggregate. Nothing from the future
# of the origin enters, including the correlation matrix.
#
# Two aggregates are evaluated:
#   DALY - the primary estimand, summed over the three income groups.
#   VLW  - the monetised quantity, using the effective VSLY of the training period's final year,
#          so the valuation input is also strictly out of sample.
cat("[5] Rolling-origin coverage of the aggregate interval construction ...\n")

groups <- income_fct
hist_wide <- inc_raw %>% select(LMIC_group, year, D, V)

agg_actual <- inc_raw %>% group_by(year) %>%
  summarise(D_total = sum(D), V_total = sum(V), .groups = "drop")

agg_rows <- list(); kk <- 0L
for (m in MODEL_SET) {
  for (o in origins) {
    h_max <- min(HMAX, Y1 - o)
    train <- hist_wide %>% filter(year <= o)
    # (i) refit each group on training data only
    fits <- setNames(lapply(groups, function(g) {
      x <- train %>% filter(LMIC_group == g) %>% arrange(year) %>% pull(D)
      fit_series(x, model = m, h = h_max, label = g)
    }), groups)
    # (iii) correlation re-estimated on TRAINING residuals at this origin
    rmat <- do.call(cbind, lapply(fits, `[[`, "residuals"))
    corr <- pd_correlation_eigen_clip(cor(rmat, use = "pairwise.complete.obs"))
    # valuation input from the final training year, so it too is out of sample
    v_train <- train %>% filter(year == o) %>%
      transmute(LMIC_group, v = V * 1e9 / D)
    v <- v_train$v[match(groups, v_train$LMIC_group)] / 1e9
    for (k in seq_len(h_max)) {
      means  <- sapply(fits, function(z) as.numeric(z$fc$mean[k]))
      lowers <- sapply(fits, function(z) as.numeric(z$fc$lower[k, 1]))
      uppers <- sapply(fits, function(z) as.numeric(z$fc$upper[k, 1]))
      ses <- pmax((uppers - lowers) / (2 * qnorm(0.975)), .Machine$double.eps)
      cov_mat <- diag(ses) %*% corr %*% diag(ses)
      # same seeding convention as the published construction
      set.seed(20260725L + k)
      z <- matrix(rnorm(simulation_n * length(groups)), ncol = length(groups))
      draws <- sweep(z %*% chol(cov_mat), 2, means, "+")
      total_d <- rowSums(draws)
      total_v <- rowSums(sweep(draws, 2, v, "*"))
      act <- agg_actual %>% filter(year == o + k)
      d_lo <- unname(quantile(total_d, 0.025)); d_hi <- unname(quantile(total_d, 0.975))
      v_lo <- unname(quantile(total_v, 0.025)); v_hi <- unname(quantile(total_v, 0.975))
      kk <- kk + 1L
      agg_rows[[kk]] <- tibble(
        model = m, origin = o, h = k, target_year = o + k,
        DALY_actual = act$D_total, DALY_point = sum(means),
        DALY_scenario_low = d_lo, DALY_scenario_high = d_hi,
        DALY_covered = as.integer(act$D_total >= d_lo & act$D_total <= d_hi),
        DALY_ape = abs(act$D_total - sum(means)) / act$D_total * 100,
        VLW_actual = act$V_total, VLW_point = sum(means * v),
        VLW_scenario_low = v_lo, VLW_scenario_high = v_hi,
        VLW_covered = as.integer(act$V_total >= v_lo & act$V_total <= v_hi),
        VLW_ape = abs(act$V_total - sum(means * v)) / act$V_total * 100,
        rel_width_DALY = (d_hi - d_lo) / sum(means) * 100,
        n_group_draws_nonpositive = sum(draws <= 0))
    }
  }
  cat("    ", m, " done\n", sep = "")
}
agg <- bind_rows(agg_rows)
write_csv(agg, file.path(R5_DIR, "R5_aggregate_interval_rolling_origin_raw.csv"))

agg_by_h <- agg %>% group_by(model, h) %>%
  summarise(n_origins = n_distinct(origin), n_obs = n(),
            DALY_MAPE = mean(DALY_ape), DALY_coverage = mean(DALY_covered) * 100,
            VLW_MAPE  = mean(VLW_ape),  VLW_coverage  = mean(VLW_covered) * 100,
            mean_relative_width_pct = mean(rel_width_DALY), .groups = "drop") %>%
  arrange(model, h)
write_csv(agg_by_h, file.path(R5_DIR, "R5_aggregate_interval_coverage_by_horizon.csv"))

agg_overall <- agg %>% group_by(model) %>%
  summarise(n_origins = n_distinct(origin), n_obs = n(),
            DALY_MAPE = mean(DALY_ape), DALY_coverage = mean(DALY_covered) * 100,
            VLW_MAPE  = mean(VLW_ape),  VLW_coverage  = mean(VLW_covered) * 100,
            mean_relative_width_pct = mean(rel_width_DALY),
            total_nonpositive_group_draws = sum(n_group_draws_nonpositive),
            .groups = "drop") %>% arrange(DALY_MAPE)
write_csv(agg_overall, file.path(R5_DIR, "R5_aggregate_interval_coverage_overall.csv"))
cat("\n  -- Aggregate construction, pooled over origins and horizons --\n")
print(as.data.frame(agg_overall %>% mutate(across(where(is.numeric), ~ round(.x, 2)))),
      row.names = FALSE)
cat("\n  -- Aggregate construction, by horizon --\n")
print(as.data.frame(agg_by_h %>% mutate(across(where(is.numeric), ~ round(.x, 2)))),
      row.names = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# 6. Major 3 - full disclosure of the joint-simulation construction
# ══════════════════════════════════════════════════════════════════════════════
cat("\n[6] Joint-simulation transparency outputs ...\n")
# Reuse the projection built in section 2 with the model the selection rule chose, so the
# disclosure describes the construction that actually produced the reported numbers.
proj <- primary

# 6a - the residual correlation matrix, before and after the positive-definite adjustment
corr_out <- bind_rows(
  as_tibble(proj$residual_correlation_raw, rownames = "Group") %>% mutate(Matrix = "Raw Pearson", .before = 1),
  as_tibble(proj$residual_correlation,     rownames = "Group") %>% mutate(Matrix = "After eigenvalue clipping", .before = 1))
write_csv(corr_out, file.path(R5_DIR, "R5_residual_correlation_matrix.csv"))
print(as.data.frame(corr_out), row.names = FALSE)

# 6b - eigenvalues before adjustment, which show whether the clip bound ever bound at all
eig <- tibble(component = seq_along(proj$eigenvalues_raw),
              eigenvalue_raw = proj$eigenvalues_raw,
              eigenvalue_after_clip = pmax(proj$eigenvalues_raw, EIGEN_FLOOR),
              clip_was_binding = ifelse(proj$eigenvalues_raw < EIGEN_FLOOR, "Yes", "No"))
write_csv(eig, file.path(R5_DIR, "R5_correlation_eigenvalues.csv"))
print(as.data.frame(eig), row.names = FALSE)

# 6c - measured non-negativity behaviour, by year
write_csv(proj$nonnegativity, file.path(R5_DIR, "R5_simulation_nonnegativity.csv"))
cat("\n  Non-positive draws across all horizons: group-level ",
    sum(proj$nonnegativity$n_group_draws_nonpositive), " of ",
    format(sum(proj$nonnegativity$draws) * 3, big.mark = ","),
    "; aggregate-level ", sum(proj$nonnegativity$n_total_draws_nonpositive), "\n", sep = "")

# 6d - a machine-readable specification of the construction, so the Methods text and the code
#      cannot drift apart again
spec <- tibble(
  Element = c("Estimand", "Group models", "Coupling", "Correlation type",
              "Correlation estimation", "Positive-definite adjustment",
              "Parameter treatment", "Marginal dispersion",
              "Draws per horizon", "Seed rule", "Non-negativity",
              "Temporal structure", "Interval interpretation", "Valuation"),
  Specification = c(
    "Income-group DALYs; VLW obtained by monetising DALY draws",
    paste0("Three income-group series fitted independently with ",
           unique(proj$diagnostics$model)[1]),
    "Draws summed across the three income groups at each horizon",
    "Cross-sectional Pearson correlation of in-sample model residuals between groups",
    "Estimated once on the full 1990-2023 fit, use='pairwise.complete.obs'; held fixed across horizons",
    paste0("Eigenvalue clipping at ", format(EIGEN_FLOOR, scientific = TRUE),
           " after symmetrisation, then cov2cor rescaling; not Higham's algorithm"),
    "Fixed at point estimates; parameter-estimation and model uncertainty are NOT propagated",
    "Backed out of the marginal forecast interval as (upper - lower) / (2 * z_0.975)",
    format(proj$draws, big.mark = ","),
    paste0("set.seed(", proj$seed, " + k) at horizon k; independent stream per horizon"),
    "No truncation applied; the number of non-positive draws is counted and reported",
    "Horizons simulated independently; draws are pointwise per year and are NOT temporal paths",
    "Model-dependent scenario range; the nominal 95% level is a construction parameter only",
    "Fixed 2023 group-specific effective VSLY"))
write_csv(spec, file.path(R5_DIR, "R5_joint_simulation_specification.csv"))

# ══════════════════════════════════════════════════════════════════════════════
# 7. Major 3 - the composition-drift calculation is endpoint-based; say so and bound it
# ══════════════════════════════════════════════════════════════════════════════
cat("\n[7] Composition drift: endpoint basis stated, and compared with a full-series fit ...\n")
vbar <- df_base %>% filter(sex_name == "Both") %>%
  group_by(LMIC_group, year) %>%
  summarise(Vbar = sum(VLW) * 1e9 / sum(DALY), .groups = "drop")

# Endpoint CAGR - the published basis. It uses 1990 and 2023 only and discards the 32 intervening
# observations, so it is sensitive to the choice of endpoints.
# Log-linear OLS - uses every year, and is reported here purely to bound that sensitivity.
drift_cmp <- vbar %>% group_by(LMIC_group) %>%
  summarise(
    Vbar_1990 = Vbar[year == 1990], Vbar_2023 = Vbar[year == 2023],
    endpoint_cagr_pct = ((Vbar_2023 / Vbar_1990)^(1 / 33) - 1) * 100,
    loglinear_cagr_pct = (exp(coef(lm(log(Vbar) ~ year))[2]) - 1) * 100,
    loglinear_r2 = summary(lm(log(Vbar) ~ year))$r.squared,
    .groups = "drop") %>%
  mutate(absolute_difference_pp = loglinear_cagr_pct - endpoint_cagr_pct)
write_csv(drift_cmp, file.path(R5_DIR, "R5_composition_drift_basis_comparison.csv"))
print(as.data.frame(drift_cmp %>% mutate(across(where(is.numeric), ~ round(.x, 4)))),
      row.names = FALSE)

grp_2050_cmp <- proj$groups %>% filter(Year == 2050) %>%
  select(LMIC_group = Group, DALY, VLW_billion)
impact <- map_dfr(c("endpoint_cagr_pct", "loglinear_cagr_pct"), function(basis) {
  a <- drift_cmp %>% transmute(LMIC_group, rate = .data[[basis]] / 100)
  g <- grp_2050_cmp %>% left_join(a, by = "LMIC_group") %>%
    mutate(adj = VLW_billion * (1 + rate)^27)
  tibble(Basis = ifelse(basis == "endpoint_cagr_pct",
                        "Endpoint CAGR, 1990 and 2023 only (published basis)",
                        "Log-linear OLS on all 34 annual observations (bounding check)"),
         VLW_2050_fixed_composition = sum(g$VLW_billion),
         VLW_2050_drift_adjusted = sum(g$adj),
         difference_pct = (sum(g$adj) / sum(g$VLW_billion) - 1) * 100)
})
write_csv(impact, file.path(R5_DIR, "R5_composition_drift_impact_comparison.csv"))
print(as.data.frame(impact %>% mutate(across(where(is.numeric), ~ round(.x, 4)))),
      row.names = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# 8. Minor 1 - data provenance: a source-level query manifest and a file-level manifest
# ══════════════════════════════════════════════════════════════════════════════
# Two tables, because they answer different questions.
#
#   Sheet A (query manifest) is what the reviewer asked for: which source, which query, which
#   codes, which classification vintage, on which date. Everything that can be READ BACK OUT OF
#   THE DOWNLOADED FILES is derived from them here rather than typed from memory, so the manifest
#   cannot drift from the data. The one field that is not recoverable from any file is the date
#   the query was actually run: a file timestamp records when the file was written on this
#   machine, which is not the query date. That field is therefore emitted as an explicit
#   placeholder and the script FAILS LOUDLY at the end if it has not been filled in, so an
#   unfilled manifest cannot be shipped by accident.
#
#   Sheet B (file manifest) is the checksum-level record: every input file, its size, its local
#   timestamp, its MD5, and its shape.
#
# To fill in the dates and the classification vintage, edit
# data_raw/EXTRACTION_DATES.csv (created below on first run). Both fields are
# author knowledge: neither can be recovered from the downloaded files, and a file
# timestamp is not a query date. Every source is served from a fixed URL rather than a versioned
# release, so the date records when the authors downloaded the files.
cat("\n[8] Data provenance: query manifest and file manifest ...\n")

DATES_FILE <- file.path("data_raw", "EXTRACTION_DATES.csv")
if (!file.exists(DATES_FILE)) {
  write_csv(tibble(
    Source = c("Global Burden of Disease Study 2023",
               "Global Burden of Disease Study 2023",
               "World Bank World Development Indicators",
               "World Bank country classification",
               "Natural Earth / GBD location geometry"),
    Dataset = c("Pancreatic cancer DALYs, annual 1990-2023",
                "Healthy life expectancy (HALE), 2023",
                "GDP per capita and total GDP, PPP, current international $",
                "Income-group classification used to define the LMIC set",
                "World map geometry keyed by GBD location_id"),
    Extraction_or_query_date = rep("TO BE SUPPLIED BY THE AUTHORS (YYYY-MM-DD)", 5),
    Classification_vintage = c(
      "n/a", "n/a",
      "PPP, current international $, as published at the access date",
      "TO BE SUPPLIED BY THE AUTHORS (classification vintage at the access date)",
      "n/a")),
    DATES_FILE)
  cat("  Created ", DATES_FILE, " - fill in the extraction dates before submitting.\n", sep = "")
}
dates <- read_csv(DATES_FILE, show_col_types = FALSE)
stopifnot(all(c("Extraction_or_query_date", "Classification_vintage") %in% names(dates)))

# -- derive the GBD query parameters from the downloaded files themselves ----------------------
gbd_files <- list.files(file.path("data_raw", "gbd_daly_yearly"), full.names = TRUE,
                        pattern = "\\.csv$")
gbd_probe <- read_csv(gbd_files[which.max(nchar(gbd_files))], show_col_types = FALSE,
                      progress = FALSE)
u <- function(x) paste(sort(unique(x)), collapse = ", ")
gbd_years <- sort(as.integer(sub("^measure2_DALYs_year([0-9]{4})\\.csv$", "\\1",
                                  basename(gbd_files))))
gbd_query <- paste0(
  "measure=", u(gbd_probe$measure), " (DALYs); ",
  "cause=", u(gbd_probe$cause), " (pancreatic cancer); ",
  "metric=", u(gbd_probe$metric), " (1 = number, 3 = rate); ",
  "sex=", u(gbd_probe$sex), " (1 = male, 2 = female, 3 = both); ",
  "age = ", length(unique(gbd_probe$age)), " groups, ids ", u(gbd_probe$age), "; ",
  "location = ", length(unique(gbd_probe$location)), " GBD locations; ",
  "year = ", min(gbd_years), "-", max(gbd_years),
  " (one file per year, ", length(gbd_files), " files)")

hale <- read_csv(file.path("data_raw", "external_metadata", "HALE.csv"),
                 show_col_types = FALSE, progress = FALSE)
hale_query <- paste0(
  "measure_id=", u(hale$measure_id), " (", u(hale$measure_name), "); ",
  "metric=", u(hale$metric_name), "; ",
  "sex=", u(hale$sex_name), "; ",
  "age = ", length(unique(hale$age_name)), " groups; ",
  "location = ", length(unique(hale$location_id)), " locations; ",
  "year=", u(hale$year))

gdp <- read_csv(file.path("data_raw", "external_metadata", "gdp.csv"),
                show_col_types = FALSE, progress = FALSE)
gdp_ind <- setdiff(names(gdp), c("country", "iso2c", "iso3c", "year"))
gdp_query <- paste0("indicators = ", paste(gdp_ind, collapse = ", "),
                    "; economies = ", length(unique(gdp$iso3c)),
                    "; year = ", min(gdp$year), "-", max(gdp$year))

cls <- read_csv(file.path("data_raw", "external_metadata", "204_with_LMIC.csv"),
                show_col_types = FALSE, progress = FALSE)
cls_query <- paste0("locations = ", nrow(cls), "; flagged LMIC = ", sum(cls$LMIC == 1),
                    "; groups = ", u(cls$LMIC_group[cls$LMIC == 1]))

query_manifest <- dates %>%
  mutate(
    Access_tool_or_portal = c(
      "GBD Results Tool, https://vizhub.healthdata.org/gbd-results/",
      "GBD Results Tool, https://vizhub.healthdata.org/gbd-results/",
      "World Bank World Development Indicators, https://databank.worldbank.org/source/world-development-indicators",
      "World Bank country and lending groups",
      "Bundled with the analysis inputs"),
    Query_parameters = c(gbd_query, hale_query, gdp_query, cls_query,
                         "Polygon geometry keyed by GBD location_id; no query parameters"),
    Indicator_or_cause_codes = c(
      paste0("GBD cause ", u(gbd_probe$cause), ", measure ", u(gbd_probe$measure)),
      paste0("GBD measure_id ", u(hale$measure_id)),
      paste(gdp_ind, collapse = ", "),
      "World Bank income groups: low, lower-middle, upper-middle",
      "n/a"),
    Downloaded_files = c(
      paste0("data_raw/gbd_daly_yearly/measure2_DALYs_year{", min(gbd_years), "..",
             max(gbd_years), "}.csv"),
      "data_raw/external_metadata/HALE.csv",
      "data_raw/external_metadata/gdp.csv",
      "data_raw/external_metadata/204_with_LMIC.csv",
      "data_raw/external_metadata/df_world2.geojson"),
    Licence_and_redistribution = c(
      rep("IHME GBD terms of use; not redistributed with the code", 2),
      "World Bank open data terms (CC BY 4.0); not redistributed with the code",
      "World Bank open data terms (CC BY 4.0); not redistributed with the code",
      "Not redistributed with the code"))
write_csv(query_manifest, file.path(R5_DIR, "R5_query_manifest.csv"))

# -- file-level manifest with checksums and shapes ---------------------------------------------
raw_files <- list.files("data_raw", full.names = TRUE, recursive = TRUE)
raw_files <- raw_files[!grepl("(^|/)\\.DS_Store$|EXTRACTION_DATES\\.csv$", raw_files)]
shape <- function(p) {
  if (!grepl("\\.csv$", p)) return(c(NA_integer_, NA_integer_))
  d <- suppressWarnings(read_csv(p, show_col_types = FALSE, progress = FALSE))
  c(nrow(d), ncol(d))
}
sh <- vapply(raw_files, shape, numeric(2))
manifest <- tibble(
  file = sub("^data_raw/", "", raw_files),
  bytes = file.size(raw_files),
  modified_utc = format(as.POSIXct(file.mtime(raw_files), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
  md5 = unname(tools::md5sum(raw_files)),
  rows = as.integer(sh[1, ]), columns = as.integer(sh[2, ])) %>% arrange(file)
write_csv(manifest, file.path(R5_DIR, "R5_data_provenance_manifest.csv"))
cat("  ", nrow(manifest), " input files catalogued with size, timestamp, MD5 and shape.\n",
    sep = "")

# -- refuse to pass silently while the dates are still placeholders ----------------------------
unfilled <- query_manifest %>%
  filter(grepl("TO BE SUPPLIED", Extraction_or_query_date) |
         grepl("TO BE SUPPLIED", Classification_vintage))
if (nrow(unfilled)) {
  PROVENANCE_INCOMPLETE <<- TRUE
  cat("\n  !! ", nrow(unfilled), " provenance field(s) are still placeholders.\n", sep = "")
  cat("     Reviewer #3, Minor 1 asks for exact extraction dates; a file timestamp is not one.\n")
  cat("     Fill in ", DATES_FILE, " and the classification vintage, then rerun.\n", sep = "")
  cat("     Rows needing attention: ",
      paste(unfilled$Dataset, collapse = "; "), "\n", sep = "")
} else {
  PROVENANCE_INCOMPLETE <<- FALSE
  cat("  All extraction dates and vintages supplied.\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# 9. Major 2 - the MASE definition, written next to the code that computes it
# ══════════════════════════════════════════════════════════════════════════════
mase_doc <- tibble(
  Item = c("Statistic", "Numerator", "Denominator", "Denominator window",
           "Recomputation", "Pooling", "Horizon behaviour", "Reference", "Computed in"),
  Definition = c(
    "Mean absolute scaled error (MASE), non-seasonal form",
    "|y_{t+h} - yhat_{t+h}| for each forecast-actual pair",
    "mean|y_t - y_{t-1}| over the training window, a one-step naive scale",
    "t = 2..n, where n is the length of the training window ending at the forecast origin",
    "The denominator is recomputed at every origin, so the scale never uses data after the origin",
    "Unweighted mean of scaled errors over origins, horizons and series (552 pairs per model)",
    "The denominator is a one-step quantity while the numerator spans h = 1..6, so MASE rises with h by construction; compare only within a fixed horizon",
    "Hyndman and Koehler (2006), International Journal of Forecasting 22(4):679-688",
    "scripts/run_R5_reviewer_analyses.R, section 1, object mase_scale"))
write_csv(mase_doc, file.path(R5_DIR, "R5_MASE_definition.csv"))

writeLines(capture.output(sessionInfo()), file.path(R5_DIR, "sessionInfo.txt"))
cat("\n==================== Done ====================\nOutput directory: ",
    rel_path(R5_DIR), "\n", sep = "")
if (isTRUE(PROVENANCE_INCOMPLETE))
  cat("\nOUTSTANDING: data_raw/EXTRACTION_DATES.csv still contains placeholders.\n",
      "Everything else ran to completion; Reviewer #3's Minor 1 is not satisfied until the\n",
      "exact query dates are entered there and this script is rerun.\n", sep = "")
