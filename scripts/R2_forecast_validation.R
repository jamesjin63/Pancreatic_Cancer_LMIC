suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(forecast)
  library(tibble)
})

root <- normalizePath(".")
daly_file <- file.path(root, "data_processed/merged.csv")
hale_file <- file.path(root, "data_raw/external_metadata/HALE.csv")
gdp_file <- file.path(root, "data_raw/external_metadata/gdp.csv")
lmic_file <- file.path(root, "data_raw/external_metadata/204_with_LMIC.csv")
out_dir <- file.path(root, "outputs/R2")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

VSL_USA <- 13.2e6
GDP_pc_USA <- 82304.62
discount_rate <- 0.03
annuity <- function(T, r) ifelse(T <= 0, NA_real_, ifelse(r == 0, T, (1 - (1 + r)^(-T)) / r))

gbd_to_wb <- c(
  "Bolivia (Plurinational State of)"="Bolivia","Congo"="Congo, Rep.",
  "Côte d'Ivoire"="Cote d'Ivoire",
  "Democratic People's Republic of Korea"="Korea, Dem. People's Rep.",
  "Democratic Republic of the Congo"="Congo, Dem. Rep.",
  "Egypt"="Egypt, Arab Rep.","Gambia"="Gambia, The",
  "Iran (Islamic Republic of)"="Iran, Islamic Rep.","Kyrgyzstan"="Kyrgyz Republic",
  "Lao People's Democratic Republic"="Lao PDR",
  "Micronesia (Federated States of)"="Micronesia, Fed. Sts.",
  "Palestine"="West Bank and Gaza","Republic of Moldova"="Moldova",
  "Saint Lucia"="St. Lucia","Saint Vincent and the Grenadines"="St. Vincent and the Grenadines",
  "Somalia"="Somalia, Fed. Rep.","Syrian Arab Republic"="Syrian Arab Republic",
  "Türkiye"="Turkiye","United Republic of Tanzania"="Tanzania","Yemen"="Yemen, Rep."
)
norm_name <- function(x) x %>%
  str_replace_all("[‘’]", "'") %>%
  str_replace_all(" ", " ") %>%
  iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
  str_squish()

df_lmic <- read_csv(lmic_file, show_col_types = FALSE) %>%
  filter(LMIC == 1) %>%
  mutate(wb = ifelse(location_name %in% names(gbd_to_wb), gbd_to_wb[location_name], location_name),
         wb = case_when(
           location_id == 155 ~ "Turkiye",
           location_id == 205 ~ "Cote d'Ivoire",
           TRUE ~ wb
         ),
         wb_n = norm_name(wb)) %>%
  select(location_id, location_name, LMIC_group, wb, wb_n)

df_gdp <- read_csv(gdp_file, show_col_types = FALSE) %>%
  filter(year == 2023, !is.na(NY.GDP.PCAP.PP.CD), !is.na(NY.GDP.MKTP.PP.CD)) %>%
  transmute(cn = norm_name(country), GDP_pc = NY.GDP.PCAP.PP.CD, GDP_tot = NY.GDP.MKTP.PP.CD)

df_econ <- df_lmic %>%
  left_join(df_gdp, by = c("wb_n" = "cn")) %>%
  filter(!is.na(GDP_pc))

excluded <- df_lmic %>%
  anti_join(df_econ %>% select(location_id), by = "location_id") %>%
  transmute(Country = location_name, `Location ID` = location_id,
            `Income group` = LMIC_group, `World Bank matched name` = wb,
            `Reason for exclusion` = "Missing 2023 GDP per capita and/or total GDP in World Bank data")
write_csv(excluded, file.path(out_dir, "R2_excluded_LMICs.csv"))

df_hale <- read_csv(hale_file, show_col_types = FALSE) %>%
  filter(metric_name == "Years", year == 2023, age_name == "All ages", sex_id %in% c(1, 2)) %>%
  select(location_id, sex_id, HALE = val)

vsly <- df_econ %>%
  select(location_id, GDP_pc) %>%
  inner_join(df_hale, by = "location_id") %>%
  filter(!is.na(HALE), HALE > 0) %>%
  mutate(VSL_i = VSL_USA * (GDP_pc / GDP_pc_USA),
         VSLY = VSL_i / annuity(HALE, discount_rate))

daly <- read_csv(daly_file, show_col_types = FALSE) %>%
  filter(cause_name == "Pancreatic cancer",
         measure_name == "DALYs (Disability-Adjusted Life Years)",
         metric_name == "Number",
         age_name == "All ages",
         sex_id %in% c(1, 2),
         location_id %in% df_econ$location_id) %>%
  select(location_id, sex_id, sex_name, year, DALY = val)

series_base <- daly %>%
  inner_join(vsly, by = c("location_id", "sex_id")) %>%
  inner_join(df_econ %>% select(location_id, LMIC_group), by = "location_id") %>%
  mutate(VLW = DALY * VSLY / 1e9)

income_series <- series_base %>%
  group_by(series = LMIC_group, year) %>%
  summarise(VLW = sum(VLW), DALY = sum(DALY), .groups = "drop")
sex_series <- series_base %>%
  group_by(series = sex_name, year) %>%
  summarise(VLW = sum(VLW), .groups = "drop")

calc_errors <- function(actual, pred) {
  err <- actual - pred
  tibble(MAE = mean(abs(err)), RMSE = sqrt(mean(err^2)), MAPE = mean(abs(err / actual)) * 100)
}

validate_one <- function(x, value_col, label) {
  x <- arrange(x, year)
  train <- x %>% filter(year <= 2017)
  test <- x %>% filter(year >= 2018, year <= 2023)
  y_train <- ts(train[[value_col]], start = min(train$year), frequency = 1)
  y_full <- ts(x[[value_col]], start = min(x$year), frequency = 1)
  actual <- test[[value_col]]

  fit_ets <- ets(y_train, model = "AAN", damped = TRUE)
  fc_ets <- forecast(fit_ets, h = nrow(test))
  fit_arima <- auto.arima(y_train, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  fc_arima <- forecast(fit_arima, h = nrow(test))

  full_fit <- ets(y_full, model = "AAN", damped = TRUE)
  lb <- Box.test(residuals(full_fit), lag = min(10, length(residuals(full_fit)) - 1), type = "Ljung-Box")

  bind_rows(
    calc_errors(actual, as.numeric(fc_ets$mean)) %>%
      mutate(series = label, outcome = value_col, model = "ETS(A,Ad,N)", holdout = "2018-2023",
             full_series_LjungBox_p = as.numeric(lb$p.value), full_series_RMSE = accuracy(full_fit)["Training set", "RMSE"]),
    calc_errors(actual, as.numeric(fc_arima$mean)) %>%
      mutate(series = label, outcome = value_col, model = paste0("auto.arima: ", arimaorder(fit_arima) %>% paste(collapse = ",")),
             holdout = "2018-2023", full_series_LjungBox_p = NA_real_, full_series_RMSE = NA_real_)
  )
}

validation_parts <- c(
  unname(lapply(split(income_series, income_series$series), function(x) bind_rows(
    validate_one(x, "VLW", unique(x$series)),
    validate_one(x, "DALY", unique(x$series))
  ))),
  unname(lapply(split(sex_series, sex_series$series), function(x) validate_one(x, "VLW", unique(x$series))))
)

validation <- bind_rows(validation_parts) %>%
  select(series, outcome, model, holdout, MAE, RMSE, MAPE, full_series_RMSE, full_series_LjungBox_p)

write_csv(validation, file.path(out_dir, "R2_forecast_holdout_validation_IE1.csv"))

simulate_total_pi <- function(df, value_col, years_keep = c(2025, 2030, 2035, 2040, 2045, 2050),
                              nsim = 50000, seed = 20260707) {
  set.seed(seed)
  groups <- c("Low income", "Lower middle income", "Upper middle income")
  h <- 27
  fits <- lapply(groups, function(g) {
    y <- df %>% filter(series == g) %>% arrange(year) %>% pull(.data[[value_col]])
    fit <- ets(ts(y, start = 1990, frequency = 1), model = "AAN", damped = TRUE)
    fc <- forecast(fit, h = h)
    list(fit = fit, fc = fc, residuals = as.numeric(residuals(fit)))
  })
  names(fits) <- groups
  resid_mat <- do.call(cbind, lapply(fits, `[[`, "residuals"))
  corr <- suppressWarnings(cor(resid_mat, use = "pairwise.complete.obs"))
  corr[is.na(corr)] <- 0
  diag(corr) <- 1

  years <- 2024:2050
  rows <- lapply(seq_along(years), function(k) {
    means <- sapply(fits, function(z) as.numeric(z$fc$mean[k]))
    lowers <- sapply(fits, function(z) as.numeric(z$fc$lower[k, 1]))
    uppers <- sapply(fits, function(z) as.numeric(z$fc$upper[k, 1]))
    ses <- pmax((uppers - lowers) / (2 * qnorm(0.975)), .Machine$double.eps)
    cov_mat <- diag(ses) %*% corr %*% diag(ses)
    z <- matrix(rnorm(nsim * length(groups)), ncol = length(groups))
    draws <- sweep(z %*% chol(cov_mat), 2, means, "+")
    total <- rowSums(draws)
    tibble(
      Year = years[k],
      Outcome = value_col,
      `Point forecast` = sum(means),
      `Lower 95% PI` = unname(quantile(total, 0.025)),
      `Upper 95% PI` = unname(quantile(total, 0.975)),
      Method = "Joint simulation using income-group ETS forecast variances and empirical residual correlation"
    )
  })
  bind_rows(rows) %>% filter(Year %in% years_keep)
}

cov_pi <- bind_rows(
  simulate_total_pi(income_series, "VLW", seed = 20260707),
  simulate_total_pi(income_series, "DALY", seed = 20260708)
)
write_csv(cov_pi, file.path(out_dir, "R2_covariance_aware_all_LMIC_PI_IE1.csv"))

cat("Wrote R2_excluded_LMICs.csv, R2_forecast_holdout_validation_IE1.csv, and R2_covariance_aware_all_LMIC_PI_IE1.csv\n")
