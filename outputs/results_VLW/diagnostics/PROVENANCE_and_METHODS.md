# Data provenance (reviewer m1)

- Disease burden: GBD 2023 (Global Burden of Disease Study 2023), pancreatic cancer DALYs,
  Number + Age-standardized Rate, 1990-2023, both sexes and sex-specific, by 5-year age group.
  Source files: data_raw/gbd_daly_yearly/measure2_DALYs_year1990.csv through year2023.csv
  (annual exports from the IHME GBD Results Tool; the canonical workflow reads these files directly).
- HALE: GBD 2023 health-adjusted life expectancy, 2023, by sex and age. Source: data_raw/external_metadata/HALE.csv.
- GDP per capita & total (PPP, current international $), 2023: World Bank World Development Indicators
  (NY.GDP.PCAP.PP.CD, NY.GDP.MKTP.PP.CD). Source: data_raw/external_metadata/gdp.csv.
- Income classification (LIC/LMIC/UMIC) & LMIC flag: data_raw/external_metadata/204_with_LMIC.csv (World Bank FY24).
- VSL reference: US DOT 2023 guidance, VSL = US$13.2 million; US GDP pc (PPP) 2023 = US$82,304.62.

Extraction/analysis date: 2026-07-24

# Method note (reviewers M1, M2, m2, m5)
- Single VSLY definition: VSLY_i = VSL_i / annuity(HALE_i, r), annuity(T,r) = (1-(1+r)^-T)/r,
  with T_i = HALE_i (sex-specific, all-ages) and base r = 0.03. An undiscounted variant (r = 0,
  VSLY = VSL_i / HALE_i) is reported as sensitivity.
- VLW = VSLY x DALY, applied uniformly to every age; age/sex/income totals are obtained by
  summation, so age-specific VLW (manuscript Table 4; generated as Table6_age_specific.csv)
  sums exactly to the headline total (Table 1), and
  both-sex = male + female (Tables 1, 2).
- Because VSLY is uniform across ages, age-specific VLW is proportional to age-specific DALYs;
  the earlier 'VLW peaks at 70-74' result was an artefact of an inconsistent age denominator and
  is not reported.

# R3 forecast and uncertainty policy
- Historical aggregate 95% UIs are not reported because matched GBD draws are unavailable; marginal bounds are never summed.
- DALY is the primary projection estimand. Projected DALYs are monetized with fixed 2023 group effective VSLY values.
- ETS and non-seasonal ARIMA are both projected to 2050. All forecast calls set level=95 explicitly.
- All-LMIC PIs use joint simulation with empirical residual correlation; subgroup endpoints are never summed.
- The income-group rate panel is the unweighted mean of country-specific age-standardized rates.

# Software (reviewer m3)
- R version 4.2.2 (2022-10-31)
- forecast 8.21.1; tidyverse 2.0.0; sf 1.0.16; sandwich 3.1.0
- Forecast diagnostics: outputs/results_VLW/diagnostics/Forecast_diagnostics_all_IE.csv
