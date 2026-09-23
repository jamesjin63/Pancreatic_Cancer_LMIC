# data_raw — source data, checksums and acquisition

All sources were downloaded on **5 April 2026** (`EXTRACTION_DATES.csv`).
`CHECKSUMS.md5` lists the MD5 of every input file used for the submitted analysis, including the
files that are not redistributed here. After downloading, verify with

```bash
cd data_raw && md5sum -c CHECKSUMS.md5          # Linux
cd data_raw && md5 -r $(awk '{print $2}' CHECKSUMS.md5) | diff - <(awk '{print $1" "$2}' CHECKSUMS.md5)   # macOS
```

## Redistribution

| File | Source | In this repository? |
|---|---|---|
| `gbd_daly_yearly/measure2_DALYs_year{1990..2023}.csv` | IHME, GBD 2023 Results tool | **No** — IHME terms of use; download as below |
| `external_metadata/HALE.csv` | IHME, GBD 2023 Results tool | **No** — IHME terms of use; download as below |
| `external_metadata/gdp.csv` | World Bank WDI (CC BY 4.0) | Yes, with attribution |
| `external_metadata/204_with_LMIC.csv` | GBD location list (204 location_id) with the World Bank FY2026 income group joined by the authors | Yes |
| `external_metadata/df_world2.geojson` | Natural Earth country polygons (public domain) keyed by GBD location_id by the authors | Yes |

## Acquiring the GBD files

**DALYs** — <https://vizhub.healthdata.org/gbd-results/>, GBD 2023:
measure = DALYs (measure_id 2); cause = Pancreatic cancer (cause_id 456); metric = Number and Rate
(metric_id 1, 3); sex = Male, Female, Both (1, 2, 3); age = the 23 age groups with age_id 1, 6-20,
22, 27, 30, 31, 32, 158, 235; location = all 204 countries and territories; one download per
year, 1990-2023, saved as `gbd_daly_yearly/measure2_DALYs_year<YYYY>.csv`.

**HALE** — same tool, GBD 2023: measure = HALE (measure_id 28); metric = Years; sex = Male, Female;
all age groups; all 204 locations; years 1990 and 2023; saved as `external_metadata/HALE.csv`.

The complete query parameters are recorded in `outputs/R5/R5_query_manifest.csv`, and file sizes,
row and column counts and MD5s in `outputs/R5/R5_data_provenance_manifest.csv`.

## Income classification

The LMIC set uses the **World Bank FY2026 income classification (released 1 July 2025)**, which was
the classification in force on the 5 April 2026 access date. The file itself is consistent with that
vintage: for example Algeria, Iran, Mongolia and Ukraine are upper-middle income (reclassified in
July 2024), Cabo Verde and Samoa are upper-middle income, Namibia is lower-middle income and
Costa Rica is high income (reclassified in July 2025).
