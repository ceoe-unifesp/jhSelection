# create data-raw directories if they don't exist
paths <- file.path("data-raw", c("parquet", "csv", "rda", "xlsx"))
fs::dir_create(here::here(paths))

# download LEAIC data
# original data source: https://www.icpsr.umich.edu/web/ICPSR/studies/35158
piggyback::pb_download(
  "35158-0001-Data.rda",
  dest = here::here("data-raw/rda"),
  repo = "ceoe-unifesp/jhSelection",
  tag = "data",
  overwrite = TRUE
)

# Download most recent version of SHR (from shrData package)
piggyback::pb_download(
  "shr.parquet",
  dest = here::here("data-raw/parquet"),
  repo = "ceoe-unifesp/shrData",
  tag = "parquet",
  overwrite = TRUE
)

# Alternative: download older version of SHR,
# used when paper was written, for reproducibility
piggyback::pb_download(
  "shr.parquet",
  dest = here::here("data-raw/parquet"),
  repo = "ceoe-unifesp/jhSelection",
  tag = "data",
  overwrite = TRUE
)

# download FE data
# original data source: https://fatalencounters.org/
# direct link: https://docs.google.com/spreadsheets/d/1dKmaV_JiWcG8XBoRgP8b4e9Eopkpgt7FL7nyspvzAsE/edit?usp=sharing
piggyback::pb_download(
  "fatal_encounters.xlsx",
  dest = here::here("data-raw/xlsx"),
  repo = "ceoe-unifesp/jhSelection",
  tag = "data",
  overwrite = TRUE
)

# download MPV data
# original data source: https://mappingpoliceviolence.org/
# direct link: https://airtable.com/shroOenW19l1m3w0H
# download date: 2025-05-01
piggyback::pb_download(
  "Mapping Police Violence.csv",
  dest = here::here("data-raw/csv"),
  repo = "ceoe-unifesp/jhSelection",
  tag = "data",
  overwrite = TRUE
)
