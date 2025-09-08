# create data-raw directories if they don't exist
paths <- file.path("data-raw", c("parquet", "csv", "rda", "xlsx"))
fs::dir_create(here::here(paths))

# download LEAIC data
piggyback::pb_download(
  "35158-0001-Data.rda",
  dest = here::here("data-raw/rda"),
  repo = "ceoe-unifesp/jhSelection",
  tag = "data",
  overwrite = TRUE
)

# download most recent version of SHR (from shrData package)
piggyback::pb_download(
  "shr.parquet",
  dest = here::here("data-raw/parquet"),
  repo = "ceoe-unifesp/shrData",
  tag = "parquet",
  overwrite = TRUE
)

# alternative: download older version of SHR, used when paper was written

# download FE data
piggyback::pb_download(
  "fatal_encounters.xlsx",
  dest = here::here("data-raw/xlsx"),
  repo = "ceoe-unifesp/jhSelection",
  tag = "data",
  overwrite = TRUE
)

# download MPV data
piggyback::pb_download(
  "Mapping Police Violence.csv",
  dest = here::here("data-raw/csv"),
  repo = "ceoe-unifesp/jhSelection",
  tag = "data",
  overwrite = TRUE
)
