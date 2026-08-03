# =============================================================================
# REVISION JLEA-26-0003 -- Enriched Fatal Encounters processing
# Runs the spatial join ONCE and keeps, at the incident level, every variable
# the referees asked about so that downstream robustness checks (FE force
# filter, race imputation, armed status) can be produced by simple aggregation.
#
# Addresses task-list items: Int-1 (task 26), R1-6 (task 17), R2-4 (task 23).
# Validates against the shipped data/counts_fe.rda before anything is trusted.
# =============================================================================
suppressMessages({
  library(dplyr); library(readxl); library(janitor); library(stringr)
  library(sf); library(tigris); library(lubridate); library(tibble); library(tidyr)
})
options(tigris_use_cache = TRUE)

harmonize_race <- function(x) {
  case_when(
    str_detect(x, "Hispanic") ~ "Hispanic",
    str_detect(x, "White")    ~ "White",
    str_detect(x, "Black")    ~ "Black",
    TRUE                       ~ "Unknown/Others"
  )
}

fe <- read_excel(here::here("data-raw/xlsx/fatal_encounters.xlsx"), guess_max = 80000) |>
  clean_names()

fe_clean <- fe |>
  mutate(
    # race from the observed field only (imputed cases fall to Unknown/Others)
    race_obs = harmonize_race(race),
    # race allowing the imputation fallback -- THIS is what 5-fe.R does
    race_imp = case_when(
      str_detect(race, "Hispanic") ~ "Hispanic",
      str_detect(race, "White")    ~ "White",
      str_detect(race, "Black")    ~ "Black",
      str_detect(race_with_imputations, "Hispanic") ~ "Hispanic",
      str_detect(race_with_imputations, "White")    ~ "White",
      str_detect(race_with_imputations, "Black")    ~ "Black",
      TRUE ~ "Unknown/Others"
    ),
    race_is_imputed = (race_obs == "Unknown/Others") & (race_imp != "Unknown/Others"),
    sex = case_when(gender == "Female" ~ "Female", gender == "Male" ~ "Male",
                    TRUE ~ "Unknown/Others"),
    # force filters
    f_gunshot = highest_level_of_force == "Gunshot",                 # code (5-fe.R)
    f_deadly  = intended_use_of_force_developing == "Deadly force",  # paper text / Finch
    # armed status, cleaned
    armed = case_when(
      armed_unarmed %in% c("Armed", "Arrmed") ~ "Armed",
      armed_unarmed == "Unarmed"              ~ "Unarmed",
      TRUE                                     ~ "Unknown"
    ),
    longitude = na_if(longitude, "na"),
    latitude  = na_if(latitude, "na"),
    latitude  = str_remove(latitude, ",$"),
    longitude = as.numeric(longitude),
    latitude  = as.numeric(latitude)
  )

ct_data <- counties(cb = TRUE, progress_bar = FALSE) |> st_transform(crs = 4326)

aux_fips <- fe_clean |>
  select(unique_id, latitude, longitude) |>
  drop_na() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_join(ct_data) |>
  as_tibble() |>
  transmute(unique_id, fips = GEOID, state = STATEFP)

fe_enriched <- fe_clean |>
  inner_join(aux_fips, "unique_id") |>
  mutate(year = year(date_of_injury_resulting_in_death_month_day_year)) |>
  transmute(unique_id, state = state.y, fips, year,
            race_obs, race_imp, race_is_imputed, sex,
            f_gunshot, f_deadly, armed)

saveRDS(fe_enriched, here::here("data-raw/revision/fe_enriched.rds"))

# ---- VALIDATION: reproduce shipped counts_fe (gunshot filter, imputed race) --
counts_fe_repro <- fe_enriched |>
  filter(f_gunshot) |>
  count(state, fips, year, race = race_imp, sex, name = "fe")

load(here::here("data/counts_fe.rda"))
shipped <- counts_fe |> arrange(state, fips, year, race, sex)
repro   <- counts_fe_repro |> arrange(state, fips, year, race, sex)

cat("shipped rows:", nrow(shipped), " reproduced rows:", nrow(repro), "\n")
cat("total FE victims shipped:", sum(shipped$fe), " reproduced:", sum(repro$fe), "\n")
joined <- full_join(shipped, repro, by = c("state","fips","year","race","sex"),
                    suffix = c("_ship","_repro"))
cat("cells identical:", sum(joined$fe_ship == joined$fe_repro, na.rm = TRUE),
    " / ", nrow(joined), "\n")
cat("cells differing or missing:", sum(is.na(joined$fe_ship) | is.na(joined$fe_repro) |
    joined$fe_ship != joined$fe_repro), "\n")

# ---- Quick counts for the memo -------------------------------------------
cat("\n--- FE filter comparison (all years) ---\n")
cat("Gunshot (code):          ", sum(fe_enriched$f_gunshot, na.rm=TRUE), "\n")
cat("Deadly force (paper):    ", sum(fe_enriched$f_deadly,  na.rm=TRUE), "\n")
cat("Both:                    ", sum(fe_enriched$f_gunshot & fe_enriched$f_deadly, na.rm=TRUE), "\n")
cat("Gunshot not Deadly:      ", sum(fe_enriched$f_gunshot & !fe_enriched$f_deadly, na.rm=TRUE), "\n")
cat("Deadly not Gunshot:      ", sum(!fe_enriched$f_gunshot & fe_enriched$f_deadly, na.rm=TRUE), "\n")
cat("\n--- Race imputation (gunshot sample) ---\n")
gs <- fe_enriched |> filter(f_gunshot)
cat("gunshot incidents:", nrow(gs), "\n")
cat("race imputed (rescued from Unknown):", sum(gs$race_is_imputed),
    sprintf(" (%.1f%%)\n", 100*mean(gs$race_is_imputed)))
cat("Unknown even after imputation:", sum(gs$race_imp=="Unknown/Others"),
    sprintf(" (%.1f%%)\n", 100*mean(gs$race_imp=="Unknown/Others")))
