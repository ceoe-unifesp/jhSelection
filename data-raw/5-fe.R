usethis::ui_info("Reading FENC data...")

# We keep every Fatal Encounters record here (no early force filter) and carry,
# at the incident level, the variables the analysis needs for the primary model
# and for the robustness checks: the observed vs imputed race, the two force
# filters, and the armed status. The primary `counts_fe` (firearm deaths,
# imputed race) is then produced from this enriched table, so the main data and
# the robustness variants share one construction path.

fe <- readxl::read_excel(
  here::here("data-raw/xlsx/fatal_encounters.xlsx"),
  guess_max = 80000
) |>
  janitor::clean_names()

harmonize_race <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "Hispanic") ~ "Hispanic",
    stringr::str_detect(x, "White") ~ "White",
    stringr::str_detect(x, "Black") ~ "Black",
    .default = "Unknown/Others"
  )
}

fe_clean <- fe |>
  dplyr::mutate(
    # race from the observed field only (imputed cases fall to Unknown/Others)
    race_obs = harmonize_race(race),
    # race allowing the imputation fallback -- this is the primary definition
    race_imp = dplyr::case_when(
      stringr::str_detect(race, "Hispanic") ~ "Hispanic",
      stringr::str_detect(race, "White") ~ "White",
      stringr::str_detect(race, "Black") ~ "Black",
      stringr::str_detect(race_with_imputations, "Hispanic") ~ "Hispanic",
      stringr::str_detect(race_with_imputations, "White") ~ "White",
      stringr::str_detect(race_with_imputations, "Black") ~ "Black",
      .default = "Unknown/Others"
    ),
    race_is_imputed = (race_obs == "Unknown/Others") & (race_imp != "Unknown/Others"),
    sex = dplyr::case_when(
      gender == "Female" ~ "Female",
      gender == "Male" ~ "Male",
      .default = "Unknown/Others"
    ),
    # force filters: gunshot is the primary (firearm) definition; deadly force is
    # the FE "intended use of force" classification used as a robustness check
    f_gunshot = highest_level_of_force == "Gunshot",
    f_deadly = intended_use_of_force_developing == "Deadly force",
    armed = dplyr::case_when(
      armed_unarmed %in% c("Armed", "Arrmed") ~ "Armed",
      armed_unarmed == "Unarmed" ~ "Unarmed",
      .default = "Unknown"
    ),
    longitude = dplyr::na_if(longitude, "na"),
    latitude = dplyr::na_if(latitude, "na"),
    latitude = stringr::str_remove(latitude, ",$"),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  )

# counties data
ct_data <- tigris::counties(cb = TRUE) |>
  sf::st_transform(crs = 4326)

aux_fips <- fe_clean |>
  dplyr::select(unique_id, latitude, longitude) |>
  tidyr::drop_na() |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  sf::st_join(ct_data) |>
  tibble::as_tibble() |>
  dplyr::transmute(unique_id, fips = GEOID, state = STATEFP)

# enriched incident-level table (one row per FE incident with a county match)
fe_incidents <- fe_clean |>
  dplyr::inner_join(aux_fips, "unique_id") |>
  dplyr::mutate(
    year = lubridate::year(date_of_injury_resulting_in_death_month_day_year)
  ) |>
  dplyr::transmute(
    unique_id, state = state.y, fips, year,
    race_obs, race_imp, race_is_imputed, sex,
    f_gunshot, f_deadly, armed
  )

usethis::use_data(fe_incidents, overwrite = TRUE)

# primary aggregation: firearm ("Gunshot") deaths, imputed race
counts_fe <- fe_incidents |>
  dplyr::filter(f_gunshot) |>
  dplyr::count(state, fips, year, race = race_imp, sex, name = "fe")

usethis::use_data(counts_fe, overwrite = TRUE)

usethis::ui_info(
  "FE incidents: {nrow(fe_incidents)}; gunshot {sum(fe_incidents$f_gunshot)}, \\
   deadly-force {sum(fe_incidents$f_deadly)}, imputed-race (gunshot) \\
   {sum(fe_incidents$race_is_imputed & fe_incidents$f_gunshot)}"
)
