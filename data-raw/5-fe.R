usethis::ui_info("Reading FENC data...")

fe <- readxl::read_excel(
  here::here("data-raw/xlsx/fatal_encounters.xlsx"),
  guess_max = 80000
) |>
  janitor::clean_names() |>
  dplyr::filter(highest_level_of_force == "Gunshot")

# counties data
ct_data <- tigris::counties(cb = TRUE) |>
  sf::st_transform(crs = 4326)

fe_clean <- fe |>
  dplyr::mutate(
    race = dplyr::case_when(
      stringr::str_detect(race, "Hispanic") ~ "Hispanic",
      stringr::str_detect(race, "White") ~ "White",
      stringr::str_detect(race, "Black") ~ "Black",
      stringr::str_detect(race_with_imputations, "Hispanic") ~ "Hispanic",
      stringr::str_detect(race_with_imputations, "White") ~ "White",
      stringr::str_detect(race_with_imputations, "Black") ~ "Black",
      .default = "Unknown/Others"
    ),
    sex = dplyr::case_when(
      gender == "Female" ~ "Female",
      gender == "Male" ~ "Male",
      .default = "Unknown/Others"
    ),
    longitude = dplyr::na_if(longitude, "na"),
    latitude = dplyr::na_if(latitude, "na"),
    latitude = stringr::str_remove(latitude, ",$"),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  )

aux_fips <- fe_clean |>
  dplyr::select(unique_id, latitude, longitude) |>
  tidyr::drop_na() |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  sf::st_join(ct_data) |>
  tibble::as_tibble() |>
  dplyr::transmute(unique_id, fips = GEOID, state = STATEFP)

counts_fe <- fe_clean |>
  dplyr::inner_join(aux_fips, "unique_id") |>
  dplyr::mutate(
    year = lubridate::year(date_of_injury_resulting_in_death_month_day_year)
  ) |>
  dplyr::count(state = state.y, fips, year, race, sex, name = "fe")

usethis::use_data(counts_fe, overwrite = TRUE)
