devtools::load_all()

usethis::ui_info("Reading SHR data...")

shr_full <- arrow::read_parquet(
  here::here("data-raw/parquet/shr.parquet")
) |>
  tibble::tibble()

# PJH CIRCUMSTANCE DATA
shr_pjh <- shr_full |>
  dplyr::filter(file_year >= 1976, file_year <= 2023) |>
  # filter for justifiable homicides
  dplyr::filter(dplyr::if_any(
    dplyr::matches("offender_([0-9]+)_circ"),
    # Filter for rows where circ is 80 (civilian) or 81 (police)
    \(circ) circ %in% c("80", "81")
  )) |>
  # transform other circumstances to NA, because its not our focus
  dplyr::mutate(dplyr::across(
    dplyr::matches("offender_([0-9]+)_circ"),
    \(circ) dplyr::if_else(circ %in% c("80", "81"), circ, NA_character_)
  ))

shr_pjh_circ <- shr_pjh |>
  # two person is enough to get justifiable homicide circumstances correctly
  dplyr::mutate(circ = dplyr::coalesce(offender_01_circ, offender_02_circ)) |>
  dplyr::mutate(
    circ = dplyr::case_when(
      circ == "81" ~ "Police",
      circ == "80" ~ "Civilian",
      TRUE ~ "Other"
    )
  ) |>
  dplyr::mutate(victims = 1 + victim_count)

# obs: for years 1976 -> 1979, we did not have data about ethnicity
counts_shr <- shr_pjh_circ |>
  dplyr::inner_join(leaic_ori, by = c("ori_code" = "ori7")) |>
  dplyr::filter(circ == "Police") |>
  tidyr::pivot_longer(
    cols = dplyr::matches("victim_[0-9]+_(race|sex|ethnic)"),
    names_to = "victim",
    values_to = "value"
  ) |>
  tidyr::drop_na(value) |>
  tidyr::separate(victim, c("dummy", "victim", "type"), sep = "_") |>
  tidyr::pivot_wider(
    names_from = type,
    values_from = value
  ) |>
  dplyr::mutate(
    race = dplyr::case_when(
      ethnic == "H" ~ "Hispanic",
      race == "B" ~ "Black",
      race == "W" ~ "White",
      .default = "Unknown/Others"
    ),
    sex = dplyr::case_when(
      sex == "M" ~ "Male",
      sex == "F" ~ "Female",
      .default = "Unknown/Others"
    )
  ) |>
  dplyr::count(
    state,
    fips,
    year = file_year,
    race,
    sex,
    name = "shr"
  )

usethis::use_data(counts_shr, overwrite = TRUE)
