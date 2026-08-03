devtools::load_all()

usethis::ui_info("Reading MPV data...")

mpv <- readr::read_csv(
  here::here("data-raw/csv/Mapping Police Violence.csv"),
  guess_max = 80000,
  show_col_types = FALSE
)

mpv_clean <- mpv |>
  dplyr::mutate(
    ori = stringr::str_sub(ori, 1, 9),
    year = lubridate::year(lubridate::dmy(date)),
    race = dplyr::case_match(
      race,
      "Black" ~ "Black",
      "Hispanic" ~ "Hispanic",
      "White" ~ "White",
      .default = "Unknown/Others"
    ),
    sex = dplyr::case_match(
      gender,
      "Male" ~ "Male",
      "Female" ~ "Female",
      .default = "Unknown/Others"
    ),
    # armed status is retained for the armed-vs-unarmed context descriptive,
    # since the SHR justifiable-homicide file carries no such flag
    armed = dplyr::case_when(
      allegedly_armed == "Allegedly Armed" ~ "Armed",
      allegedly_armed %in% c("Unarmed", "Unarmed/Did Not Have Actual Weapon") ~ "Unarmed",
      allegedly_armed == "Vehicle" ~ "Vehicle",
      .default = "Unknown"
    )
  )

# ORI-join accounting: linking by the 9-digit ORI is itself an inclusion filter
n_total <- nrow(mpv_clean)
n_ori <- sum(!is.na(mpv_clean$ori) & nchar(mpv_clean$ori) == 9)
mpv_joined <- mpv_clean |>
  dplyr::inner_join(leaic_ori, by = c("ori" = "ori9"))
usethis::ui_info(
  "MPV incidents: {n_total}; with a 9-digit ORI {n_ori} \\
   ({round(100 * n_ori / n_total, 1)}%); matched to LEAIC {nrow(mpv_joined)} \\
   ({round(100 * nrow(mpv_joined) / n_total, 1)}%)"
)

# enriched incident-level table (keeps armed status) for the context descriptive
mpv_incidents <- mpv_joined |>
  dplyr::transmute(state = state.y, fips, year, race, sex, armed)

usethis::use_data(mpv_incidents, overwrite = TRUE)

counts_mpv <- mpv_joined |>
  dplyr::count(state = state.y, fips, year, race, sex, name = "mpv")

usethis::use_data(counts_mpv, overwrite = TRUE)
