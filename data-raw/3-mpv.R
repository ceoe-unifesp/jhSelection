devtools::load_all()

usethis::ui_info("Reading MPV data...")

mpv <- readr::read_csv(
  here::here("data-raw/csv/Mapping Police Violence.csv"),
  guess_max = 80000,
  show_col_types = FALSE
)

counts_mpv <- mpv |>
  dplyr::mutate(
    ori = stringr::str_sub(ori, 1, 9),
    year = lubridate::year(lubridate::dmy(date))
  ) |>
  dplyr::mutate(
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
    )
  ) |>
  dplyr::inner_join(leaic_ori, by = c("ori" = "ori9")) |>
  dplyr::count(state = state.y, fips, year, race, sex, name = "mpv")

usethis::use_data(counts_mpv, overwrite = TRUE)
