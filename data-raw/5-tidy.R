devtools::load_all()

usethis::ui_info("Data tidying...")

da_list <- list(
  shr = counts_shr,
  fe = counts_fe,
  mpv = counts_mpv
)

da_full <- da_list |>
  purrr::reduce(\(x, y) {
    dplyr::full_join(x, y, c("state", "fips", "year", "race", "sex"))
  }) |>
  dplyr::mutate(
    race = forcats::fct_relevel(
      race,
      c("White", "Black", "Hispanic", "Unknown/Others")
    ),
    sex = forcats::fct_relevel(sex, c("Male", "Female", "Unknown/Others"))
  )

da_model_fips <- da_full |>
  tidyr::replace_na(list(shr = 0))

da_model_state <- da_full |>
  dplyr::group_by(state, year, race, sex) |>
  dplyr::summarise(
    shr = sum(shr, na.rm = TRUE),
    fe = sum(fe, na.rm = TRUE),
    mpv = sum(mpv, na.rm = TRUE),
    .groups = "drop"
  )

usethis::use_data(da_model_fips, overwrite = TRUE)
usethis::use_data(da_model_state, overwrite = TRUE)
