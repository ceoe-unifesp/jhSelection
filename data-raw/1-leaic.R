load(here::here("data-raw/rda/35158-0001-Data.rda"))

leaic <- tibble::as_tibble(da35158.0001) |>
  janitor::clean_names() |>
  dplyr::mutate(ori_code = as.character(ori7))

leaic_ori <- leaic |>
  dplyr::mutate(
    ori7 = as.character(ori7),
    ori9 = as.character(ori9),
    fips = as.character(fips),
    state = as.character(fips_st)
  ) |>
  dplyr::filter(ori7 != "-1     ") |>
  dplyr::select(ori7, ori9, fips, state) |>
  dplyr::distinct()

usethis::use_data(leaic_ori, overwrite = TRUE)
