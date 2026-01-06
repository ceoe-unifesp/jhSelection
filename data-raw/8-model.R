devtools::load_all()

models_fips_nounk_nointer <- purrr::map(c("fe", "mpv"), \(x) {
  fit_model(
    da_model_fips,
    offset_var = x,
    type = "fips",
    remove_unknown = TRUE,
    interaction = FALSE
  )
})
models_state_nounk_nointer <- purrr::map(c("fe", "mpv"), \(x) {
  fit_model(
    da_model_state,
    offset_var = x,
    type = "state",
    remove_unknown = TRUE,
    interaction = FALSE
  )
})
models_fips_nounk_inter <- purrr::map(c("fe", "mpv"), \(x) {
  fit_model(
    da_model_fips,
    offset_var = x,
    type = "fips",
    remove_unknown = TRUE,
    interaction = TRUE
  )
})
models_state_nounk_inter <- purrr::map(c("fe", "mpv"), \(x) {
  fit_model(
    da_model_state,
    offset_var = x,
    type = "state",
    remove_unknown = TRUE,
    interaction = TRUE
  )
})

tab01 <- c(models_fips_nounk_nointer, models_state_nounk_nointer) |>
  fixest::etable()

print(tab01)

tab02 <- c(models_fips_nounk_inter, models_state_nounk_inter) |>
  fixest::etable()

print(tab02)


da_model_state |>
  dplyr::filter(
    year >= min(years_available()[["fe"]]),
    year <= max(years_available()[["fe"]]),
    shr > 0 | fe > 0,
    race != "Unknown/Others",
    sex != "Unknown/Others"
  ) |>
  tidyr::drop_na()


yr_offset <- years_available()[["mpv"]]
yr_shr <- years_available()[["shr"]]
da_model_state |>
  dplyr::filter(
    year >= max(yr_offset[1], yr_shr[1]),
    year <= min(yr_offset[2], yr_shr[2])
  ) |>
  dplyr::mutate(
    ofs = .data[["mpv"]],
    ofs = dplyr::na_if(ofs, 0),
    ofs = ifelse(is.na(ofs), 0 + .5 * (shr > 0), ofs)
  ) |>
  dplyr::filter(
    shr > 0 | ofs > 0,
    race != "Unknown/Others",
    sex != "Unknown/Others"
  ) |>
  tidyr::drop_na()

## log \theta
