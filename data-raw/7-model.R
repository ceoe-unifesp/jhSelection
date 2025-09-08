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

tab02 <- c(models_fips_nounk_inter, models_state_nounk_inter) |>
  fixest::etable()

print(tab01)
print(tab02)
