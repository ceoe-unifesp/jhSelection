devtools::load_all()

usethis::ui_info("Data tidying...")

# build_da() performs the full outer join, sets reference levels, and returns the
# county-year and state-year tables. It lives in R/tidy.R so the Fatal Encounters
# robustness variants (8-model.R) can be rebuilt through the same code path.
da <- build_da(counts_shr, counts_fe, counts_mpv)

da_model_fips <- da$fips
da_model_state <- da$state

usethis::use_data(da_model_fips, overwrite = TRUE)
usethis::use_data(da_model_state, overwrite = TRUE)
