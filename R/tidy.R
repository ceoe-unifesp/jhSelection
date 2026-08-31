#' Build the county- and state-level analysis datasets
#'
#' Full-outer-joins the three aggregated count tables on
#' `state, fips, year, race, sex`, sets the reference levels for `race` and
#' `sex`, and returns both the county-level table (missing SHR set to zero) and
#' the state-level aggregate. Factored out of `data-raw/6-tidy.R` so that the
#' Fatal Encounters robustness variants (alternative force filters and race
#' imputation) can be rebuilt through the same code path as the main data.
#'
#' @param counts_shr,counts_fe,counts_mpv Aggregated victim counts by
#'   `state, fips, year, race, sex`.
#'
#' @return A list with `fips` (county-year) and `state` (state-year) data frames.
#' @export
build_da <- function(counts_shr, counts_fe, counts_mpv) {
  da_full <- list(counts_shr, counts_fe, counts_mpv) |>
    purrr::reduce(\(x, y) {
      dplyr::full_join(x, y, c("state", "fips", "year", "race", "sex"))
    }) |>
    dplyr::mutate(
      race = forcats::fct_relevel(
        race, c("White", "Black", "Hispanic", "Unknown/Others")
      ),
      sex = forcats::fct_relevel(sex, c("Male", "Female", "Unknown/Others"))
    )

  da_fips <- da_full |>
    tidyr::replace_na(list(shr = 0))

  da_state <- da_full |>
    dplyr::group_by(state, year, race, sex) |>
    dplyr::summarise(
      shr = sum(shr, na.rm = TRUE),
      fe = sum(fe, na.rm = TRUE),
      mpv = sum(mpv, na.rm = TRUE),
      .groups = "drop"
    )

  list(fips = da_fips, state = da_state)
}
