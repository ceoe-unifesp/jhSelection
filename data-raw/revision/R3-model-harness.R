# =============================================================================
# REVISION JLEA-26-0003 -- Model harness (sourced by the analysis scripts)
# Generalises fit_model() so family and geographic FE are chosen EXPLICITLY
# (not confounded via BIC), and exposes the continuity-correction constant and
# the drop-benchmark-zero-cells option, so the editor's and internal points can
# be answered with matched specifications.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(forcats); library(fixest)})

# Rebuild the merged analysis data from a given FE count table (swap-in for
# robustness variants), mirroring data-raw/6-tidy.R exactly.
build_da <- function(counts_fe_var, counts_shr, counts_mpv) {
  da_full <- list(counts_shr, counts_fe_var, counts_mpv) |>
    purrr::reduce(\(x, y) dplyr::full_join(x, y, c("state","fips","year","race","sex"))) |>
    mutate(
      race = fct_relevel(race, c("White","Black","Hispanic","Unknown/Others")),
      sex  = fct_relevel(sex,  c("Male","Female","Unknown/Others"))
    )
  da_fips <- da_full |> replace_na(list(shr = 0))
  da_state <- da_full |>
    group_by(state, year, race, sex) |>
    summarise(shr = sum(shr, na.rm=TRUE), fe = sum(fe, na.rm=TRUE),
              mpv = sum(mpv, na.rm=TRUE), .groups = "drop")
  list(fips = da_fips, state = da_state)
}

# Prepare the estimation frame with an explicit continuity-correction constant
# `cc` and an option to drop cells where the benchmark is zero.
prep <- function(da_model, offset_var = "fe", remove_unknown = TRUE,
                 cc = 0.5, drop_bench_zero = FALSE) {
  d <- da_model |>
    filter(year >= 2013, year <= 2021) |>
    mutate(
      ofs_raw = .data[[offset_var]],
      ofs_raw = na_if(ofs_raw, 0),
      bench_zero = is.na(ofs_raw),
      ofs = ifelse(bench_zero, 0 + cc * (shr > 0), ofs_raw)
    ) |>
    filter(shr > 0 | ofs > 0)
  if (drop_bench_zero) d <- d |> filter(!bench_zero)
  d <- d |> select(-c(fe, mpv)) |> drop_na(ofs, shr, race, sex)
  if (remove_unknown) d <- d |> filter(race != "Unknown/Others", sex != "Unknown/Others")
  d
}

# Fit one model with EXPLICIT family and geographic fixed effects.
# family in {"poisson","negbin"}; geo in {"fips","state","none"}.
refit <- function(da_model, offset_var = "fe", family = "poisson", geo = "fips",
                  interaction = FALSE, cc = 0.5, drop_bench_zero = FALSE,
                  remove_unknown = TRUE) {
  d <- prep(da_model, offset_var, remove_unknown, cc, drop_bench_zero)
  rhs <- if (interaction) "i(race, sex)" else "i(race) + i(sex)"
  fe_part <- switch(geo, fips = "year + fips", state = "year + state", none = "year")
  fm <- stats::as.formula(paste("shr ~", rhs, "|", fe_part))
  vc <- switch(geo, fips = ~year + fips, state = ~year + state, none = ~year)
  if (family == "poisson") {
    m <- fixest::feglm(fm, d, offset = ~log(ofs), family = "poisson", vcov = vc)
  } else {
    m <- fixest::fenegbin(fm, d, offset = ~log(ofs), vcov = vc)
  }
  attr(m, "n_prep") <- nrow(d)
  m
}
