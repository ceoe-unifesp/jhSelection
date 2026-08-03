#' Prepare the estimation sample for the reporting-bias model
#'
#' Aligns the SHR outcome with a benchmark offset (FE or MPV), restricts to the
#' common 2013--2021 window, and applies the continuity correction for cells in
#' which the benchmark is zero. Factored out of [fit_model()] so that the main
#' models and every robustness variant share exactly one data-preparation path.
#'
#' The continuity correction replaces a zero (or missing) benchmark with `cc`
#' **only** for cells where the SHR records at least one victim; it is never
#' added to the SHR outcome, and cells in which both the SHR and the benchmark
#' are zero are dropped. This mirrors the description in the paper's Empirical
#' Strategy section.
#'
#' @param da_model Analysis data frame with columns `shr`, `fe`, `mpv`, `year`,
#'   `race`, `sex`, and a geographic identifier (`fips` and/or `state`).
#' @param offset_var Benchmark used as the offset, `"fe"` or `"mpv"`.
#' @param remove_unknown Drop `"Unknown/Others"` race and sex cells. Default `TRUE`.
#' @param cc Continuity correction applied to zero-benchmark cells. Default `0.5`.
#' @param drop_bench_zero If `TRUE`, drop cells whose benchmark is zero instead of
#'   applying the continuity correction (a robustness check). Default `FALSE`.
#'
#' @return A prepared data frame with the offset column `ofs`.
#' @export
prep_model_data <- function(
  da_model,
  offset_var = "fe",
  remove_unknown = TRUE,
  cc = 0.5,
  drop_bench_zero = FALSE
) {
  d <- da_model |>
    dplyr::filter(year >= 2013, year <= 2021) |>
    dplyr::mutate(
      ofs = .data[[offset_var]],
      ofs = dplyr::na_if(ofs, 0),
      bench_zero = is.na(ofs),
      # correction hits only cells where the SHR sees a victim the benchmark missed
      ofs = ifelse(bench_zero, 0 + cc * (shr > 0), ofs)
    ) |>
    dplyr::filter(shr > 0 | ofs > 0)
  if (drop_bench_zero) {
    d <- dplyr::filter(d, !bench_zero)
  }
  d <- d |>
    dplyr::select(-dplyr::any_of(c("fe", "mpv"))) |>
    tidyr::drop_na(ofs, shr, race, sex)
  if (remove_unknown) {
    d <- dplyr::filter(d, race != "Unknown/Others", sex != "Unknown/Others")
  }
  d
}

#' Fit the demographic adjustment model
#'
#' Fits a Poisson or Negative Binomial regression estimating how the SHR count
#' relates to a more complete benchmark (FE or MPV), with the benchmark as an
#' offset and fixed effects for year and geography. The reported coefficients are
#' log reporting ratios relative to White males; a negative value means the group
#' is underreported in the SHR.
#'
#' @param da_model Analysis data frame (see [prep_model_data()]).
#' @param offset_var Benchmark used as the offset, `"fe"` (default) or `"mpv"`.
#' @param type Geographic fixed effects: `"fips"` (county, default), `"state"`,
#'   or `"none"` (year effects only; used to show what the geographic fixed
#'   effects are doing).
#' @param remove_unknown Drop `"Unknown/Others"` cells. Default `TRUE`.
#' @param interaction If `TRUE`, estimate one coefficient per race-by-sex cell
#'   (subgroup reporting rates) instead of separate race and sex effects.
#'   Default `FALSE`.
#' @param family Count family: `"auto"` (default) selects Poisson or Negative
#'   Binomial by BIC, reproducing the published tables; `"poisson"` or `"negbin"`
#'   force the family so it is not confounded with the level of geographic
#'   aggregation.
#' @param cc,drop_bench_zero Passed to [prep_model_data()] for the continuity-
#'   correction robustness checks.
#'
#' @return A fitted model object from the `fixest` package, with attributes
#'   `family_used` and `n_obs` (rows in the prepared sample).
#'
#' @export
fit_model <- function(
  da_model,
  offset_var = "fe",
  type = "fips",
  remove_unknown = TRUE,
  interaction = FALSE,
  family = c("auto", "poisson", "negbin"),
  cc = 0.5,
  drop_bench_zero = FALSE
) {
  family <- match.arg(family)
  type <- match.arg(type, c("fips", "state", "none"))

  da_model_prep <- prep_model_data(
    da_model, offset_var, remove_unknown, cc, drop_bench_zero
  )

  rhs <- if (interaction) "i(race, sex)" else "i(race) + i(sex)"
  fe_part <- switch(type, fips = "year + fips", state = "year + state", none = "year")
  fm <- stats::as.formula(paste("shr ~", rhs, "|", fe_part))
  fm_vcov <- switch(
    type,
    fips = stats::as.formula("~ year + fips"),
    state = stats::as.formula("~ year + state"),
    none = stats::as.formula("~ year")
  )

  fit_poisson <- function() {
    fixest::feglm(
      fml = fm, data = da_model_prep, offset = ~ log(ofs),
      family = "poisson", vcov = fm_vcov
    )
  }
  fit_negbin <- function() {
    fixest::fenegbin(
      fml = fm, data = da_model_prep, offset = ~ log(ofs), vcov = fm_vcov
    )
  }

  if (family == "poisson") {
    model <- fit_poisson()
    fam_used <- "Poisson"
  } else if (family == "negbin") {
    model <- fit_negbin()
    fam_used <- "Neg. Bin."
  } else {
    # family == "auto": choose by BIC (reproduces the published behaviour)
    model_poisson <- fit_poisson()
    model_negbin <- fit_negbin()
    bic_poisson <- stats::BIC(model_poisson)
    bic_negbin <- stats::BIC(model_negbin)
    usethis::ui_info(
      "Comparing BIC: Poisson = {round(bic_poisson, 2)}, Negative Binomial = {round(bic_negbin, 2)}"
    )
    if (bic_poisson < bic_negbin) {
      model <- model_poisson
      fam_used <- "Poisson"
    } else {
      model <- model_negbin
      fam_used <- "Neg. Bin."
    }
  }
  attr(model, "family_used") <- fam_used
  attr(model, "n_obs") <- nrow(da_model_prep)
  model
}
