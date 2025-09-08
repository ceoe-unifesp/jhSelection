#' Fit the demographic adjustment model
#'
#' This function fits a Poisson or Negative Binomial regression model to estimate
#' the relationship between police-related deaths recorded in different data sources,
#' adjusting for demographic factors such as sex, race, and geographic location.
#'
#' @param da_model A data frame containing the data to be used for modeling.
#'  It should include columns for the counts of police-related deaths from different sources,
#' as well as demographic and geographic variables.
#' @param offset_var A string specifying the name of the variable to be used as an offset in the model.
#' Default is "fe" (Fatal Encounters counts).
#' @param type A string indicating the geographic level for fixed effects.
#' Options are "fips" for county-level fixed effects or "state" for state-level fixed effects.
#' Default is "fips".
#' @param remove_unknown A logical value indicating whether to remove records with "Unknown/Others"
#' Default is TRUE.
#' @param interaction A logical value indicating whether to include interaction terms
#' between sex and race. Default is FALSE.
#'
#' @return A fitted model object from the `fixest` package.
#'
#' @export
fit_model <- function(
  da_model,
  offset_var = "fe",
  type = "fips",
  remove_unknown = TRUE,
  interaction = FALSE
) {
  yr_offset <- years_available()[[offset_var]]
  yr_shr <- years_available()[["shr"]]
  da_model_prep <- da_model |>
    dplyr::filter(
      year >= max(yr_offset[1], yr_shr[1]),
      year <= min(yr_offset[2], yr_shr[2])
    ) |>
    dplyr::mutate(
      ofs = .data[[offset_var]],
      # offset can't be zero
      ofs = dplyr::na_if(ofs, 0),
      ofs = ifelse(is.na(ofs), 0 + .5 * (shr > 0), ofs)
    ) |>
    dplyr::filter(
      shr > 0 | ofs > 0
    ) |>
    tidyr::drop_na()
  if (remove_unknown) {
    da_model_prep <- da_model_prep |>
      dplyr::filter(race != "Unknown/Others", sex != "Unknown/Others")
  }
  if (type == "fips") {
    if (interaction) {
      fm <- shr ~ i(race, sex) | year + fips
    } else {
      fm <- shr ~ i(race) + i(sex) | year + fips
    }
    fm_vcov <- ~ year + fips
  } else if (type == "state") {
    if (interaction) {
      fm <- shr ~ i(race, sex) | year + state
    } else {
      fm <- shr ~ i(race) + i(sex) | year + state
    }
    fm_vcov <- ~ year + state
  }
  if (type %in% c("fips")) {
    model <- fixest::feglm(
      fml = fm,
      data = da_model_prep,
      offset = ~ log(ofs),
      family = "poisson",
      vcov = fm_vcov
    )
  } else {
    model <- fixest::fenegbin(
      fml = fm,
      data = da_model_prep,
      offset = ~ log(ofs),
      vcov = fm_vcov
    )
  }
  return(model)
}
