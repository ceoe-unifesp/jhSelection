#' Get the range of years available for each dataset
#'
#' @return A named list with the range of years available for each dataset
#'
#' @export
years_available <- function() {
  list(
    shr = range(jhSelection::counts_shr$year),
    fe = range(jhSelection::counts_fe$year),
    mpv = range(jhSelection::counts_mpv$year)
  )
}
