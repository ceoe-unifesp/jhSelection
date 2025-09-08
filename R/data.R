#' State year race sex panel of police-related deaths (model input)
#'
#' `da_model_state` collapses three independent sources of U.S. police-involved
#' fatalities—the FBI *Supplementary Homicide Reports* (SHR), the
#' *Fatal Encounters* (FE) project, and the *Mapping Police Violence* (MPV)
#' database—into a common \emph{state × year × race × sex} taxonomy suitable for
#' regression and visualization.  Counts are harmonised with the Bureau of
#' Justice Statistics *Law Enforcement Agency Identifiers Crosswalk* (LEAIC).
#'
#' @format A tibble with **5 907 rows** and **7 variables**:
#' \describe{
#'   \item{state}{Two-digit state FIPS code (character).}
#'   \item{year}{Calendar year, 1976–2025 (numeric).}
#'   \item{race}{Victim race (`factor`): `"White"`, `"Black"`,
#'     `"Hispanic/Latino"`, `"Asian/Pacific Islander"`,
#'     `"Native American/Alaska Native"`, `"Unknown/Other"`.}
#'   \item{sex}{Victim sex (`factor`): `"Male"`, `"Female"`, `"Unknown/Others"`.}
#'   \item{shr}{Events counted in SHR. 0 = verified none; `NA` = source missing.}
#'   \item{fe}{Events counted in Fatal Encounters.  Interpretation as above.}
#'   \item{mpv}{Events counted in Mapping Police Violence.  Interpretation as above.}
#' }
#'
#' @source
#' FBI Uniform Crime Reporting Program, *Supplementary Homicide Reports*,
#' 1985–2025, NACJD ICPSR 3795.
#' Burghart, D. B. (2024). *Fatal Encounters*, v 1.4. Harvard Dataverse,
#' \doi{10.7910/DVN/ZJF9IW}.
#' Mapping Police Violence (2025 release), \url{https://mappingpoliceviolence.org/download}.
#'
#' @keywords datasets
"da_model_state"

#' County–year–race–sex panel of police-related deaths (model input)
#'
#' `da_model_fips` extends `da_model_state` to the 3 142 U.S. counties (plus
#' county-equivalents) using LEAIC to map agency ORIs to five-digit county FIPS
#' codes.  The structure is identical except for the additional `fips` field.
#'
#' @format A tibble with **22 214 rows** and **8 variables**:
#' \describe{
#'   \item{state}{Two-digit state FIPS code (character).}
#'   \item{fips}{Five-digit county FIPS code (character).}
#'   \item{year}{Calendar year, 1976–2025 (numeric).}
#'   \item{race}{Victim race (`factor`).}
#'   \item{sex}{Victim sex (`factor`).}
#'   \item{shr}{SHR count.}
#'   \item{fe}{Fatal Encounters count.}
#'   \item{mpv}{Mapping Police Violence count.}
#' }
#'
#' @inherit da_model_state details
#' @inherit da_model_state source
#' @keywords datasets
"da_model_fips"

#' SHR-only county counts of police-involved homicides
#'
#' `counts_shr` provides raw counts of police-involved homicides from the FBI
#' *Supplementary Homicide Reports* at the \emph{county × year × race × sex}
#' level prior to any reconciliation with other sources.
#'
#' @format A tibble with **9 705 rows** and **6 variables**:
#' \describe{
#'   \item{state}{Two-digit state FIPS code.}
#'   \item{fips}{County FIPS code.}
#'   \item{year}{Year, 1976–2023.}
#'   \item{race}{Victim race (character).}
#'   \item{sex}{Victim sex (character).}
#'   \item{shr}{Count of events in SHR (integer).}
#' }
#'
#' @source FBI UCR, *Supplementary Homicide Reports* 1976–2025.
#' @keywords datasets
"counts_shr"

#' MPV-only county counts of police-related deaths
#'
#' `counts_mpv` contains raw tallies from the *Mapping Police Violence* project
#' at the \emph{county × year × race × sex} granularity before harmonisation.
#'
#' @format A tibble with **8 918 rows** and **6 variables**:
#' \describe{
#'   \item{state}{Two-digit state FIPS code.}
#'   \item{fips}{County FIPS code.}
#'   \item{year}{Year, 2013–2024.}
#'   \item{race}{Victim race (character).}
#'   \item{sex}{Victim sex (character).}
#'   \item{mpv}{Count of events in MPV (integer).}
#' }
#'
#' @source Mapping Police Violence, 2025 release.
#' @keywords datasets
"counts_mpv"

#' Fatal Encounters-only county counts of police-related deaths
#'
#' `counts_fe` reports raw incident counts from the crowd-sourced
#' *Fatal Encounters* database at the \emph{county × year × race × sex} level.
#'
#' @format A tibble with **14 437 rows** and **6 variables**:
#' \describe{
#'   \item{state}{Two-digit state FIPS code.}
#'   \item{fips}{County FIPS code.}
#'   \item{year}{Year, 2000–2024.}
#'   \item{race}{Victim race (character).}
#'   \item{sex}{Victim sex (character).}
#'   \item{fe}{Count of events in Fatal Encounters (integer).}
#' }
#'
#' @source Burghart, D. B. (2024). *Fatal Encounters*, v 1.4.
#' @keywords datasets
"counts_fe"

#' Law Enforcement Agency Identifiers Crosswalk (ORI and FIPS)
#'
#' `leaic_ori` is the Bureau of Justice Statistics LEAIC table linking
#' Originating Agency Identifiers (ORI) to county and state FIPS codes.
#'
#' @format A tibble with **22 019 rows** and **4 variables**:
#' \describe{
#'   \item{ori7}{Seven-character ORI (legacy form).}
#'   \item{ori9}{Nine-character ORI (full form).}
#'   \item{fips}{County FIPS code (character).}
#'   \item{state}{Two-digit state FIPS code (character).}
#' }
#'
#' @source U.S. Bureau of Justice Statistics (2021). *Law Enforcement Agency
#' Identifiers Crosswalk*.
#' @keywords datasets
"leaic_ori"
