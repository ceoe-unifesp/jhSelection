devtools::load_all()

# =============================================================================
# Models and robustness for the paper.
#
#   Section 1  Main tables (race/sex main effects; race-by-sex subgroups)
#   Section 2  Family x aggregation x benchmark grid (appendix)
#   Section 3  Continuity-correction sensitivity (appendix)
#   Section 4  Extensive vs intensive margin decomposition (appendix)
#   Section 5  FE force-filter and race-imputation robustness (appendix)
#   Section 6  Role of the fixed effects: no-geo-FE and reporting-intensity strata
#   Section 7  Estimation-sample descriptives and armed-vs-unarmed context
#
# Every model goes through fit_model() (R/model.R); the FE robustness variants
# are rebuilt with build_da() (R/tidy.R) from the enriched `fe_incidents`.
# =============================================================================

# ---- Section 1: main tables --------------------------------------------------
usethis::ui_info("Main models...")

models_fips_nointer <- purrr::map(c("fe", "mpv"), \(x) {
  fit_model(da_model_fips, offset_var = x, type = "fips", interaction = FALSE)
})
models_state_nointer <- purrr::map(c("fe", "mpv"), \(x) {
  fit_model(da_model_state, offset_var = x, type = "state", interaction = FALSE)
})
models_fips_inter <- purrr::map(c("fe", "mpv"), \(x) {
  fit_model(da_model_fips, offset_var = x, type = "fips", interaction = TRUE)
})
models_state_inter <- purrr::map(c("fe", "mpv"), \(x) {
  fit_model(da_model_state, offset_var = x, type = "state", interaction = TRUE)
})

tab_main <- c(models_fips_nointer, models_state_nointer) |> fixest::etable()
tab_subgroups <- c(models_fips_inter, models_state_inter) |> fixest::etable()
print(tab_main)
print(tab_subgroups)

# ---- Section 2: family x aggregation x benchmark grid (appendix) ------------
usethis::ui_info("Appendix: family x aggregation x benchmark grid...")

grid <- tidyr::expand_grid(
  bench = c("fe", "mpv"),
  family = c("poisson", "negbin"),
  geo = c("fips", "state")
)
grid_models <- purrr::pmap(grid, \(bench, family, geo) {
  da <- if (geo == "fips") da_model_fips else da_model_state
  fit_model(da, offset_var = bench, type = geo, family = family)
})
names(grid_models) <- with(grid, sprintf("%s.%s.%s", family, geo, bench))
print(fixest::etable(grid_models, fitstat = ~ n + bic))

# ---- Section 3: continuity-correction sensitivity (appendix) ----------------
usethis::ui_info("Appendix: continuity-correction sensitivity...")

coef_row <- function(m, pat) {
  ct <- summary(m)$coeftable
  r <- grep(pat, rownames(ct))
  if (!length(r)) return(NA_character_)
  sprintf("%+.4f (p=%.3f)", ct[r, 1], ct[r, 4])
}
cc_grid <- tidyr::expand_grid(bench = c("fe", "mpv"), cc = c(0.25, 0.5, 1.0))
cc_tab <- purrr::pmap_dfr(cc_grid, \(bench, cc) {
  m <- fit_model(da_model_fips, offset_var = bench, type = "fips",
                 family = "poisson", cc = cc)
  tibble::tibble(
    benchmark = toupper(bench), cc = cc,
    Black = coef_row(m, "Black"), Hispanic = coef_row(m, "Hispanic"),
    Female = coef_row(m, "Female"), n = attr(m, "n_obs")
  )
})
cc_drop <- purrr::map_dfr(c("fe", "mpv"), \(bench) {
  m <- fit_model(da_model_fips, offset_var = bench, type = "fips",
                 family = "poisson", drop_bench_zero = TRUE)
  tibble::tibble(
    benchmark = toupper(bench), cc = NA_real_,
    Black = coef_row(m, "Black"), Hispanic = coef_row(m, "Hispanic"),
    Female = coef_row(m, "Female"), n = attr(m, "n_obs")
  )
})
print(dplyr::bind_rows(cc_tab, cc_drop))

# ---- Section 4: extensive vs intensive margin decomposition (appendix) ------
usethis::ui_info("Appendix: margin decomposition...")

margin_decomp <- purrr::map_dfr(c("fe", "mpv"), \(bench) {
  d <- da_model_fips |>
    dplyr::filter(year >= 2013, year <= 2021,
                  race != "Unknown/Others", sex != "Unknown/Others")
  d$b <- d[[bench]]
  d$b[is.na(d$b)] <- 0
  d$shr[is.na(d$shr)] <- 0
  cy <- d |>
    dplyr::group_by(state, fips, year) |>
    dplyr::summarise(shr = sum(shr), b = sum(b), .groups = "drop") |>
    dplyr::filter(b > 0) |>
    dplyr::mutate(reports = shr > 0)
  gap_tot <- sum(cy$b) - sum(cy$shr)
  gap_ext <- sum(cy$b[!cy$reports])
  gap_int <- sum(cy$b[cy$reports]) - sum(cy$shr[cy$reports])
  tibble::tibble(
    benchmark = toupper(bench),
    county_years = nrow(cy),
    pct_non_reporting = round(100 * mean(!cy$reports), 1),
    capture_rate = round(100 * sum(cy$shr) / sum(cy$b), 1),
    extensive_pct = round(100 * gap_ext / gap_tot, 1),
    intensive_pct = round(100 * gap_int / gap_tot, 1),
    within_reporting_capture = round(100 * sum(cy$shr[cy$reports]) /
      sum(cy$b[cy$reports]), 1)
  )
})
print(margin_decomp)

# ---- Section 5: FE force-filter and race-imputation robustness (appendix) ---
usethis::ui_info("Appendix: FE force-filter and imputation robustness...")

make_counts_fe <- function(force = c("gunshot", "deadly", "all"),
                           race = c("imp", "obs")) {
  force <- match.arg(force)
  race <- match.arg(race)
  d <- fe_incidents
  if (force == "gunshot") d <- dplyr::filter(d, f_gunshot)
  if (force == "deadly") d <- dplyr::filter(d, f_deadly)
  rc <- if (race == "imp") "race_imp" else "race_obs"
  dplyr::count(d, state, fips, year, race = .data[[rc]], sex, name = "fe")
}
fit_fe_variant <- function(force, race) {
  cfe <- make_counts_fe(force, race)
  da <- build_da(counts_shr, cfe, counts_mpv)
  m <- fit_model(da$fips, offset_var = "fe", type = "fips", family = "poisson")
  tibble::tibble(
    force = force, race = race, fe_victims = sum(cfe$fe),
    Black = coef_row(m, "Black"), Hispanic = coef_row(m, "Hispanic"),
    Female = coef_row(m, "Female"), n = attr(m, "n_obs")
  )
}
fe_robust <- dplyr::bind_rows(
  fit_fe_variant("gunshot", "imp"),
  fit_fe_variant("deadly", "imp"),
  fit_fe_variant("all", "imp"),
  fit_fe_variant("gunshot", "obs")
)
print(fe_robust)

# ---- Section 6: role of the fixed effects (appendix) ------------------------
usethis::ui_info("Appendix: role of the fixed effects...")

# (a) no geographic fixed effects (year effects only)
no_geo <- purrr::map_dfr(c("fe", "mpv"), \(bench) {
  m_geo <- fit_model(da_model_fips, offset_var = bench, type = "fips", family = "poisson")
  m_non <- fit_model(da_model_fips, offset_var = bench, type = "none", family = "poisson")
  tibble::tibble(
    benchmark = toupper(bench),
    county_FE_Hispanic = coef_row(m_geo, "Hispanic"),
    year_only_Hispanic = coef_row(m_non, "Hispanic"),
    county_FE_Female = coef_row(m_geo, "Female"),
    year_only_Female = coef_row(m_non, "Female")
  )
})
print(no_geo)

# (b) FE-drop accounting: cells in all-zero-SHR counties absorbed by the FE
fe_drop <- purrr::map_dfr(c("fe", "mpv"), \(bench) {
  d <- prep_model_data(da_model_fips, offset_var = bench)
  pos <- d |>
    dplyr::group_by(fips) |>
    dplyr::summarise(any_pos = any(shr > 0), .groups = "drop")
  drop_obs <- sum(d$fips %in% pos$fips[!pos$any_pos])
  tibble::tibble(
    benchmark = toupper(bench), cells = nrow(d),
    all_zero_counties = sum(!pos$any_pos), obs_dropped = drop_obs,
    pct_dropped = round(100 * drop_obs / nrow(d), 1)
  )
})
print(fe_drop)

# (c) stratify by within-county reporting intensity (FE, Poisson/county)
d <- da_model_fips |>
  dplyr::filter(year >= 2013, year <= 2021,
                race != "Unknown/Others", sex != "Unknown/Others")
d$fe[is.na(d$fe)] <- 0
d$shr[is.na(d$shr)] <- 0
cint <- d |>
  dplyr::group_by(fips) |>
  dplyr::summarise(shr = sum(shr), fe = sum(fe), .groups = "drop") |>
  dplyr::filter(fe > 0) |>
  dplyr::mutate(
    ratio = shr / fe,
    stratum = dplyr::case_when(
      ratio == 0 ~ "0 non-reporting",
      ratio <= 0.5 ~ "1 low (<=50%)",
      ratio <= 1 ~ "2 mid (50-100%)",
      TRUE ~ "3 high (>100%)"
    )
  )
da_str <- da_model_fips |> dplyr::left_join(dplyr::select(cint, fips, stratum), "fips")
strata_tab <- purrr::map_dfr(c("1 low (<=50%)", "2 mid (50-100%)"), \(st) {
  sub <- dplyr::filter(da_str, stratum == st)
  m <- fit_model(sub, offset_var = "fe", type = "fips", family = "poisson")
  tibble::tibble(stratum = st, Female = coef_row(m, "Female"), n = attr(m, "n_obs"))
})
print(strata_tab)

# ---- Section 7: estimation-sample descriptives and armed context ------------
usethis::ui_info("Appendix: estimation-sample descriptives and armed context...")

describe_panel <- function(da, bench, geo) {
  m <- fit_model(da, offset_var = bench, type = geo,
                 family = if (geo == "fips") "poisson" else "negbin")
  d <- prep_model_data(da, bench)
  gcol <- if (geo == "fips") "fips" else "state"
  tibble::tibble(
    benchmark = toupper(bench), level = geo,
    obs = attr(m, "n_obs"),
    geo_units = dplyr::n_distinct(d[[gcol]]),
    pct_shr_zero = round(100 * mean(d$shr == 0), 0),
    mean_shr = round(mean(d$shr), 2),
    mean_bench = round(mean(d$ofs), 2)
  )
}
panels <- dplyr::bind_rows(
  describe_panel(da_model_fips, "fe", "fips"),
  describe_panel(da_model_fips, "mpv", "fips"),
  describe_panel(da_model_state, "fe", "state"),
  describe_panel(da_model_state, "mpv", "state")
)
print(panels)

# armed-vs-unarmed context by race (MPV, 2013-2021)
armed_tab <- mpv_incidents |>
  dplyr::filter(year >= 2013, year <= 2021,
                race != "Unknown/Others", sex != "Unknown/Others") |>
  dplyr::count(race, armed) |>
  dplyr::group_by(race) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 1)) |>
  dplyr::ungroup() |>
  dplyr::filter(armed == "Unarmed")
print(armed_tab)
