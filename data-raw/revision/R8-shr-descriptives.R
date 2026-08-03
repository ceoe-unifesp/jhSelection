# =============================================================================
# TASK 16 (R1-5, Table 2) + justification for the gunshot filter (Task 26) +
# weapon descriptive (Task 23).
# (1) SHR firearm share -> justifies restricting FE/MPV to firearm deaths.
# (2) Estimation-sample descriptives (the ACTUAL analysis panels), replacing the
#     ambiguous full-join Table 2.
# =============================================================================
suppressMessages({library(dplyr); library(tidyr); library(arrow); library(stringr); library(tibble)})
devtools::load_all(quiet = TRUE)
source(here::here("data-raw/revision/R3-model-harness.R"))
sink(here::here("output/revision/task16_descriptives.txt"))

# ---- (1) SHR weapon composition for justifiable POLICE homicides -------------
shr_full <- arrow::read_parquet(here::here("data-raw/parquet/shr.parquet")) |> tibble()
shr_pjh <- shr_full |>
  filter(file_year >= 1976, file_year <= 2023) |>
  filter(if_any(matches("offender_([0-9]+)_circ"), \(c) c %in% c("80","81"))) |>
  mutate(circ = coalesce(offender_01_circ, offender_02_circ),
         circ = case_when(circ=="81"~"Police", circ=="80"~"Civilian", TRUE~"Other")) |>
  filter(circ == "Police")
cat("=== SHR justifiable POLICE homicides: weapon used by officer (offender_01_weapon) ===\n")
wtab <- shr_pjh |> count(w = offender_01_weapon, sort = TRUE) |> mutate(pct = round(100*n/sum(n),1))
print(head(wtab, 15))
firearm_codes <- c("11","12","13","14","15")  # handgun, rifle, shotgun, other gun, firearm type unk
fa <- shr_pjh |> mutate(is_fa = str_sub(offender_01_weapon,1,2) %in% firearm_codes)
cat(sprintf("\nFirearm share of SHR justifiable police homicides: %.1f%%\n", 100*mean(fa$is_fa, na.rm=TRUE)))
cat("(SHR weapon codes 11-15 = firearms. This justifies benchmarking against firearm/'Gunshot' deaths.)\n")

# ---- (2) Estimation-sample descriptives (Table 2 replacement) ----------------
describe_panel <- function(da, bench, geo, cc = 0.5) {
  d <- prep(da, bench, remove_unknown = TRUE, cc = cc)
  ident <- d |> group_by(across(all_of(if (geo=="fips") "fips" else "state"))) |>
    summarise(any_pos = any(shr>0), .groups="drop")
  tibble(
    panel = sprintf("%s / %s", toupper(bench), geo),
    cells = nrow(d),
    used_in_model = sum(d[[if(geo=="fips")"fips" else "state"]] %in%
                          ident[[if(geo=="fips")"fips" else "state"]][ident$any_pos]),
    geo_units = n_distinct(d[[if (geo=="fips") "fips" else "state"]]),
    years = paste0(min(d$year),"-",max(d$year)),
    shr_mean = round(mean(d$shr),2), shr_max = max(d$shr),
    shr_zero_pct = round(100*mean(d$shr==0),1),
    ofs_mean = round(mean(d$ofs),2), ofs_max = round(max(d$ofs),1)
  )
}
cat("\n=== ESTIMATION-SAMPLE DESCRIPTIVES (2013-2021, known race & sex) ===\n")
tab <- bind_rows(
  describe_panel(da_model_fips, "fe", "fips"),
  describe_panel(da_model_fips, "mpv", "fips"),
  describe_panel(da_model_state, "fe", "state"),
  describe_panel(da_model_state, "mpv", "state")
)
print(as.data.frame(tab))

cat("\n=== Cell composition by race x sex (FE/county estimation sample) ===\n")
d <- prep(da_model_fips, "fe", remove_unknown = TRUE)
print(d |> count(race, sex) |> pivot_wider(names_from=sex, values_from=n, values_fill=0))

cat("\n=== Clarify the N=23,869 full-join sample (current Table 2) ===\n")
load(here::here("data/da_model_fips.rda"))
cat("da_model_fips (full outer join, all years/sources):\n")
cat("  rows:", nrow(da_model_fips), " -> this is the number in the current Table 2 caption\n")
cat("  cells with SHR>0:", sum(da_model_fips$shr>0, na.rm=TRUE),
    " FE>0:", sum(da_model_fips$fe>0, na.rm=TRUE),
    " MPV>0:", sum(da_model_fips$mpv>0, na.rm=TRUE), "\n")
cat("  year span:", min(da_model_fips$year), "-", max(da_model_fips$year), "\n")
sink()
cat("written output/revision/task16_descriptives.txt\n")
