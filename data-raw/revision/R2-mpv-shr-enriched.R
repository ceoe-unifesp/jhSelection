# =============================================================================
# REVISION JLEA-26-0003 -- Enriched MPV and SHR processing + join accounting
# - MPV: keep allegedly_armed / signs_of_mental_illness; quantify the ORI-join
#        loss for the methods appendix (user request: document MPV filtering).
# - SHR: quantify the justifiable-homicide extraction and reproduce counts_shr.
# Addresses: R2-4 (task 23, armed x race descriptive), appendix, R1-5 (Table 2).
# =============================================================================
suppressMessages({
  library(dplyr); library(readr); library(arrow); library(stringr)
  library(lubridate); library(tidyr); library(tibble)
})
devtools::load_all(quiet = TRUE)   # for leaic_ori

# ---------------------------- MPV --------------------------------------------
mpv_raw <- read_csv(here::here("data-raw/csv/Mapping Police Violence.csv"),
                    guess_max = 80000, show_col_types = FALSE)

mpv <- mpv_raw |>
  mutate(
    ori9 = str_sub(ori, 1, 9),
    year = year(dmy(date)),
    race = case_match(race, "Black" ~ "Black", "Hispanic" ~ "Hispanic",
                      "White" ~ "White", .default = "Unknown/Others"),
    sex = case_match(gender, "Male" ~ "Male", "Female" ~ "Female",
                     .default = "Unknown/Others"),
    armed = case_when(
      allegedly_armed %in% c("Allegedly Armed") ~ "Armed",
      allegedly_armed %in% c("Unarmed", "Unarmed/Did Not Have Actual Weapon") ~ "Unarmed",
      allegedly_armed %in% c("Vehicle") ~ "Vehicle",
      TRUE ~ "Unknown"
    )
  )

# ---- ORI-join accounting (appendix) ----
n_total   <- nrow(mpv)
n_ori_ok  <- sum(!is.na(mpv$ori9) & mpv$ori9 != "" & nchar(mpv$ori9) == 9)
mpv_joined <- mpv |> inner_join(leaic_ori, by = c("ori9" = "ori9"))
n_joined  <- nrow(mpv_joined)
cat("======== MPV JOIN ACCOUNTING ========\n")
cat("MPV raw incidents:                 ", n_total, "\n")
cat("with a 9-char ORI populated:       ", n_ori_ok,
    sprintf(" (%.1f%%)\n", 100*n_ori_ok/n_total))
cat("surviving inner_join to LEAIC:     ", n_joined,
    sprintf(" (%.1f%%)\n", 100*n_joined/n_total))
cat("dropped by the ORI join:           ", n_total - n_joined,
    sprintf(" (%.1f%%)\n", 100*(n_total-n_joined)/n_total))
cat("date range:", min(mpv$year, na.rm=TRUE), "-", max(mpv$year, na.rm=TRUE), "\n")

# validate reproduction of counts_mpv
counts_mpv_repro <- mpv_joined |>
  count(state = state.y, fips, year, race, sex, name = "mpv")
load(here::here("data/counts_mpv.rda"))
cat("counts_mpv shipped rows:", nrow(counts_mpv), " reproduced:", nrow(counts_mpv_repro),
    " victims shipped:", sum(counts_mpv$mpv), " reproduced:", sum(counts_mpv_repro$mpv), "\n")

# enriched MPV county-incident file (keeps armed) for task 23
mpv_enriched <- mpv_joined |>
  transmute(state = state.y, fips, year, race, sex, armed,
            mental = signs_of_mental_illness)
saveRDS(mpv_enriched, here::here("data-raw/revision/mpv_enriched.rds"))

# ---- armed x race descriptive (task 23) ----
cat("\n======== ARMED x RACE, MPV (2013-2021, known race & sex) ========\n")
mpv_tab <- mpv_enriched |>
  filter(year >= 2013, year <= 2021, race != "Unknown/Others", sex != "Unknown/Others")
print(mpv_tab |> count(race, armed) |>
        group_by(race) |> mutate(pct = round(100*n/sum(n),1)) |> ungroup() |>
        filter(armed == "Unarmed"))

# ---------------------------- SHR --------------------------------------------
shr_full <- arrow::read_parquet(here::here("data-raw/parquet/shr.parquet")) |> tibble()
cat("\n======== SHR ACCOUNTING ========\n")
cat("SHR raw incident-rows:", nrow(shr_full), "\n")
cat("file_year range:", min(shr_full$file_year), "-", max(shr_full$file_year), "\n")
