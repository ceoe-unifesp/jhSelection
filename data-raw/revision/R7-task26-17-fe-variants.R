# =============================================================================
# TASKS 26 (Int-1) & 17 (R1-6): FE force-filter and race-imputation robustness.
# Build FE count variants from the enriched incident file and refit the main
# county-level Poisson models, holding everything else fixed.
#   Force filter:  Gunshot (code) | Deadly force (paper text/Finch) | All FE
#   Race source:   with imputation (code) | observed only
# =============================================================================
source(here::here("data-raw/revision/R3-model-harness.R"))
devtools::load_all(quiet = TRUE)   # counts_shr, counts_mpv
fe_enr <- readRDS(here::here("data-raw/revision/fe_enriched.rds"))
sink(here::here("output/revision/task26_17_fe_variants.txt"))

make_counts_fe <- function(force = c("gunshot","deadly","all"),
                           race = c("imp","obs")) {
  force <- match.arg(force); race <- match.arg(race)
  d <- fe_enr
  if (force == "gunshot") d <- dplyr::filter(d, f_gunshot)
  if (force == "deadly")  d <- dplyr::filter(d, f_deadly)
  rc <- if (race == "imp") "race_imp" else "race_obs"
  d |> dplyr::count(state, fips, year, race = .data[[rc]], sex, name = "fe")
}

fit_variant <- function(force, race, geo = "fips", fam = "poisson") {
  cfe <- make_counts_fe(force, race)
  da <- build_da(cfe, counts_shr, counts_mpv)
  m <- refit(da[[geo]], "fe", fam, geo)
  ct <- summary(m)$coeftable
  g <- function(p){r<-grep(p,rownames(ct)); sprintf("%+.4f(p%.3f)", ct[r,1], ct[r,4])}
  list(n_victims = sum(cfe$fe), n = attr(m,"n_prep"),
       Black = g("Black"), Hispanic = g("Hispanic"), Female = g("Female"))
}

cat("=============== TASK 26: FE FORCE FILTER (county, Poisson, imputed race) ===============\n")
cat("Baseline = Gunshot (what the code does); paper text claims 'Deadly force'.\n\n")
for (f in c("gunshot","deadly","all")) {
  r <- fit_variant(f, "imp")
  cat(sprintf("  force=%-8s FE victims=%5d  n=%d  Black %s  Hispanic %s  Female %s\n",
              f, r$n_victims, r$n, r$Black, r$Hispanic, r$Female))
}

cat("\n=============== TASK 17: RACE IMPUTATION (county, Poisson, gunshot filter) ===============\n")
cat("Baseline = with imputation (code); alternative = observed race only.\n\n")
for (rc in c("imp","obs")) {
  r <- fit_variant("gunshot", rc)
  lab <- if (rc=="imp") "with-imputation" else "observed-only  "
  cat(sprintf("  race=%s FE victims=%5d  n=%d  Black %s  Hispanic %s  Female %s\n",
              lab, r$n_victims, r$n, r$Black, r$Hispanic, r$Female))
}

cat("\n(For reference, the state-level NegBin under the same variants:)\n")
for (f in c("gunshot","deadly")) {
  r <- fit_variant(f, "imp", geo="state", fam="negbin")
  cat(sprintf("  state/negbin force=%-8s Black %s Hispanic %s Female %s\n",
              f, r$Black, r$Hispanic, r$Female))
}
sink()
cat("written output/revision/task26_17_fe_variants.txt\n")
