# =============================================================================
# TASK 27 (Int-2): the continuity correction is not what Sec.4 describes.
# Code sets offset = 0.5 ONLY when the benchmark is 0 AND shr>0 (i.e. SHR-only
# cells), never adds 0.5 to all counts, and drops shr==0 & bench==0 cells.
# Here we (a) count the SHR-only cells by race/sex, (b) test sensitivity to the
# constant cc in {0.25,0.5,1.0}, (c) refit dropping benchmark-zero cells.
# =============================================================================
source(here::here("data-raw/revision/R3-model-harness.R"))
devtools::load_all(quiet = TRUE)
sink(here::here("output/revision/task27_offset.txt"))

# ---- (a) how many cells are "SHR-only" (benchmark 0, shr>0)? -----------------
count_shr_only <- function(da, bench) {
  d <- da |> dplyr::filter(year>=2013, year<=2021,
                           race!="Unknown/Others", sex!="Unknown/Others")
  d$b <- d[[bench]]
  d |> dplyr::mutate(shr_only = (is.na(b) | b==0) & shr>0) |>
    dplyr::filter(shr>0 | (!is.na(b) & b>0))
}
for (nm in c("fips","state")) for (bench in c("fe","mpv")) {
  da <- if (nm=="fips") da_model_fips else da_model_state
  d <- count_shr_only(da, bench)
  tot <- nrow(d); so <- sum(d$shr_only)
  cat(sprintf("\n== %s / %s : SHR-only cells = %d of %d (%.1f%%) ==\n",
              nm, toupper(bench), so, tot, 100*so/tot))
  print(d |> dplyr::filter(shr_only) |> dplyr::count(race, sex) |>
          dplyr::arrange(desc(n)))
}

# ---- (b) sensitivity of coefficients to the correction constant --------------
cat("\n\n=== SENSITIVITY TO CONTINUITY CONSTANT cc (county, Poisson) ===\n")
for (bench in c("fe","mpv")) {
  cat(sprintf("\n-- benchmark %s --\n", toupper(bench)))
  for (cc in c(0.25, 0.5, 1.0)) {
    m <- refit(da_model_fips, bench, "poisson", "fips", cc = cc)
    ct <- summary(m)$coeftable
    g <- function(p){r<-grep(p,rownames(ct)); sprintf("%+.4f(p%.3f)", ct[r,1], ct[r,4])}
    cat(sprintf("  cc=%.2f  Black %s  Hispanic %s  Female %s  [n=%d]\n",
                cc, g("Black"), g("Hispanic"), g("Female"), attr(m,"n_prep")))
  }
}
cat("\n=== SENSITIVITY TO cc (state, NegBin) ===\n")
for (bench in c("fe","mpv")) {
  cat(sprintf("\n-- benchmark %s --\n", toupper(bench)))
  for (cc in c(0.25, 0.5, 1.0)) {
    m <- refit(da_model_state, bench, "negbin", "state", cc = cc)
    ct <- summary(m)$coeftable
    g <- function(p){r<-grep(p,rownames(ct)); sprintf("%+.4f(p%.3f)", ct[r,1], ct[r,4])}
    cat(sprintf("  cc=%.2f  Black %s  Hispanic %s  Female %s  [n=%d]\n",
                cc, g("Black"), g("Hispanic"), g("Female"), attr(m,"n_prep")))
  }
}

# ---- (c) drop benchmark-zero cells entirely ----------------------------------
cat("\n\n=== DROP benchmark-zero cells (only cells with a positive benchmark) ===\n")
for (cfg in list(c("fips","fe","poisson"), c("fips","mpv","poisson"),
                 c("state","fe","negbin"), c("state","mpv","negbin"))) {
  geo <- cfg[1]; bench <- cfg[2]; fam <- cfg[3]
  da <- if (geo=="fips") da_model_fips else da_model_state
  m0 <- refit(da, bench, fam, geo, drop_bench_zero = FALSE)
  m1 <- refit(da, bench, fam, geo, drop_bench_zero = TRUE)
  gb <- function(m){ct<-summary(m)$coeftable; r<-grep("Black",rownames(ct)); sprintf("%+.4f(p%.3f)",ct[r,1],ct[r,4])}
  gf <- function(m){ct<-summary(m)$coeftable; r<-grep("Female",rownames(ct)); sprintf("%+.4f(p%.3f)",ct[r,1],ct[r,4])}
  cat(sprintf("  %-6s %-4s %-8s  keep-all: Black %s Female %s [n=%d] | drop-bench0: Black %s Female %s [n=%d]\n",
              geo, toupper(bench), fam, gb(m0), gf(m0), attr(m0,"n_prep"),
              gb(m1), gf(m1), attr(m1,"n_prep")))
}
sink()
cat("written output/revision/task27_offset.txt\n")
