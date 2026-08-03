# =============================================================================
# TASKS 7 & 8 (Ed-4 & Ed-5): reconcile the 2:1 gap in Fig.1 with the moderate
# coefficients, and answer "are the FE the right tool if non-reporting agencies
# drive the difference?".
#
# Decompose the SHR-vs-benchmark gap (2013-2021, common window) into:
#   (i)  EXTENSIVE margin: victims in county-years where SHR reports NOTHING
#   (ii) INTENSIVE margin: shortfall within county-years where SHR reports >0
# Then show how many observations the county FE drop (all-zero-outcome groups),
# refit with year-FE only, and stratify by county reporting intensity.
# =============================================================================
source(here::here("data-raw/revision/R3-model-harness.R"))
devtools::load_all(quiet = TRUE)
sink(here::here("output/revision/task7_8_margins.txt"))

for (bench in c("fe","mpv")) {
  d <- da_model_fips |>
    dplyr::filter(year >= 2013, year <= 2021,
                  race != "Unknown/Others", sex != "Unknown/Others")
  d$b <- d[[bench]]; d$b[is.na(d$b)] <- 0; d$shr[is.na(d$shr)] <- 0
  # collapse to county-year (sum over race/sex) to define reporting status
  cy <- d |> dplyr::group_by(state, fips, year) |>
    dplyr::summarise(shr = sum(shr), b = sum(b), .groups="drop") |>
    dplyr::filter(b > 0)                       # counties where the benchmark saw a death
  cy <- cy |> dplyr::mutate(reports = shr > 0)
  tot_b   <- sum(cy$b)
  ext_b   <- sum(cy$b[!cy$reports])            # benchmark victims in non-reporting county-years
  int_b   <- sum(cy$b[cy$reports])             # benchmark victims in reporting county-years
  int_shr <- sum(cy$shr[cy$reports])
  gap_tot <- tot_b - sum(cy$shr)
  gap_ext <- ext_b                             # SHR=0 there, so full benchmark count is the gap
  gap_int <- int_b - int_shr
  cat(sprintf("\n================ BENCHMARK = %s (2013-2021) ================\n", toupper(bench)))
  cat(sprintf("County-years with a benchmark death:            %d\n", nrow(cy)))
  cat(sprintf("  of which SHR reports nothing (non-reporting): %d (%.1f%%)\n",
              sum(!cy$reports), 100*mean(!cy$reports)))
  cat(sprintf("Benchmark victims total:                        %d\n", tot_b))
  cat(sprintf("SHR victims total:                              %d\n", sum(cy$shr)))
  cat(sprintf("Overall gap (benchmark - SHR):                  %d (SHR captures %.1f%%)\n",
              gap_tot, 100*sum(cy$shr)/tot_b))
  cat(sprintf("  EXTENSIVE margin (non-reporting counties):    %d  (%.1f%% of the gap)\n",
              gap_ext, 100*gap_ext/gap_tot))
  cat(sprintf("  INTENSIVE margin (reporting-but-incomplete):  %d  (%.1f%% of the gap)\n",
              gap_int, 100*gap_int/gap_tot))
  cat(sprintf("  Within reporting counties SHR captures:       %.1f%% of benchmark victims\n",
              100*int_shr/int_b))
}

# ---- Task 8: observations dropped by all-zero-outcome county FE --------------
cat("\n\n================ FE-DROP ACCOUNTING (county models) ================\n")
for (bench in c("fe","mpv")) {
  d <- prep(da_model_fips, bench, remove_unknown = TRUE, cc = 0.5)
  # a county contributes to identification only if it has >=1 positive SHR outcome
  pos_by_fips <- d |> dplyr::group_by(fips) |>
    dplyr::summarise(any_pos = any(shr > 0), n = dplyr::n(), .groups="drop")
  drop_fips <- sum(!pos_by_fips$any_pos)
  drop_obs  <- sum(d$fips %in% pos_by_fips$fips[!pos_by_fips$any_pos])
  cat(sprintf("%s/county: prepared cells=%d; counties=%d; all-zero-SHR counties dropped=%d; obs dropped=%d (%.1f%%); obs used=%d\n",
              toupper(bench), nrow(d), nrow(pos_by_fips), drop_fips, drop_obs,
              100*drop_obs/nrow(d), nrow(d)-drop_obs))
}

# ---- Task 8: refit with YEAR FE only (no geographic FE) ----------------------
cat("\n================ NO GEOGRAPHIC FE (year FE only), Poisson ================\n")
for (bench in c("fe","mpv")) {
  m_geo <- refit(da_model_fips, bench, "poisson", "fips")
  m_non <- refit(da_model_fips, bench, "poisson", "none")
  g <- function(m,p){ct<-summary(m)$coeftable; r<-grep(p,rownames(ct)); sprintf("%+.4f(p%.3f)",ct[r,1],ct[r,4])}
  cat(sprintf("  %s  county-FE:  Black %s Hispanic %s Female %s\n", toupper(bench),
              g(m_geo,"Black"), g(m_geo,"Hispanic"), g(m_geo,"Female")))
  cat(sprintf("  %s  year-only:  Black %s Hispanic %s Female %s\n", toupper(bench),
              g(m_non,"Black"), g(m_non,"Hispanic"), g(m_non,"Female")))
}

# ---- Task 8: stratify by county reporting intensity (SHR/benchmark ratio) ----
cat("\n================ STRATIFY BY COUNTY REPORTING INTENSITY (FE, Poisson/county) ================\n")
d <- da_model_fips |> dplyr::filter(year>=2013, year<=2021,
        race!="Unknown/Others", sex!="Unknown/Others")
d$fe[is.na(d$fe)] <- 0; d$shr[is.na(d$shr)] <- 0
cint <- d |> dplyr::group_by(fips) |>
  dplyr::summarise(shr=sum(shr), fe=sum(fe), .groups="drop") |>
  dplyr::filter(fe>0) |> dplyr::mutate(ratio = shr/fe,
    stratum = dplyr::case_when(ratio==0 ~ "0 non-reporting",
                               ratio<=0.5 ~ "1 low (<=50%)",
                               ratio<=1  ~ "2 mid (50-100%)",
                               TRUE      ~ "3 high (>100%)"))
cat("Counties by reporting stratum:\n"); print(cint |> dplyr::count(stratum))
da_s <- da_model_fips |> dplyr::left_join(dplyr::select(cint,fips,stratum), "fips")
for (st in c("1 low (<=50%)","2 mid (50-100%)","3 high (>100%)")) {
  sub <- da_s |> dplyr::filter(stratum==st)
  m <- tryCatch(refit(sub, "fe","poisson","fips"), error=function(e) NULL)
  if (!is.null(m)) {
    g <- function(p){ct<-summary(m)$coeftable; r<-grep(p,rownames(ct)); if(length(r)) sprintf("%+.4f(p%.3f)",ct[r,1],ct[r,4]) else "NA"}
    cat(sprintf("  stratum %-16s Black %s Hispanic %s Female %s [n=%d]\n",
                st, g("Black"), g("Hispanic"), g("Female"), attr(m,"n_prep")))
  }
}
sink()
cat("written output/revision/task7_8_margins.txt\n")
