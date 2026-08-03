# =============================================================================
# Emit LaTeX fragments for the revision appendix tables, from validated models.
# =============================================================================
source(here::here("data-raw/revision/R3-model-harness.R"))
devtools::load_all(quiet = TRUE)
outdir <- here::here("output/revision"); dir.create(outdir, showWarnings = FALSE)

dict <- c("race::Black"="Black victim","race::Hispanic"="Hispanic victim",
          "sex::Female"="Female victim", fips="County","state="="State", year="Year")

# ---- Appendix A1: full 2x2x2 grid, main effects (answers Ed-7) ----------------
grid <- expand.grid(bench=c("fe","mpv"), family=c("poisson","negbin"),
                    geo=c("fips","state"), stringsAsFactors=FALSE)
mods <- list()
for (i in seq_len(nrow(grid))) {
  g <- grid[i,]; da <- if (g$geo=="fips") da_model_fips else da_model_state
  mods[[sprintf("%s.%s.%s", g$family, g$geo, g$bench)]] <- refit(da, g$bench, g$family, g$geo)
}
# order: county (poisson fe, poisson mpv, negbin fe, negbin mpv) then state
ord <- c("poisson.fips.fe","poisson.fips.mpv","negbin.fips.fe","negbin.fips.mpv",
         "poisson.state.fe","poisson.state.mpv","negbin.state.fe","negbin.state.mpv")
fixest::etable(mods[ord], tex=TRUE, dict=dict, digits=4, fitstat=~n+bic,
               file=file.path(outdir,"tabA1_grid.tex"), replace=TRUE,
               title="Full family $\\times$ geographic-FE $\\times$ benchmark grid (main effects).",
               label="tab:grid", notes="Same specification throughout; family and geographic fixed effects are set explicitly rather than chosen by BIC. Within each geographic level the family choice barely moves the coefficients; the sign change in the Black coefficient tracks the county-vs-state level, not the family.")

cat("wrote tabA1_grid.tex\n")

# ---- New Table 2: estimation-sample descriptives ------------------------------
describe_panel <- function(da, bench, geo, cc=0.5) {
  m <- refit(da, bench, if (geo=="fips") "poisson" else "negbin", geo)
  d <- prep(da, bench, TRUE, cc)
  gcol <- if (geo=="fips") "fips" else "state"
  data.frame(
    Benchmark = toupper(bench), Level = tools::toTitleCase(geo),
    N = fixest::fitstat(m,"n")$n,
    Cells = nrow(d),
    Units = dplyr::n_distinct(d[[gcol]]),
    SHRmean = round(mean(d$shr),2), SHRmax = max(d$shr),
    SHRzero = paste0(round(100*mean(d$shr==0),0),"\\%"),
    Benchmean = round(mean(d$ofs),2)
  )
}
tab2 <- rbind(
  describe_panel(da_model_fips,"fe","fips"),
  describe_panel(da_model_fips,"mpv","fips"),
  describe_panel(da_model_state,"fe","state"),
  describe_panel(da_model_state,"mpv","state"))
writeLines(capture.output(print(tab2, row.names=FALSE)), file.path(outdir,"table2_panels.txt"))
cat("wrote table2_panels.txt\n"); print(tab2, row.names=FALSE)
