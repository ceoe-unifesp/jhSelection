# =============================================================================
# TASK 10 (Ed-7): Poisson vs NegBin was confounded with county vs state FE
# because the shipped fit_model() picks the family by BIC. Here we cross family
# x geographic FE x benchmark EXPLICITLY (2 x 2 x 2 = 8), holding everything
# else fixed, so the editor can see which factor drives the sign flip.
# =============================================================================
source(here::here("data-raw/revision/R3-model-harness.R"))
devtools::load_all(quiet = TRUE)   # da_model_fips, da_model_state
sink(here::here("output/revision/task10_grid.txt"))

cat("=== VALIDATION: harness reproduces the published baseline ===\n")
b1 <- refit(da_model_fips, "fe",  "poisson", "fips")
b3 <- refit(da_model_state,"fe",  "negbin",  "state")
cat("poisson/fips/fe  -> race=Black:", round(coef(b1)["race::Black"],4),
    " sex=Female:", round(coef(b1)["sex::Female"],4), " (published 0.0260 / -0.2109)\n")
cat("negbin/state/fe  -> race=Black:", round(coef(b3)["race::Black"],4),
    " sex=Female:", round(coef(b3)["sex::Female"],4), " (published 0.2558 / -0.2299)\n\n")

grid <- expand.grid(bench = c("fe","mpv"), geo = c("fips","state"),
                    family = c("poisson","negbin"), stringsAsFactors = FALSE)
mods <- list(); labs <- c()
for (i in seq_len(nrow(grid))) {
  g <- grid[i,]
  da <- if (g$geo == "fips") da_model_fips else da_model_state
  m <- refit(da, g$bench, g$family, g$geo)
  key <- sprintf("%s/%s/%s", g$family, g$geo, toupper(g$bench))
  mods[[key]] <- m; labs <- c(labs, key)
}

cat("=== FULL 2x2x2 GRID (main effects), same specification throughout ===\n")
cat("Columns: family / geo-FE / benchmark\n\n")
print(fixest::etable(mods, digits = 4, fitstat = ~ n + bic,
                     dict = c("race::Black"="Black","race::Hispanic"="Hispanic",
                              "sex::Female"="Female")))

cat("\n\n=== Focus: race=Black coefficient across the grid ===\n")
blk <- sapply(mods, \(m) {
  ct <- summary(m)$coeftable
  r <- grep("Black", rownames(ct))
  sprintf("%.4f (se %.4f, p %.3f)", ct[r,1], ct[r,2], ct[r,4])
})
for (k in names(blk)) cat(sprintf("  %-22s %s\n", k, blk[k]))

cat("\n=== Focus: holding FAMILY fixed, does the sign flip persist across geo? ===\n")
for (fam in c("poisson","negbin")) for (bm in c("fe","mpv")) {
  kf <- sprintf("%s/fips/%s", fam, toupper(bm)); ks <- sprintf("%s/state/%s", fam, toupper(bm))
  cf <- coef(mods[[kf]])["race::Black"]; cs <- coef(mods[[ks]])["race::Black"]
  cat(sprintf("  %-8s %-4s  county=%+.4f  state=%+.4f  %s\n", fam, toupper(bm), cf, cs,
              ifelse(sign(cf)!=sign(cs), "<-- SIGN FLIP across geo", "")))
}
cat("\n=== Holding GEO fixed, does the sign flip persist across family? ===\n")
for (geo in c("fips","state")) for (bm in c("fe","mpv")) {
  kp <- sprintf("poisson/%s/%s", geo, toupper(bm)); kn <- sprintf("negbin/%s/%s", geo, toupper(bm))
  cp <- coef(mods[[kp]])["race::Black"]; cn <- coef(mods[[kn]])["race::Black"]
  cat(sprintf("  %-6s %-4s  poisson=%+.4f  negbin=%+.4f  %s\n", geo, toupper(bm), cp, cn,
              ifelse(sign(cp)!=sign(cn), "<-- sign flip across family", "same sign")))
}
sink()
cat("written output/revision/task10_grid.txt\n")
saveRDS(mods, here::here("output/revision/task10_models.rds"))
