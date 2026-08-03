# Revision analyses for JLEA-26-0003

Scripts added to address the editor's and reviewers' comments on the manuscript
*Who Is Missing? The Demographic Structure of Underreporting in Justifiable
Homicide Reports* (formerly *Selection Bias on Justifiable Homicides Reports*).

Every script writes its output to `output/revision/` and is reproducible from the
processed package data (`data/*.rda`); the two enrichment scripts additionally
require the raw inputs downloaded by `data-raw/1-download.R`.

| Script | Addresses | Produces |
|---|---|---|
| `R1-fe-enriched.R` | Int-1 (FE filter), R1-6 (imputation), R2-4 (armed) | `fe_enriched.rds`; validates against `counts_fe.rda` |
| `R2-mpv-shr-enriched.R` | R2-4, appendix, R1-5 | `mpv_enriched.rds`; MPV ORI-join accounting; armed x race |
| `R3-model-harness.R` | (sourced) | `build_da()`, `prep()`, `refit()` with explicit family / geo / cc |
| `R4-task10-grid.R` | Ed-7 | `task10_grid.txt` (2x2x2 family x geo x benchmark grid) |
| `R5-task27-offset.R` | Int-2 | `task27_offset.txt` (continuity-correction sensitivity) |
| `R6-task7-8-margins.R` | Ed-4, Ed-5 | `task7_8_margins.txt` (extensive/intensive decomposition) |
| `R7-task26-17-fe-variants.R` | Int-1, R1-6 | `task26_17_fe_variants.txt` (FE filter + imputation robustness) |
| `R8-shr-descriptives.R` | R1-5, Int-1 | `task16_descriptives.txt` (firearm share; estimation samples) |
| `R9-latex-tables.R` | tables | LaTeX fragments and Panel B numbers |

Key results (verified, reproducing the published Table 3 exactly):

- **Margin decomposition:** ~79% of the SHR-vs-benchmark gap is the extensive
  margin (non-reporting counties); ~21% is the intensive margin. Within-reporting
  capture 68.8% (FE) / 74.2% (MPV).
- **Family vs aggregation:** holding aggregation fixed, Poisson and Negative
  Binomial coefficients coincide at the county level; the Black sign change tracks
  county-vs-state, not the family.
- **Continuity correction:** race coefficients insensitive to the constant and to
  dropping zero-benchmark cells; female result robust and if anything stronger.
- **FE filter / imputation:** female robust across Gunshot / Deadly-force / all-FE
  filters and across imputed vs observed race; Hispanic stronger under Deadly-force
  and observed-race-only.

Run all (from the package root, after `1-download.R`):

```r
for (f in sprintf("data-raw/revision/R%d-%s.R",
                  c(1,2,4,5,6,7,8,9),
                  c("fe-enriched","mpv-shr-enriched","task10-grid","task27-offset",
                    "task7-8-margins","task26-17-fe-variants","shr-descriptives",
                    "latex-tables")))
  source(here::here(f))
```
