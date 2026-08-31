
# Race and Sex in the Underreporting of Police Justifiable Homicides: Replication Package

<!-- badges: start -->
[![R-CMD-check](https://github.com/ceoe-unifesp/jhSelection/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ceoe-unifesp/jhSelection/actions/workflows/R-CMD-check.yaml)[![DOI](https://zenodo.org/badge/1052969485.svg)](https://doi.org/10.5281/zenodo.17081221)

<!-- badges: end -->

## Overview

This R package provides a complete replication package for the paper **"Race and Sex in the Underreporting of Police Justifiable Homicides"** by Julio Trecenti, Ivan Ribeiro, and John Donohue (formerly circulated as "Selection Bias on Justifiable Homicide Reports"). The paper asks whether, among law-enforcement agencies that do report to the FBI's Supplementary Homicide Report (SHR), the incidents they omit are related to the victim's race and sex. It harmonizes three independent databases of police-involved fatalities in the United States (SHR, Fatal Encounters, and Mapping Police Violence) and estimates the demographic structure of underreporting with county and state fixed effects, together with the robustness checks and post-hoc analyses reported in the paper's appendices.

## How to replicate the analysis

First, download the package from GitHub from the [zip file](https://github.com/ceoe-unifesp/jhSelection/archive/refs/heads/main.zip) or clone the repository.

Then, open the project in RStudio or your preferred R environment and run the following commands to load all the necessary libraries:

```r
# Install devtools if not already installed
if (!require(devtools)) install.packages("devtools")

devtools::install()
```

## Data

The package harmonizes three independent sources of U.S. police-involved fatalities into common taxonomies suitable for regression analysis and visualization:

### Primary Datasets

#### State-Level Data (`da_model_state`)

- **Coverage**: 50 US states, 1976-2025 (5,907 observations)
- **Structure**: State × year × race × sex panel
- **Sources**: FBI SHR, Fatal Encounters, Mapping Police Violence
- **Purpose**: Main analysis dataset for state-level regression models

#### County-Level Data (`da_model_fips`)

- **Coverage**: 3,142 US counties, 1976-2025 (22,214 observations)
- **Structure**: County × year × race × sex panel
- **Sources**: Same as state-level, mapped via LEAIC crosswalk
- **Purpose**: Robustness checks at finer geographic resolution

### Raw Count Data

#### SHR-Only Data (`counts_shr`)

- **Coverage**: Counties, 1976-2025 (9,705 observations)
- **Source**: FBI Supplementary Homicide Reports
- **Variables**: Police-involved homicide counts by county, year, race, sex

#### Fatal Encounters Data (`counts_fe`)

- **Coverage**: Counties, 2000-2024 (14,437 observations)
- **Source**: Fatal Encounters crowd-sourced database
- **Variables**: Police-related death counts by county, year, race, sex

#### Mapping Police Violence Data (`counts_mpv`)

- **Coverage**: Counties, 2013-2024 (8,918 observations)
- **Source**: Mapping Police Violence project
- **Variables**: Police violence incident counts by county, year, race, sex

### Supporting Data

#### LEAIC Crosswalk (`leaic_ori`)

- **Coverage**: 22,019 law enforcement agencies
- **Source**: Bureau of Justice Statistics Law Enforcement Agency Identifiers Crosswalk
- **Purpose**: Maps Originating Agency Identifiers (ORI) to county and state FIPS codes

## Reproducing the Analysis

The complete analysis can be reproduced by running the R scripts in the `data-raw/` folder in sequence. Each script serves a specific purpose in the replication pipeline.

It is possible to reproduce the results running only the scripts `data-raw/7-dataviz.R` and `data-raw/8-model.R`. The other scripts are only necessary for the data download and processing. If you only want to run the analysis, you can skip the first 6 scripts.

### Step 1: Data Download (`data-raw/1-download.R`)

**Purpose**: Downloads raw data from external sources and GitHub releases.

**Run with**: `source("data-raw/1-download.R")`

This script:

- Downloads LEAIC crosswalk data from ICPSR (Study 35158)
- Downloads SHR data from the `shrData` package repository
- Downloads Fatal Encounters data from the project's GitHub releases
- Downloads Mapping Police Violence data from the project's releases
- Creates necessary directory structure for data storage

You don't need to run this script again unless you want to update the raw data files.

### Step 2: LEAIC Processing (`data-raw/2-leaic.R`)

**Run with**: `source("data-raw/2-leaic.R")`

**Purpose**: Processes the Law Enforcement Agency Identifiers Crosswalk.

This script:

- Loads and cleans the LEAIC data
- Creates the `leaic_ori` dataset linking ORI codes to FIPS codes
- Saves the processed crosswalk data using `usethis::use_data()`

### Step 3: SHR Data Processing (`data-raw/3-shr.R`)

**Run with**: `source("data-raw/3-shr.R")`

**Purpose**: Processes FBI Supplementary Homicide Reports data.

This script:

- Filters SHR data for justifiable homicides (circumstances 80 and 81)
- Focuses on police justifiable homicides (circumstance 81)
- Harmonizes race and ethnicity categories
- Creates county-level counts by year, race, and sex
- Produces the `counts_shr` dataset

### Step 4: MPV Data Processing (`data-raw/4-mpv.R`)

**Run with**: `source("data-raw/4-mpv.R")`

**Purpose**: Processes Mapping Police Violence data.

This script processes the MPV database and creates the `counts_mpv` dataset.

### Step 5: Fatal Encounters Processing (`data-raw/5-fe.R`)

**Run with**: `source("data-raw/5-fe.R")`

**Purpose**: Processes Fatal Encounters data.

This script processes the Fatal Encounters database and creates the `counts_fe` dataset.

### Step 6: Data Harmonization (`data-raw/6-tidy.R`)

**Run with**: `source("data-raw/6-tidy.R")`

**Purpose**: Harmonizes all data sources into common taxonomies.

This script:

- Combines data from all three sources (SHR, FE, MPV)
- Creates the main analysis datasets (`da_model_state` and `da_model_fips`)
- Ensures consistent race and sex categories across sources
- Handles missing data and temporal coverage differences

### Step 7: Data Visualization (`data-raw/7-dataviz.R`)

**Run with**: `source("data-raw/7-dataviz.R")`

**Purpose**: Creates the figures used in the paper.

This script generates: the victim count by data source over time (Figure 1, with the y-axis anchored at zero); the proportional distributions of victims by race and by sex across sources, and their trends over time; the county-level maps of female and Hispanic reporting disparities; and the per-observation victim-count distribution by source (the histogram that shows the SHR is zero in most observations while the benchmarks are not).

### Step 8: Regression Analysis (`data-raw/8-model.R`)

**Run with**: `source("data-raw/8-model.R")`

**Purpose**: Reproduces every model in the paper, the main tables and all of the
robustness and post-hoc analyses in the appendices. Every model goes through a
single `fit_model()` path (`R/model.R`); the Fatal Encounters robustness variants
are rebuilt with `build_da()` (`R/tidy.R`) from the enriched incident table.

The script is organised in seven sections:

1. **Main tables**: race and sex main effects, and race-by-sex subgroup reporting
   rates, at county and state levels, against both benchmarks (paper Tables 3-4).
2. **Family x aggregation x benchmark grid**: Poisson vs Negative Binomial crossed
   with county/state and FE/MPV, showing the sign change tracks the aggregation
   level, not the distributional family (Table 8).
3. **Continuity-correction sensitivity**: coefficients for a correction of 0.25,
   0.5, and 1.0, and for dropping the corrected observations (Table 9).
4. **Extensive vs intensive margin decomposition**: splits the SHR-versus-benchmark
   gap into agency non-participation and within-agency omission (about four-fifths
   extensive; Table 7).
5. **Fatal Encounters robustness**: the force filter (gunshot, deadly force, all
   deaths) and observed vs imputed race (Table 10).
6. **Role of the fixed effects (post-hoc)**: dropping the geographic fixed effects,
   the share of observations in never-reporting counties absorbed by the fixed
   effects, and female underreporting stratified by within-county reporting
   intensity.
7. **Estimation-sample descriptives and armed context**: the four regression
   samples (Table 2, Panel B) and the share of MPV victims recorded as unarmed by
   race (Table 6).

Tables are printed with `fixest::etable()` and `tibble` summaries.

## Installation

You can install the development version of jhSelection from GitHub with:

``` r
# install.packages("pak")
pak::pak("ceoe-unifesp/jhSelection")
```

## Computational Requirements

- **Memory**: Minimum 4GB RAM recommended
- **Time**:
  - Data download: ~2-3 minutes
  - Data processing: ~2-3 minutes
  - Visualization: ~1-2 minutes
  - Regression analysis: ~1 minute
- **Storage**: ~50MB for raw data files

## Example Usage

```r
library(jhSelection)

# Load the main state-level dataset
data("da_model_state")

# Examine the structure
str(da_model_state)

# Load county-level data  
data("da_model_fips")

# Load raw SHR counts
data("counts_shr")

# Load LEAIC crosswalk
data("leaic_ori")
```

## Citation

If you use this replication package in your research, please cite:

```latex
@software{jhselection_2025,
  author       = {Julio Trecenti and Ivan Ribeiro and John Donohue},
  title        = {ceoe-unifesp/jhSelection: Replication Package for Selection Bias on Justifiable Homicide Reports},
  year         = 2025,
  publisher    = {GitHub},
  version      = {v1.0.0},
  doi          = {https://doi.org/10.5281/zenodo.17081221},
  url          = {https://zenodo.org/records/17081222}
}
```

## License

This replication package is provided under MIT License for academic and research purposes.
