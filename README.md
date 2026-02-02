# UK Biobank: Study 3: Cross sectional association of self-report PA and self-report fracture

This repository contains scripts and R Markdown documents for analyses examining
fracture incidence and physical activity in UK Biobank.

## How to reproduce

1. Place UKB raw data files in `/data_raw/` (not under version control).
2. Run the pipeline:
source("scripts/05_run_all.R")
3. Outputs (tables and figures) will appear in `/results/`.

## Dependencies
- R version: tbd
- Key packages: tidyverse, survival, tableone, ggplot2, knitr, rmarkdown

## Data security
- No UK Biobank data had been committed to Git.
- Data storage complies with institutional and UKB data access rules.

