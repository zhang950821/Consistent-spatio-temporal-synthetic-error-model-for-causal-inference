# PSTSEM R Project

This project refactors the original monolithic R Markdown workflow into a
clearer R project while keeping the original workflows available.

## Structure

- `R/`: decoupled helper functions for paths, packages, data loading, spatial
  weights, residual matrices, adaptive-lasso synthetic error models,
  simulations, plotting, and Moran diagnostics.
- `scripts/`: runnable project entry points.
- `scripts/chunks/`: the original Rmd code chunks split into separate scripts.
- `reports/`: a lightweight report template.
- `tests/`: smoke tests for dependencies, data, helper functions, simulation,
  diagnostics, and chunk extraction.
- `data/`: copied input data used by the original relative paths.
- `inst/original/`: untouched copy of the source Rmd.
- `results/`: generated outputs.
- `VALIDATION.md`: commands run and validation outcomes.

## Quick Start

Open `PSTSEM.Rproj` in RStudio, then run:

```r
source("R/load_project.R")
pstsem_source_project(load_packages = TRUE)
empirical_data <- pstsem_load_empirical_data()
```

Run smoke tests:

```r
source("scripts/run_smoke_tests.R")
```

Or from a terminal:

```sh
Rscript scripts/run_smoke_tests.R
```

## Main Scripts

- `scripts/00_check_packages.R`: verify all required packages are installed.
- `scripts/01_prepare_data.R`: load empirical data and save `results/empirical_data.rds`.
- `scripts/02_run_empirical_pstsem_legacy.R`: run the original empirical PSTSEM
  chunk sequence, including the slow 50-state kernel loop.
- `scripts/02_run_empirical_core_legacy.R`: run the faster LST plus
  LS/LASSO/adaptive-LASSO empirical prediction chain.
- `scripts/02b_run_kernel_smoke_legacy.R`: run the original kernel chunk on
  Kansas only as a functional smoke test.
- `scripts/03_run_baselines_legacy.R`: run empirical core, then
  synthetic-control baseline chunks.
- `scripts/04_run_plots_legacy.R`: run plotting chunks after model outputs exist.
- `scripts/05_run_simulation_legacy.R`: run simulation chunks.
- `scripts/06_run_spatial_correlation_legacy.R`: run spatial correlation chunks.
- `scripts/99_run_all_legacy.R`: run every original workflow chunk in order.

## Notes

The original Rmd relied heavily on global variables and `assign/get` object
names such as `Kansas_training_data`. The helper API in `R/` returns structured
lists instead. For exact legacy reproduction, the chunk runner still supports
the original object names and working-directory assumptions.

The line `install.packages("spdep")` from the original Rmd was disabled in the
split chunk script. Package installation is intentionally separated from model
execution; use `scripts/00_check_packages.R` to identify missing packages.
