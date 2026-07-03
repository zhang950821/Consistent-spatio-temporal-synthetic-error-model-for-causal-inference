# Validation Log

Validation was run on 2026-07-03 with R 4.2.1 on Windows.

## Dependency Notes

The original Rmd calls `npreg()` but had `library(np)` commented out. To run that
stage, `np` was added to `R/packages.R`.

On this machine, CRAN source installation of `np` failed because the current
source dependency chain required `Matrix >= 1.6.0` while R 4.2.1 provides
`Matrix 1.4-1`. The working setup is:

- `np 0.60-17` from CRAN Windows binary
- `quantreg 5.97` from CRAN Windows binary
- `MatrixModels 0.5-0` from CRAN archive
- `Matrix 1.4-1` from the R 4.2.1 base library

## Commands Run

```sh
Rscript scripts/00_check_packages.R
Rscript scripts/run_smoke_tests.R
Rscript scripts/01_prepare_data.R
Rscript -e "files <- list.files('scripts/chunks', pattern='[.]R$', full.names=TRUE); for (f in files) parse(f); cat('parsed_chunks=', length(files), '/', length(files), '\n')"
Rscript -e "source('R/load_project.R'); pstsem_source_project(); pstsem_run_legacy_stage('setup'); stopifnot(exists('Kansas_training_data'), exists('W'), length(state)==50, all(dim(W)==c(50,50)))"
Rscript scripts/02_run_empirical_core_legacy.R
Rscript scripts/02b_run_kernel_smoke_legacy.R
Rscript scripts/03_run_baselines_legacy.R
Rscript scripts/04_run_plots_legacy.R
Rscript scripts/05_run_simulation_legacy.R
Rscript scripts/06_run_spatial_correlation_legacy.R
```

## Results

- Smoke tests passed.
- All 29 split Rmd chunk scripts parsed successfully.
- Data preparation loaded 50 states and produced a 50 by 50 spatial weight matrix.
- Empirical core completed with:
  - `alasso_MSE_train = 0.1909`
  - `alasso_MSE_valid = 0.8850447`
- Kansas-only kernel smoke completed with the original `npreg()` chunk.
- Baseline script completed: SC, modified SC, ASCM, ST-ASCM, gsynth, and robust SC.
- Plot script completed after running the required model/baseline prerequisites.
- Simulation script completed after fixing two original `sep_series(100)` calls.
- Spatial correlation script completed with:
  - `total_timepoints = 86`
  - `mean_moran_I = 0.01761772`
  - `significant_5pct_count = 33`
  - `significant_1pct_count = 25`

## Original Issues Fixed In Split Legacy Scripts

- Disabled `install.packages("spdep")` inside the split library chunk.
- Added `np` to project dependency loading for `npreg()`.
- Removed UTF-8 BOM from generated chunk scripts so R 4.2.1 can parse them.
- Changed `step(...)` calls to `step(..., trace = 0)` to reduce console noise.
- Fixed `least square synthetic error` by defining `residual_matrix_valid` and
  `control_valid_residual` before `predict.lm()`, and passing a data frame.
- Fixed `W_std>-as.matrix(W_std)` to `W_std <- as.matrix(W_std)`.
- Fixed two simulation calls from `sep_series(100)` to a loop over all 50 units.
- Disabled an unfinished simulation synthetic-control block by default; it can be
  enabled with `options(pstsem.run_unfinished_sim_sc = TRUE)` after its time
  index is corrected.

## Long-Run Note

`scripts/02_run_empirical_pstsem_legacy.R` preserves the original full empirical
sequence including the 50-state `npreg(..., bwmethod = "cv.aic")` kernel loop.
That full kernel run exceeded a 10 minute timeout in this validation session.
Use `scripts/02_run_empirical_core_legacy.R` for the validated main PSTSEM chain
and `scripts/02b_run_kernel_smoke_legacy.R` for a functional kernel check.
