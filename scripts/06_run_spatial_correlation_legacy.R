source(file.path("scripts", "_bootstrap.R"))
root <- pstsem_script_root()
source(file.path(root, "R", "load_project.R"))
pstsem_source_project(root, load_packages = TRUE)

pstsem_run_legacy_stage("setup", root = root)

diff_kansas_total_mod <- read.csv("data/diff_kansas_total.csv")[, -1]
diff_kansas_total_mod <- diff_kansas_total_mod[diff_kansas_total_mod$timepoint != 3, ]

pstsem_run_legacy_stage("spatial_correlation", root = root)

cat("Spatial correlation finished.\n")
print(summary_table)
