source(file.path("scripts", "_bootstrap.R"))
root <- pstsem_script_root()
source(file.path(root, "R", "load_project.R"))
pstsem_source_project(root)

empirical_data <- pstsem_load_empirical_data()
saveRDS(empirical_data, pstsem_results_path("empirical_data.rds", root = root))

cat("Prepared empirical data for", length(empirical_data$state), "states.\n")
cat("Saved:", pstsem_results_path("empirical_data.rds", root = root), "\n")
