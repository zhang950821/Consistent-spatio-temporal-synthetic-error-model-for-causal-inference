source(file.path("scripts", "_bootstrap.R"))
root <- pstsem_script_root()
source(file.path(root, "R", "load_project.R"))
pstsem_source_project(root, load_packages = TRUE)

pstsem_run_legacy_stage("setup", root = root)
pstsem_run_legacy_stage("empirical_core", root = root)

cat("Empirical core finished.\n")
cat("alasso_MSE_train:", alasso_MSE_train, "\n")
cat("alasso_MSE_valid:", alasso_MSE_valid, "\n")
