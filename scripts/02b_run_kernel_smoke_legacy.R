source(file.path("scripts", "_bootstrap.R"))
root <- pstsem_script_root()
source(file.path(root, "R", "load_project.R"))
pstsem_source_project(root, load_packages = TRUE)

pstsem_run_legacy_stage("setup", root = root)

# The full kernel chunk loops over all 50 states and can take a long time.
# This smoke run uses the original chunk on Kansas only.
state <- "Kansas"
pstsem_run_legacy_stage("kernel_smoke", root = root)
stopifnot(exists("Kansas_model"))

cat("Kernel smoke finished for Kansas.\n")
