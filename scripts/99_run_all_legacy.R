source(file.path("scripts", "_bootstrap.R"))
root <- pstsem_script_root()
source(file.path(root, "R", "load_project.R"))
pstsem_source_project(root, load_packages = TRUE)

pstsem_run_legacy_stage("all", root = root)
