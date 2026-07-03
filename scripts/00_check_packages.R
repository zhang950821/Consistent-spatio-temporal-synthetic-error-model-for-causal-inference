source(file.path("scripts", "_bootstrap.R"))
root <- pstsem_script_root()
source(file.path(root, "R", "load_project.R"))
pstsem_source_project(root)

status <- pstsem_check_packages(stop_on_missing = FALSE)
print(status, row.names = FALSE)

if (any(!status$installed)) {
  quit(status = 1)
}
