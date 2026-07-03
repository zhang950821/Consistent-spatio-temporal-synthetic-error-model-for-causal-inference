source(file.path("scripts", "_bootstrap.R"))
root <- pstsem_script_root()
source(file.path(root, "R", "load_project.R"))
pstsem_source_project(root)

testthat::test_dir(file.path(root, "tests", "testthat"), reporter = "summary")
