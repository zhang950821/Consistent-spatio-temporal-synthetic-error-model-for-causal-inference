testthat::test_that("required packages are installed", {
  status <- pstsem_check_packages(stop_on_missing = FALSE)
  testthat::expect_false(any(!status$installed))
})

testthat::test_that("original chunks were split into scripts", {
  manifest <- pstsem_chunk_manifest()
  testthat::expect_equal(nrow(manifest), 29)
  testthat::expect_true(all(file.exists(pstsem_path(manifest$file))))
  testthat::expect_true("data preparasion" %in% manifest$chunk)
  testthat::expect_true("spatial correlation test" %in% manifest$chunk)
})

testthat::test_that("core data files are present", {
  testthat::expect_true(file.exists(pstsem_data_path("kansas.csv")))
  testthat::expect_true(file.exists(pstsem_data_path("kansas_raw.csv")))
  testthat::expect_true(file.exists(pstsem_data_path("us-state-capitals.csv")))
  testthat::expect_true(dir.exists(pstsem_data_path("total_matrix", "diff_using_60train_data")))
})
