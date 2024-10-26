testthat::test_that("data files are found in path correctly", {
  testthat::expect_equal(
    get_files_in_path(testthat::test_path("testdata")),
    "testdata/2024_01_EC_DATEN.CSV"
  )
})

testthat::test_that("data gets read correctly", {
  expected_data_frame <- input_for_testing
  actual_data <- read_power_data(testthat::test_path("testdata"))
  testthat::expect_equal(
    as.data.frame(actual_data),
    as.data.frame(expected_data_frame),
    ignore_attr = TRUE
  )
})
