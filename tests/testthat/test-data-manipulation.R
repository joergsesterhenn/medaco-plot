library(dplyr)
library(tidyr)

testthat::test_that("data is pivoted correctly to long form", {
  expected_data_frame <-
    dplyr::arrange(
      tidyr::as_tibble(
        rbind(
          data.frame(
            timestamp = formated_sequence_of_ten_days,
            type = "total_input",
            value = 1.1:10.1
          ), data.frame(
            timestamp = formated_sequence_of_ten_days,
            type = "total_output",
            value = 10.1:1.1
          )
        )
      ), timestamp
    )
  testthat::expect_equal(
    expected_data_frame,
    pivot_longer_data(formated_input_for_testing)
  )
})

testthat::test_that("data is transformed to produce hourly data", {
  expected_data_frame <-
    tidyr::as_tibble(
      rbind(
        data.frame(
          hour = "01",
          type = "total_input",
          value = 56
        ), data.frame(
          hour = "01",
          type = "total_output",
          value = 56
        )
      )
    )
  testthat::expect_equal(
    expected_data_frame,
    get_hourly_data_long(input_for_testing)
  )
})

testthat::test_that("data is transformed to produce monthly data", {
  expected_data_frame <-
    tidyr::as_tibble(
      rbind(
        data.frame(
          year_month = "2000-01",
          type = "total_input",
          value = 56
        ), data.frame(
          year_month = "2000-01",
          type = "total_output",
          value = 56
        )
      )
    )
  testthat::expect_equal(
    expected_data_frame,
    get_monthly_data_long(input_for_testing)
  )
})

testthat::test_that("data is transformed to produce hourly and monthly data", {
  expected_data_frame <-
    tidyr::as_tibble(
      rbind(
        data.frame(
          month = "2000-01",
          hour = "01",
          type = "total_input",
          value = 56
        ), data.frame(
          month = "2000-01",
          hour = "01",
          type = "total_output",
          value = 56
        )
      )
    )
  testthat::expect_equal(
    expected_data_frame,
    get_hourly_monthly_data_long(input_for_testing)
  )
})
