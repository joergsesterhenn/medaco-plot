library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

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

testthat::test_that("data is transformed to produce daily data", {
  expected_data_frame <-
    tidyr::as_tibble(
      rbind(
        data.frame(
          day = as.Date("2000-01-01"),
          type = "total_input",
          value = 1.1
        ), data.frame(
          day = as.Date("2000-01-01"),
          type = "total_output",
          value = 10.1
        )
      )
    )
  testthat::expect_equal(
    expected_data_frame,
    dplyr::filter(
      get_daily_data_long(input_for_testing, 2000), .data$day == "2000-01-01"
    )
  )
})

testthat::test_that("data is transformed to produce yearly data", {
  expected_data_frame <-
    tidyr::as_tibble(
      rbind(
        data.frame(
          year = "2000",
          type = "total_input",
          value = 56
        ), data.frame(
          year = "2000",
          type = "total_output",
          value = 56
        )
      )
    )
  testthat::expect_equal(
    expected_data_frame,
    get_yearly_data_long(input_for_testing)
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

testthat::test_that("data gets filtered correctly", {
  power_data <- data.frame(
    timestamp = c(
      as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
      as.POSIXct("2001-01-02 01:00:00", tz = "UTC"),
      as.POSIXct("2002-02-01 02:00:00", tz = "UTC"),
      as.POSIXct("2003-02-02 02:00:00", tz = "UTC")
    ),
    total_input = c(1.0, 2.0, 3.0, 4.0),
    total_output = c(4.0, 3.0, 2.0, 1.0)
  )
  expected_power_data <- data.frame(
    timestamp = as.POSIXct("2002-02-01 02:00:00", tz = "UTC"),
    total_input = 3.0,
    total_output = 2.0
  )
  testthat::expect_equal(
    get_data_for_year(power_data, "2002"),
    expected_power_data
  )
})

testthat::test_that("years are pulled from data correctly", {
  power_data <- data.frame(
    timestamp = c(
      as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
      as.POSIXct("2001-01-02 01:00:00", tz = "UTC"),
      as.POSIXct("2002-02-01 02:00:00", tz = "UTC"),
      as.POSIXct("2003-02-02 02:00:00", tz = "UTC")
    ),
    total_input = c(1.0, 2.0, 3.0, 4.0),
    total_output = c(4.0, 3.0, 2.0, 1.0)
  )
  expected_power_data <- data.frame(
    year = c(2000, 2001, 2002, 2003)
  )
  testthat::expect_equal(
    get_years_in_data(power_data),
    expected_power_data
  )
})


testthat::test_that("data matching the plot is retrieved", {
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
    get_data_for_plot_function(
      "plot_by_hour_per_year_bars",
      input_for_testing
    )
  )
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
    get_data_for_plot_function(
      "plot_by_hour_per_month_lines",
      input_for_testing
    )
  )
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
    get_data_for_plot_function(
      "plot_by_month_bars",
      input_for_testing
    )
  )
  expected_data_frame <-
    tidyr::as_tibble(
      rbind(
        data.frame(
          year = "2000",
          type = "total_input",
          value = 56
        ), data.frame(
          year = "2000",
          type = "total_output",
          value = 56
        )
      )
    )
  testthat::expect_equal(
    expected_data_frame,
    get_data_for_plot_function(
      "plot_by_year_bars",
      input_for_testing
    )
  )
  expected_data_frame <-
    tidyr::as_tibble(
      rbind(
        data.frame(
          day = as.Date("2000-01-01"),
          type = "total_input",
          value = 1.1
        ), data.frame(
          day = as.Date("2000-01-01"),
          type = "total_output",
          value = 10.1
        )
      )
    )
  testthat::expect_equal(
    expected_data_frame,
    dplyr::filter(
      get_data_for_plot_function(
        "plot_by_day_per_year_top_10_bars",
        input_for_testing
      ), .data$day == "2000-01-01"
    )
  )
  testthat::expect_equal(
    input_for_testing,
    get_data_for_plot_function(
      "plot_something_unknown",
      input_for_testing
    )
  )
})
