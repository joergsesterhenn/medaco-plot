library(dplyr)

sequence_of_ten_days <- seq(
  as.POSIXct("2000-01-01 01:00:00"),
  as.POSIXct("2000-01-10 01:00:00"),
  by = "days"
)

formated_sequence_of_ten_days <- format(
  sequence_of_ten_days,
  format = "%m-%d-%Y %H:%M"
)

input_for_testing <- data.frame(
  timestamp = sequence_of_ten_days,
  INPUT = 1:10,
  OUTPUT = 10:1
)

formated_input_for_testing <- data.frame(
  timestamp = formated_sequence_of_ten_days,
  total_input = 1:10,
  total_output = 10:1
)

test_that("data gets pivoted correctly to long form", {
  expected_data_frame <-
    arrange(
      as_tibble(
        rbind(
          data.frame(
            timestamp = formated_sequence_of_ten_days,
            type = "total_input",
            value = 1:10
          ), data.frame(
            timestamp = formated_sequence_of_ten_days,
            type = "total_output",
            value = 10:1
          )
        )
      ), timestamp
    )
  expect_equal(
    expected_data_frame,
    pivot_longer_data(formated_input_for_testing)
  )
})

test_that("data gets transformed to produce hourly data", {
  expected_data_frame <-
    as_tibble(
      rbind(
        data.frame(
          hour = "01",
          type = "total_input",
          value = 55
        ), data.frame(
          hour = "01",
          type = "total_output",
          value = 55
        )
      )
    )
  expect_equal(
    expected_data_frame,
    get_hourly_data_long(input_for_testing)
  )
})

test_that("data gets transformed to produce monthly data", {
  expected_data_frame <-
    as_tibble(
      rbind(
        data.frame(
          year_month = "2000-01",
          type = "total_input",
          value = 55
        ), data.frame(
          year_month = "2000-01",
          type = "total_output",
          value = 55
        )
      )
    )
  expect_equal(
    expected_data_frame,
    get_monthly_data_long(input_for_testing)
  )
})

test_that("data gets transformed to produce hourly and monthly data", {
  expected_data_frame <-
    as_tibble(
      rbind(
        data.frame(
          month = "2000-01",
          hour = "01",
          type = "total_input",
          value = 55
        ), data.frame(
          month = "2000-01",
          hour = "01",
          type = "total_output",
          value = 55
        )
      )
    )
  expect_equal(
    expected_data_frame,
    get_hourly_monthly_data_long(input_for_testing)
  )
})