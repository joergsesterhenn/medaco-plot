library(dplyr)
test_that("data gets pivoted correctly to long form", {
  test_data_frame <- data.frame(
    timestamp = format(
      seq(as.Date("2000-01-01"), as.Date("2000-01-10"), by = "days"),
      format = "%m-%d-%Y"
    ),
    total_input = 1:10,
    total_output = 10:1
  )
  expected_data_frame <-
    arrange(
      as_tibble(
        rbind(
          data.frame(
            timestamp = format(
              seq(as.Date("2000-01-01"), as.Date("2000-01-10"), by = "days"),
              format = "%m-%d-%Y"
            ),
            type = "total_input",
            value = 1:10
          ), data.frame(
            timestamp = format(
              seq(as.Date("2000-01-01"), as.Date("2000-01-10"), by = "days"),
              format = "%m-%d-%Y"
            ),
            type = "total_output",
            value = 10:1
          )
        )
      ), timestamp
    )
  expect_equal(expected_data_frame, pivot_longer_data(test_data_frame))
})
