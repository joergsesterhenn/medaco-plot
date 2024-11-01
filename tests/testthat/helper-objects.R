# fixtures for tests, see https://testthat.r-lib.org/articles/test-fixtures.html
library(tibble)

sequence_of_ten_days <- seq(
  as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
  as.POSIXct("2000-01-10 01:00:00", tz = "UTC"),
  by = "days"
)

formated_sequence_of_ten_days <- format(
  sequence_of_ten_days,
  format = "%m-%d-%Y %H:%M"
)

input_for_testing <- tibble::tibble(
  timestamp = sequence_of_ten_days,
  INPUT = 1.1:10.1,
  OUTPUT = 10.1:1.1
)

formated_input_for_testing <- tibble::tibble(
  timestamp = formated_sequence_of_ten_days,
  total_input = 1.1:10.1,
  total_output = 10.1:1.1
)
