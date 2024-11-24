library(magrittr)
#' Get Hourly Data in Long Format
#'
#' Aggregates and reshapes the data by hour, returning it in a long format.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @return A data frame with hourly `total_input` and `total_output` values.
#' @importFrom dplyr group_by summarise
#' @importFrom tidyr pivot_longer
#' @examples
#' # Example using a small sample data frame
#' power_data <- data.frame(
#'   timestamp = c(
#'        as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'        as.POSIXct("2000-01-02 01:00:00", tz = "UTC"),
#'        as.POSIXct("2000-02-01 02:00:00", tz = "UTC"),
#'        as.POSIXct("2000-02-02 02:00:00", tz = "UTC")),
#'   INPUT = c(1.0, 2.0, 3.0, 4.0),
#'   OUTPUT = c(4.0, 3.0, 2.0, 1.0)
#' )
#' get_hourly_data_long(power_data)
#' @export
get_hourly_data_long <- function(power_data) {
  power_data$hour <- format(power_data$timestamp, "%H")

  # Summing INPUT and OUTPUT values for each month
  hourly_data <- power_data %>%
    dplyr::group_by(.data$hour) %>%
    dplyr::summarise(
      total_input = sum(.data$INPUT, na.rm = TRUE),
      total_output = sum(.data$OUTPUT, na.rm = TRUE),
      .groups = "drop"
    )

  return(pivot_longer_data(hourly_data))
}

#' Get Monthly Data in Long Format
#'
#' Aggregates and reshapes the data by month, returning it in a long format.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @return A data frame with monthly `total_input` and `total_output` values.
#' @importFrom dplyr group_by summarise
#' @importFrom tidyr pivot_longer
#' @examples
#' # Example using a small sample data frame
#' power_data <- data.frame(
#'   timestamp = c(
#'        as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'        as.POSIXct("2000-01-02 01:00:00", tz = "UTC"),
#'        as.POSIXct("2000-02-01 02:00:00", tz = "UTC"),
#'        as.POSIXct("2000-02-02 02:00:00", tz = "UTC")),
#'   INPUT = c(1.0, 2.0, 3.0, 4.0),
#'   OUTPUT = c(4.0, 3.0, 2.0, 1.0)
#' )
#' get_monthly_data_long(power_data)
#' @export
get_monthly_data_long <- function(power_data) {
  power_data$year_month <- format(power_data$timestamp, "%Y-%m")

  # Summing INPUT and OUTPUT values for each month
  monthly_data <- power_data %>%
    dplyr::group_by(.data$year_month) %>%
    dplyr::summarise(
      total_input = sum(.data$INPUT, na.rm = TRUE),
      total_output = sum(.data$OUTPUT, na.rm = TRUE),
      .groups = "drop"
    )

  return(pivot_longer_data(monthly_data))
}

#' Get Monthly Data in Long Format
#'
#' Aggregates and reshapes the data by year, returning it in a long format.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @return A data frame with yearly `total_input` and `total_output` values.
#' @importFrom dplyr group_by summarise
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @examples
#' # Example using a small sample data frame
#' power_data <- data.frame(
#'   timestamp = c(
#'        as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'        as.POSIXct("2000-01-02 01:00:00", tz = "UTC"),
#'        as.POSIXct("2000-02-01 02:00:00", tz = "UTC"),
#'        as.POSIXct("2000-02-02 02:00:00", tz = "UTC")),
#'   INPUT = c(1.0, 2.0, 3.0, 4.0),
#'   OUTPUT = c(4.0, 3.0, 2.0, 1.0)
#' )
#' get_monthly_data_long(power_data)
#' @export
get_yearly_data_long <- function(power_data) {
  power_data$year <- format(power_data$timestamp, "%Y")

  # Summing INPUT and OUTPUT values for each month
  yearly_data <- power_data %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarise(
      total_input = sum(.data$INPUT, na.rm = TRUE),
      total_output = sum(.data$OUTPUT, na.rm = TRUE),
      .groups = "drop"
    )

  return(pivot_longer_data(yearly_data))
}


#' Get Hourly and Monthly Data in Long Format
#'
#' Aggregates and reshapes the data by both hour and month, returning it.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @return A data frame with hourly and monthly values.
#' @importFrom dplyr group_by summarise
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @examples
#' # Example using a small sample data frame
#' power_data <- data.frame(
#'   timestamp = c(
#'        as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'        as.POSIXct("2000-01-02 01:00:00", tz = "UTC"),
#'        as.POSIXct("2000-02-01 02:00:00", tz = "UTC"),
#'        as.POSIXct("2000-02-02 02:00:00", tz = "UTC")),
#'   INPUT = c(1.0, 2.0, 3.0, 4.0),
#'   OUTPUT = c(4.0, 3.0, 2.0, 1.0)
#' )
#' get_hourly_monthly_data_long(power_data)
#' @export
get_hourly_monthly_data_long <- function(power_data) {
  # Data preprocessing
  power_data$hour <- format(power_data$timestamp, "%H")
  power_data$month <- format(power_data$timestamp, "%Y-%m")

  # Summing INPUT and OUTPUT values for each hour and month
  hourly_monthly_data <- power_data %>%
    dplyr::group_by(.data$month, .data$hour) %>%
    dplyr::summarise(
      total_input = sum(.data$INPUT, na.rm = TRUE),
      total_output = sum(.data$OUTPUT, na.rm = TRUE),
      .groups = "drop"
    )

  return(pivot_longer_data(hourly_monthly_data))
}

#' Pivot Data to Long Format for Input and Output
#'
#' Transforms data from a wide format to a long format, creating separate rows
#' for `total_input` and `total_output`.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @return A data frame in long format with a `type` column indicating input or
#' output, and a `value` column for the corresponding values.
#' @importFrom tidyr pivot_longer
#' @examples
#' # Example using a small sample data frame
#' power_data <- data.frame(
#'   timestamp = c(
#'        as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'        as.POSIXct("2000-01-02 01:00:00", tz = "UTC"),
#'        as.POSIXct("2000-02-01 02:00:00", tz = "UTC"),
#'        as.POSIXct("2000-02-02 02:00:00", tz = "UTC")),
#'   total_input = c(1.0, 2.0, 3.0, 4.0),
#'   total_output = c(4.0, 3.0, 2.0, 1.0)
#' )
#' pivot_longer_data(power_data)
#' @export
pivot_longer_data <- function(power_data) {
  return(
    tidyr::pivot_longer(
      data = power_data,
      cols = c("total_input", "total_output"),
      names_to = "type",
      values_to = "value"
    )
  )
}
