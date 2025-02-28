library(magrittr)
#' Get Hourly Data in Long Format
#'
#' Aggregates and reshapes the data by hour, returning it in a long format.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @return A data frame with hourly `total_input` and `total_output` values.
#' @importFrom dplyr group_by summarise
#' @examples
#' # Example using a small sample data frame
#' power_data <- data.frame(
#'   timestamp = c(
#'     as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-02 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-02-01 02:00:00", tz = "UTC"),
#'     as.POSIXct("2000-02-02 02:00:00", tz = "UTC")
#'   ),
#'   INPUT = c(1.0, 2.0, 3.0, 4.0),
#'   OUTPUT = c(4.0, 3.0, 2.0, 1.0)
#' )
#' get_hourly_data_long(power_data)
#' @export
get_hourly_data_long <- function(power_data) {
  power_data$hour <- format(power_data$timestamp, "%H")

  # Summing INPUT and OUTPUT values for each month
  power_data %>%
    dplyr::group_by(.data$hour) %>%
    dplyr::summarise(
      total_input = sum(.data$INPUT, na.rm = TRUE),
      total_output = sum(.data$OUTPUT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer_data()
}

#' Get Daily Data in Long Format
#'
#' Aggregates and reshapes the data by day, returning it in a long format.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @return A data frame with daily `total_input` and `total_output` values.
#' @importFrom dplyr group_by summarise
#' @examples
#' # Example using a small sample data frame
#' power_data <- data.frame(
#'   timestamp = c(
#'     as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-02 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-02-01 02:00:00", tz = "UTC"),
#'     as.POSIXct("2000-02-02 02:00:00", tz = "UTC")
#'   ),
#'   INPUT = c(1.0, 2.0, 3.0, 4.0),
#'   OUTPUT = c(4.0, 3.0, 2.0, 1.0)
#' )
#' get_hourly_data_long(power_data)
#' @export
get_calendaric_data <- function(power_data) {
  power_data %>%
    dplyr::mutate(
      yday = lubridate::yday(.data$timestamp),
      year = lubridate::year(.data$timestamp),
    ) %>%
    dplyr::filter(.data$year == 2024) %>%
    # Summing INPUT and OUTPUT values for each month
    dplyr::group_by(.data$yday) %>%
    dplyr::summarise(
      total_input = sum(.data$INPUT, na.rm = TRUE),
      total_output = sum(.data$OUTPUT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::complete(
      yday = seq(
        from = 1,
        to = 366,
        by = 1
      )
    ) %>%
    dplyr::mutate(
      dplyr::across(
        tidyr::starts_with("total"), \(x) tidyr::replace_na(x, 0)
      )
    )
}

#' Get Monthly Data in Long Format
#'
#' Aggregates and reshapes the data by month, returning it in a long format.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @return A data frame with monthly `total_input` and `total_output` values.
#' @importFrom dplyr group_by summarise
#' @examples
#' # Example using a small sample data frame
#' power_data <- data.frame(
#'   timestamp = c(
#'     as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-02 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-02-01 02:00:00", tz = "UTC"),
#'     as.POSIXct("2000-02-02 02:00:00", tz = "UTC")
#'   ),
#'   INPUT = c(1.0, 2.0, 3.0, 4.0),
#'   OUTPUT = c(4.0, 3.0, 2.0, 1.0)
#' )
#' get_monthly_data_long(power_data)
#' @export
get_monthly_data_long <- function(power_data) {
  power_data$year_month <- format(power_data$timestamp, "%Y-%m")

  # Summing INPUT and OUTPUT values for each month
  power_data %>%
    dplyr::group_by(.data$year_month) %>%
    dplyr::summarise(
      total_input = sum(.data$INPUT, na.rm = TRUE),
      total_output = sum(.data$OUTPUT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer_data()
}

#' Get Monthly Data in Long Format
#'
#' Aggregates and reshapes the data by year, returning it in a long format.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @return A data frame with yearly `total_input` and `total_output` values.
#' @importFrom dplyr group_by summarise
#' @importFrom rlang .data
#' @examples
#' # Example using a small sample data frame
#' power_data <- data.frame(
#'   timestamp = c(
#'     as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-02 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-02-01 02:00:00", tz = "UTC"),
#'     as.POSIXct("2000-02-02 02:00:00", tz = "UTC")
#'   ),
#'   INPUT = c(1.0, 2.0, 3.0, 4.0),
#'   OUTPUT = c(4.0, 3.0, 2.0, 1.0)
#' )
#' get_monthly_data_long(power_data)
#' @export
get_yearly_data_long <- function(power_data) {
  power_data$year <- format(power_data$timestamp, "%Y")

  # Summing INPUT and OUTPUT values for each month
  power_data %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarise(
      total_input = sum(.data$INPUT, na.rm = TRUE),
      total_output = sum(.data$OUTPUT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer_data()
}


#' Get Hourly and Monthly Data in Long Format
#'
#' Aggregates and reshapes the data by both hour and month, returning it.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @return A data frame with hourly and monthly values.
#' @importFrom dplyr group_by summarise
#' @importFrom rlang .data
#' @examples
#' # Example using a small sample data frame
#' power_data <- data.frame(
#'   timestamp = c(
#'     as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-02 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-02-01 02:00:00", tz = "UTC"),
#'     as.POSIXct("2000-02-02 02:00:00", tz = "UTC")
#'   ),
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
  power_data %>%
    dplyr::group_by(.data$month, .data$hour) %>%
    dplyr::summarise(
      total_input = sum(.data$INPUT, na.rm = TRUE),
      total_output = sum(.data$OUTPUT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer_data()
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
#'     as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-02 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-02-01 02:00:00", tz = "UTC"),
#'     as.POSIXct("2000-02-02 02:00:00", tz = "UTC")
#'   ),
#'   total_input = c(1.0, 2.0, 3.0, 4.0),
#'   total_output = c(4.0, 3.0, 2.0, 1.0)
#' )
#' pivot_longer_data(power_data)
#' @export
pivot_longer_data <- function(power_data) {
  tidyr::pivot_longer(
    data = power_data,
    cols = c("total_input", "total_output"),
    names_to = "type",
    values_to = "value"
  )
}
