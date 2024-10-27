library(dplyr)
library(tidyr)


utils::globalVariables(c("%>%"))

#' Get Hourly Data in Long Format
#'
#' Aggregates and reshapes the data by hour, returning it in a long format.
#'
#' @param df A data frame containing `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @return A data frame with hourly `total_input` and `total_output` values.
#' @importFrom dplyr group_by summarise
#' @importFrom tidyr pivot_longer
#' @examples
#' get_hourly_data_long(df)
get_hourly_data_long <- function(df) {
  df$hour <- format(df$timestamp, "%H")

  # Summing INPUT and OUTPUT values for each month
  hourly_data <- df %>%
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
#' @param df A data frame containing `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @return A data frame with monthly `total_input` and `total_output` values.
#' @importFrom dplyr group_by summarise
#' @importFrom tidyr pivot_longer
#' @examples
#' get_monthly_data_long(df)
get_monthly_data_long <- function(df) {
  df$year_month <- format(df$timestamp, "%Y-%m")

  # Summing INPUT and OUTPUT values for each month
  monthly_data <- df %>%
    dplyr::group_by(.data$year_month) %>%
    dplyr::summarise(
      total_input = sum(.data$INPUT, na.rm = TRUE),
      total_output = sum(.data$OUTPUT, na.rm = TRUE),
      .groups = "drop"
    )

  return(pivot_longer_data(monthly_data))
}

#' Get Hourly and Monthly Data in Long Format
#'
#' Aggregates and reshapes the data by both hour and month, returning it.
#'
#' @param df A data frame containing `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @return A data frame with hourly and monthly values.
#' @importFrom dplyr group_by summarise
#' @importFrom tidyr pivot_longer
#' @examples
#' get_hourly_monthly_data_long(df)
get_hourly_monthly_data_long <- function(df) {
  # Data preprocessing
  df$hour <- format(df$timestamp, "%H")
  df$month <- format(df$timestamp, "%Y-%m")

  # Summing INPUT and OUTPUT values for each hour and month
  hourly_monthly_data <- df %>%
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
#' @param df A data frame containing `total_input` and `total_output` columns.
#' @return A data frame in long format with a `type` column indicating input or
#' output, and a `value` column for the corresponding values.
#' @importFrom tidyr pivot_longer
#' @examples
#' pivot_longer_data(df)
pivot_longer_data <- function(df) {
  return(
    tidyr::pivot_longer(
      data = df,
      cols = c("total_input", "total_output"),
      names_to = "type",
      values_to = "value"
    )
  )
}
