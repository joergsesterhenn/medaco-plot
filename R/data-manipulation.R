library(dplyr)
library(tidyr)


utils::globalVariables(c("%>%"))

# gets hourly data in long format
get_hourly_data_in_long_format <- function(df) {
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

# gets monthly data in long format
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

# gets hourly and monthly data in long format
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

# pivots data so that there is a separate row for input and output
pivot_longer_data <- function(df) {
  return(df %>%
    tidyr::pivot_longer(
      cols = c(.data$total_input, .data$total_output),
      names_to = "type",
      values_to = "value"
    ))
}
