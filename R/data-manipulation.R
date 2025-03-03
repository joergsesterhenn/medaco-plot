library(magrittr)
library(dplyr, warn.conflicts = FALSE)
#' Get Hourly Data in Long Format
#'
#' Aggregates and reshapes the data by hour, returning it in a long format.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @return A data frame with hourly `total_input` and `total_output` values.
#' @importFrom dplyr group_by summarise
#' @importFrom dplyr .data
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
#' @param year_to_plot year for which to filter the dataframe.
#' @return A data frame with daily `total_input` and `total_output` values.
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
#' get_daily_data_long(power_data)
#' @export
get_daily_data_long <- function(power_data, year_to_plot = 2024) {
  power_data %>%
    dplyr::mutate(day = as.Date(.data$timestamp)) %>%
    dplyr::group_by(.data$day) %>%
    dplyr::summarise(
      total_input = sum(.data$INPUT, na.rm = TRUE),
      total_output = sum(.data$OUTPUT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::complete(
      day = seq(
        from = as.Date(paste(year_to_plot, "-01-01", sep = "")),
        to = as.Date(paste(year_to_plot, "-12-31", sep = "")),
        by = "day"
      )
    ) %>%
    pivot_longer_data()
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

#' Get a list of years for which data is available in a dataframe.
#'
#' @param power_data data frame with `timestamp` columns.
#' @return A data frame with only a `year` column and corresponding values.
#' @importFrom rlang .data
#' @examples
#' # Example using a small sample data frame
#' power_data <- data.frame(
#'   timestamp = c(
#'     as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'     as.POSIXct("2001-01-02 01:00:00", tz = "UTC"),
#'     as.POSIXct("2002-02-01 02:00:00", tz = "UTC"),
#'     as.POSIXct("2003-02-02 02:00:00", tz = "UTC")
#'   ),
#'   total_input = c(1.0, 2.0, 3.0, 4.0),
#'   total_output = c(4.0, 3.0, 2.0, 1.0)
#' )
#' get_years_in_data(power_data)
#' @export
get_years_in_data <- function(power_data) {
  power_data %>%
    dplyr::mutate(year = lubridate::year(.data$timestamp)) %>%
    dplyr::select("year") %>%
    dplyr::distinct(.data$year)
}

#' Filter dataframe for data of one specified year only.
#'
#' @param power_data data frame with `timestamp` columns.
#' @param selected_year year for which to return data.
#' @importFrom rlang .data
#' @return A data frame with data only for the selected year.
#' @examples
#' # Example using a small sample data frame
#' power_data <- data.frame(
#'   timestamp = c(
#'     as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'     as.POSIXct("2001-01-02 01:00:00", tz = "UTC"),
#'     as.POSIXct("2002-02-01 02:00:00", tz = "UTC"),
#'     as.POSIXct("2003-02-02 02:00:00", tz = "UTC")
#'   ),
#'   total_input = c(1.0, 2.0, 3.0, 4.0),
#'   total_output = c(4.0, 3.0, 2.0, 1.0)
#' )
#' get_data_for_year(power_data, 2002)
#' @export
get_data_for_year <- function(power_data, selected_year) {
  power_data %>%
    dplyr::filter(lubridate::year(.data$timestamp) == selected_year)
}

#' Get data for plot type to present it next to the plot.
#'
#' @param plot_type name of the selected plot type.
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @return A data frame with values matching the plot.
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
#' get_data_for_plot_type("by hour per month lines", power_data)
#' @export
get_data_for_plot_type <- function(plot_type, power_data) {
  if (startsWith(plot_type, "by year")) {
    get_yearly_data_long(power_data)
  } else if (startsWith(plot_type, "by month")) {
    get_monthly_data_long(power_data)
  } else if (startsWith(plot_type, "by day")) {
    get_daily_data_long(power_data)
  } else if (startsWith(plot_type, "by hour per year")) {
    get_hourly_data_long(power_data)
  } else if (startsWith(plot_type, "by hour per month")) {
    get_hourly_monthly_data_long(power_data)
  } else {
    power_data
  }
}
