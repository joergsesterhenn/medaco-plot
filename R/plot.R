library(magrittr)
library(ggplot2)
#' Map of Dropdown Items to Plot Functions
#'
#' A data frame mapping dropdown box items to corresponding plot functions.
#'
#' @format A data frame with dropdown items as row names and plot function
#' names as column values.
plot_map <- data.frame(
  map = c(
    "by year (bars)" = "plot_by_year_bars",
    "by month (bars)" = "plot_by_month_bars",
    "by day per year (heatmap)" = "plot_by_day_per_year_calendar_heatmap",
    "by day per year (top 10, bars)" = "plot_by_day_per_year_top_10_bars",
    "by hour per year (bars)" = "plot_by_hour_per_year_bars",
    "by hour per month (bars)" = "plot_by_hour_per_month_bars",
    "by hour per month (heatmap)" = "plot_by_hour_per_month_heatmap",
    "by hour per month (lines)" = "plot_by_hour_per_month_lines",
    "by hour per month (ridgelines)" = "plot_by_hour_per_month_ridgelines",
    "by hour per month (stacked areas)" = "plot_by_hour_per_month_stacked_areas"
  )
)

input_color <- "#add8e6"
output_color <- "#e97171"
dark_background_color <- "#1d1f21"
strip_color <- "purple"

#' Generic Plot Function
#'
#' Selects a plot function based on the specified type and plots the dataset.
#'
#' @param plot_type Character, the plot type from the dropdown menu.
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param display_mode decides whether to plot dark or light.
#' @param year_to_plot year for which to plot the dataframe.
#' @import ggplot2
#' @return A ggplot object created by the appropriate plotting function.
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
#' plot("by month", power_data, "light", 2000)
#' @export
plot <- function(
    plot_type,
    power_data,
    display_mode = "dark",
    year_to_plot = 2024) {
  function_name <- plot_map[plot_type, "map"]
  get(function_name)(power_data, year_to_plot, display_mode) +
    get_theme(display_mode)
}

get_theme <- function(display_mode) {
  if (display_mode == "dark") {
    ggdark::dark_theme_light(base_size = 20) +
      theme(plot.background = element_rect(
        fill = dark_background_color, colour = dark_background_color
      )) +
      theme(panel.background = element_rect(
        fill = dark_background_color, colour = dark_background_color
      )) +
      theme(legend.background = element_rect(
        fill = dark_background_color, colour = dark_background_color
      )) + theme(strip.background = element_rect(fill = strip_color))
  } else {
    ggplot2::theme_light(base_size = 20) +
      theme(strip.background = element_rect(fill = strip_color))
  }
}

##################################################
# implement new plot functions below and add them
# to the list above to have them appear in the ui
##################################################

#' Plot Aggregated Data by Year
#'
#' Generates a bar plot showing yearly input and output sums.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
#' @param display_mode decides whether to plot dark or light.
#' @return A ggplot object showing yearly aggregated values.
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
#' plot_by_year_bars(power_data)
#' @export
plot_by_year_bars <- function(
    power_data,
    year_to_plot = 2024,
    display_mode = "dark") {
  power_data %>%
    get_yearly_data_long() %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$year, y = .data$value, fill = .data$type)
    ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(
      title = i18n$t("by year"),
      x = i18n$t("year"),
      y = i18n$t("sum of values (kWh)")
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::scale_fill_manual(
      values = c("total_input" = input_color, "total_output" = output_color)
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(.data$value, digits = 0)),
      vjust = 1.5,
      position = ggplot2::position_dodge(.9),
      colour = "black"
    )
}

#' Plot Aggregated Data by Month
#'
#' Generates a bar plot showing monthly input and output sums.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
#' @param display_mode decides whether to plot dark or light.
#' @return A ggplot object showing monthly aggregated values.
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
#' plot_by_month_bars(power_data)
#' @export
plot_by_month_bars <- function(
    power_data,
    year_to_plot = 2024,
    display_mode = "dark") {
  power_data %>%
    get_monthly_data_long() %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$year_month, y = .data$value, fill = .data$type)
    ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(
      title = i18n$t("by month"),
      x = i18n$t("month"),
      y = i18n$t("sum of values (kWh)")
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::scale_fill_manual(
      values = c("total_input" = input_color, "total_output" = output_color)
    )
}

#' Plot Aggregated Data by Hour
#'
#' Generates a bar plot showing hourly input and output sums.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
#' @param display_mode decides whether to plot dark or light.
#' @return A ggplot object showing hourly aggregated values.
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
#' plot_by_hour_per_year_bars(power_data)
#' @export
plot_by_hour_per_year_bars <- function(
    power_data,
    year_to_plot = 2024,
    display_mode = "dark") {
  power_data %>%
    get_hourly_data_long() %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$hour, y = .data$value, fill = .data$type)
    ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(
      title = i18n$t("by hour"),
      x = i18n$t("hour"),
      y = i18n$t("sum of values (kWh)")
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::scale_fill_manual(
      values = c("total_input" = input_color, "total_output" = output_color)
    )
}

#' Plot Hourly Data by Month
#'
#' Generates a bar plot showing hourly input and output sums by month.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
#' @param display_mode decides whether to plot dark or light.
#' @return A ggplot object showing hourly values by month in facets.
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
#' plot_by_hour_per_month_bars(power_data)
#' @export
plot_by_hour_per_month_bars <- function(
    power_data,
    year_to_plot = 2024,
    display_mode = "dark") {
  power_data %>%
    get_hourly_monthly_data_long() %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$hour, y = .data$value, fill = .data$type)
    ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::facet_wrap(~month, ncol = 3) +
    ggplot2::labs(
      title = i18n$t("by hour per month"),
      x = i18n$t("hour"),
      y = i18n$t("sum of values (kWh)")
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::scale_fill_manual(
      values = c("total_input" = input_color, "total_output" = output_color)
    )
}

#' Generate a Heatmap of Hourly Data by Month
#'
#' Creates a heatmap to show hourly input and output data across months.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
#' @param display_mode decides whether to plot dark or light.
#' @return A ggplot object with a heatmap representing input/output.
#' @importFrom rlang .data
#' @examples
#' # Example using a small sample data frame
#' power_data <- data.frame(
#'   timestamp = c(
#'     as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-02 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-01 02:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-02 02:00:00", tz = "UTC")
#'   ),
#'   INPUT = c(1.0, 2.0, 3.0, 4.0),
#'   OUTPUT = c(4.0, 3.0, 2.0, 1.0)
#' )
#' plot_by_hour_per_month_heatmap(power_data)
#' @export
plot_by_hour_per_month_heatmap <- function(
    power_data,
    year_to_plot = 2024,
    display_mode = "dark") {
  power_data %>%
    get_hourly_monthly_data_long() %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$hour, y = .data$month, fill = .data$value)
    ) +
    ggplot2::geom_tile() +
    ggplot2::facet_wrap(~type, ncol = 1) +
    ggplot2::scale_fill_gradient(low = "white", high = "blue") +
    ggplot2::labs(
      title = i18n$t("by hour per month (heatmap)"),
      x = i18n$t("hour"),
      y = i18n$t("month"),
      fill = i18n$t("sum of values (kWh)")
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Generate a Ridgeline Plot of Hourly Data by Month
#'
#' Creates a ridgeline plot to show distribution of input/output data by month.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
#' @param display_mode year for which to plot the dataframe.
#' @return A ggplot object with a ridgeline plot.
#' @importFrom rlang .data
#' @examples
#' # Example using a small sample data frame
#' power_data <- data.frame(
#'   timestamp = c(
#'     as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-02 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-02-01 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-02-02 01:00:00", tz = "UTC")
#'   ),
#'   INPUT = c(1.0, 2.0, 3.0, 4.0),
#'   OUTPUT = c(4.0, 3.0, 2.0, 1.0)
#' )
#' plot_by_hour_per_month_ridgelines(power_data)
#' @export
plot_by_hour_per_month_ridgelines <- function(
    power_data,
    year_to_plot = 2024,
    display_mode = "dark") {
  power_data %>%
    get_hourly_monthly_data_long() %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = as.numeric(.data$hour),
        y = .data$month,
        height = .data$value,
        fill = .data$type
      )
    ) +
    ggridges::geom_density_ridges(stat = "identity", alpha = 0.8, scale = 0.9) +
    # ggplot2::facet_wrap(~type, ncol = 1) +
    ggplot2::labs(
      title = i18n$t("by hour per month (ridgeline)"),
      x = i18n$t("hour"),
      y = i18n$t("month")
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::scale_fill_manual(
      values = c("total_input" = input_color, "total_output" = output_color)
    ) +
    ggplot2::scale_x_continuous(breaks = seq(1, 23))
}

#' Generate a Stacked Area Chart of Hourly Data by Month
#'
#' Creates a stacked area chart to visualize input/output data by month.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
#' @param display_mode decides whether to plot dark or light.
#' @return A ggplot object with a stacked area chart.
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
#' plot_by_hour_per_month_stacked_areas(power_data)
#' @export
plot_by_hour_per_month_stacked_areas <- function(
    power_data,
    year_to_plot = 2024,
    display_mode = "dark") {
  power_data %>%
    get_hourly_monthly_data_long() %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$hour,
        y = .data$value,
        fill = .data$month,
        group = .data$month
      )
    ) +
    ggplot2::geom_area(position = "stack", alpha = 0.8) +
    ggplot2::facet_wrap(~type, ncol = 1) +
    ggplot2::labs(
      title = i18n$t("by hour per month (stacked area)"),
      x = i18n$t("hour"),
      y = i18n$t("sum of values (kWh)")
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::scale_fill_viridis_d() # Use a color gradient to distinguish months
}

#' Generate a Line Chart of Hourly Data by Month
#'
#' Creates a line chart to show input/output data by hour for each month.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
#' @param display_mode decides whether to plot dark or light.
#' @return A ggplot object with a line chart.
#' @importFrom rlang .data
#' @examples
#' # Example using a small sample data frame
#' power_data <- data.frame(
#'   timestamp = c(
#'     as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-02 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-01 02:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-02 02:00:00", tz = "UTC")
#'   ),
#'   INPUT = c(1.0, 2.0, 3.0, 4.0),
#'   OUTPUT = c(4.0, 3.0, 2.0, 1.0)
#' )
#' plot_by_hour_per_month_lines(power_data)
#' @export
plot_by_hour_per_month_lines <- function(
    power_data,
    year_to_plot = 2024,
    display_mode = "dark") {
  power_data %>%
    get_hourly_monthly_data_long() %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$hour,
        y = .data$value,
        color = .data$month,
        group = .data$month
      )
    ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~type, ncol = 1) +
    ggplot2::labs(
      title = i18n$t("by hour per month (lines)"),
      x = i18n$t("hour"),
      y = i18n$t("sum of values (kWh)"),
      color = i18n$t("month")
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Generate a Bar Chart of top 10 days of power input vs output
#'
#' Creates a Bar Chart and also displays mean and percentile values.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
#' @param display_mode decides whether to plot dark or light.
#' @return A ggplot object with a chart.
#' @importFrom rlang .data
#' @importFrom stats quantile reorder
#' @examples
#' # Example using a small sample data frame
#' power_data <- data.frame(
#'   timestamp = c(
#'     as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-02 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-01 02:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-02 02:00:00", tz = "UTC")
#'   ),
#'   INPUT = c(1.0, 2.0, 3.0, 4.0),
#'   OUTPUT = c(4.0, 3.0, 2.0, 1.0)
#' )
#' plot_by_day_per_year_top_10_bars(power_data)
#' @export
plot_by_day_per_year_top_10_bars <- function(
    power_data,
    year_to_plot = 2024,
    display_mode = "dark") {
  # Aggregate data by day
  daily_data <- power_data %>%
    get_daily_data_long(year_to_plot)

  daily_input_data <- daily_data %>%
    dplyr::filter(.data$type == "total_input")

  daily_output_data <- daily_data %>%
    dplyr::filter(.data$type == "total_output")

  # Identify top 10 days for input and output
  top_input_days <- daily_input_data %>%
    dplyr::arrange(dplyr::desc(.data$value)) %>%
    dplyr::slice_head(n = 10)

  top_output_days <- daily_output_data %>%
    dplyr::arrange(dplyr::desc(.data$value)) %>%
    dplyr::slice_head(n = 10)

  # Combine both sets of top days for plotting
  top_days <- dplyr::bind_rows(top_input_days, top_output_days)

  # Calculate statistics for input and output separately
  avg_input <- mean(daily_input_data$value, na.rm = TRUE)
  avg_output <- mean(daily_output_data$value, na.rm = TRUE)
  percentile_90_input <- quantile(daily_input_data$value,
    probs = 0.9,
    na.rm = TRUE
  )
  percentile_90_output <- quantile(daily_output_data$value,
    probs = 0.9,
    na.rm = TRUE
  )
  percentile_10_input <- quantile(daily_input_data$value,
    probs = 0.1,
    na.rm = TRUE
  )
  percentile_10_output <- quantile(daily_output_data$value,
    probs = 0.1,
    na.rm = TRUE
  )

  # Plot the data with separate bar graphs for input and output
  ggplot2::ggplot(top_days, ggplot2::aes(
    x = reorder(.data$day, .data$value),
    y = .data$value,
    fill = .data$type
  )) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~ .data$type, scales = "free_x") +
    ggplot2::geom_hline(
      data = top_input_days,
      ggplot2::aes(yintercept = avg_input),
      linetype = "dashed",
      linewidth = 1
    ) +
    ggplot2::geom_hline(
      data = top_input_days,
      ggplot2::aes(yintercept = percentile_90_input),
      linetype = "dotted",
      linewidth = 1
    ) +
    ggplot2::geom_hline(
      data = top_input_days,
      ggplot2::aes(yintercept = percentile_10_input),
      linetype = "dotted",
      linewidth = 1
    ) +
    ggplot2::geom_hline(
      data = top_output_days,
      ggplot2::aes(yintercept = avg_output),
      linetype = "dashed",
      linewidth = 1
    ) +
    ggplot2::geom_hline(
      data = top_output_days,
      ggplot2::aes(yintercept = percentile_90_output),
      linetype = "dotted",
      linewidth = 1
    ) +
    ggplot2::geom_hline(
      data = top_output_days,
      ggplot2::aes(yintercept = percentile_10_output),
      linetype = "dotted",
      linewidth = 1
    ) +
    ggplot2::labs(
      title = i18n$t("by day per year (top 10, bars)"),
      x = i18n$t("day"),
      y = i18n$t("sum of values (kWh)"),
      fill = i18n$t("type")
    ) +
    ggplot2::geom_text(
      data = top_input_days,
      x = 1, y = avg_input, label = "average",
      vjust = -1, hjust = 0
    ) +
    ggplot2::geom_text(
      data = top_input_days,
      x = 1, y = percentile_90_input,
      label = "90th percentile",
      vjust = -1, hjust = 0
    ) +
    ggplot2::geom_text(
      data = top_input_days,
      x = 1, y = percentile_10_input,
      label = "10th percentile",
      vjust = -1, hjust = 0
    ) +
    ggplot2::geom_text(
      data = top_output_days,
      x = 1, y = avg_output, label = "average",
      vjust = -1, hjust = 0
    ) +
    ggplot2::geom_text(
      data = top_output_days,
      x = 1, y = percentile_90_output,
      label = "90th percentile",
      vjust = -1, hjust = 0
    ) +
    ggplot2::geom_text(
      data = top_output_days,
      x = 1, y = percentile_10_output,
      label = "10th percentile",
      vjust = -1, hjust = 0
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::scale_fill_manual(
      values = c(input_color, output_color), guide = "none"
    )
}

#' Generate Calendar Heatmap Chart
#'
#' Creates calendar heatmap chart to show input/output data per day.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
#' @param display_mode decides whether to plot dark or light.
#' @return A ggplot object with a chart.
#' @examples
#' # Example using a small sample data frame
#' power_data <- data.frame(
#'   timestamp = c(
#'     as.POSIXct("2000-01-01 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-02 01:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-01 02:00:00", tz = "UTC"),
#'     as.POSIXct("2000-01-02 02:00:00", tz = "UTC")
#'   ),
#'   INPUT = c(1.0, 2.0, 3.0, 4.0),
#'   OUTPUT = c(4.0, 3.0, 2.0, 1.0)
#' )
#' plot_by_day_per_year_calendar_heatmap(power_data, 2000)
#' @export
plot_by_day_per_year_calendar_heatmap <- function(
    power_data,
    year_to_plot = 2024,
    display_mode = "dark") {
  daily_data <- get_daily_data_long(power_data, year_to_plot)

  daily_input_data <- daily_data %>%
    dplyr::filter(.data$type == "total_input")

  daily_output_data <- daily_data %>%
    dplyr::filter(.data$type == "total_output")

  if (display_mode == "dark") {
    calendar_background <- dark_background_color
    calendar_text_color <- "white"
  } else {
    calendar_background <- "white"
    calendar_text_color <- "black"
  }
  cowplot::plot_grid(
    calendR::calendR(
      year = year_to_plot,
      special.days = daily_input_data$value,
      gradient = TRUE,
      special.col = "blue",
      low.col = "white",
      start = "M",
      title = paste("input ", year_to_plot),
      bg.col = calendar_background,
      mbg.col = calendar_background,
      text.col = calendar_text_color,
      title.col = calendar_text_color,
      subtitle.col = calendar_text_color,
      weeknames.col = calendar_text_color,
      months.col = calendar_text_color
    ),
    calendR::calendR(
      year = year_to_plot,
      special.days = daily_output_data$value,
      gradient = TRUE,
      special.col = "red",
      low.col = "white",
      start = "M",
      title = paste("output ", year_to_plot),
      bg.col = calendar_background,
      mbg.col = calendar_background,
      text.col = calendar_text_color,
      title.col = calendar_text_color,
      subtitle.col = calendar_text_color,
      weeknames.col = calendar_text_color,
      months.col = calendar_text_color
    )
  )
}
