library(magrittr)
#' Map of Dropdown Items to Plot Functions
#'
#' A data frame mapping dropdown box items to corresponding plot functions.
#'
#' @format A data frame with dropdown items as row names and plot function
#' names as column values.
plot_map <- data.frame(
  map = c(
    "by year" = "plot_aggregated_by_year",
    "by month" = "plot_aggregated_by_month",
    "by hour" = "plot_aggregated_by_hour",
    "by hour and month" = "plot_by_hour_and_month",
    "heatmap" = "plot_heatmap",
    "calendar heatmap" = "plot_calendar_heatmap",
    "line chart" = "plot_line_chart",
    "ridgeline" = "plot_ridgeline",
    "stacked area" = "plot_stacked_area",
    "top 10" = "plot_top_days"
  )
)

output_color <- "#add8e6"
input_color <- "#e97171"

#' Generic Plot Function
#'
#' Selects a plot function based on the specified type and plots the dataset.
#'
#' @param plot_type Character, the plot type from the dropdown menu.
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
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
#' plot("by month", power_data, 2000)
#' @export
plot <- function(plot_type, power_data, year_to_plot = 2024) {
  function_name <- plot_map[plot_type, "map"]
  get(function_name)(power_data, year_to_plot)
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
#' plot_aggregated_by_year(power_data)
#' @export
plot_aggregated_by_year <- function(power_data, year_to_plot = 2024) {
  power_data %>%
    get_yearly_data_long() %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$year, y = .data$value, fill = .data$type)
    ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(
      title = "Yearly Input and Output Sums (kWh)",
      x = "Year",
      y = "Sum of Values"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::scale_fill_manual(
      values = c("total_input" = output_color, "total_output" = input_color)
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(.data$value, digits = 0)),
      vjust = 1.5,
      position = ggplot2::position_dodge(.9),
      colour = "white",
      fontface = 2
    )
}

#' Plot Aggregated Data by Month
#'
#' Generates a bar plot showing monthly input and output sums.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
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
#' plot_aggregated_by_month(power_data)
#' @export
plot_aggregated_by_month <- function(power_data, year_to_plot = 2024) {
  power_data %>%
    get_monthly_data_long() %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$year_month, y = .data$value, fill = .data$type)
    ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(
      title = "Monthly Input and Output Sums",
      x = "Month",
      y = "Sum of Values"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::scale_fill_manual(
      values = c("total_input" = output_color, "total_output" = input_color)
    )
}

#' Plot Aggregated Data by Hour
#'
#' Generates a bar plot showing hourly input and output sums.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
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
#' plot_aggregated_by_hour(power_data)
#' @export
plot_aggregated_by_hour <- function(power_data, year_to_plot = 2024) {
  power_data %>%
    get_hourly_data_long() %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$hour, y = .data$value, fill = .data$type)
    ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(
      title = "Hourly Input and Output Sums",
      x = "Hour",
      y = "Sum of Values"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::scale_fill_manual(
      values = c("total_input" = output_color, "total_output" = input_color)
    )
}

#' Plot Hourly Data by Month
#'
#' Generates a bar plot showing hourly input and output sums by month.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
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
#' plot_by_hour_and_month(power_data)
#' @export
plot_by_hour_and_month <- function(power_data, year_to_plot = 2024) {
  power_data %>%
    get_hourly_monthly_data_long() %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$hour, y = .data$value, fill = .data$type)
    ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::facet_wrap(~month, ncol = 3) +
    ggplot2::labs(
      title = "Hourly Input and Output Sums by Month",
      x = "Hour",
      y = "Sum of Values"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::scale_fill_manual(
      values = c("total_input" = output_color, "total_output" = input_color)
    )
}

#' Generate a Heatmap of Hourly Data by Month
#'
#' Creates a heatmap to show hourly input and output data across months.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
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
#' plot_heatmap(power_data)
#' @export
plot_heatmap <- function(power_data, year_to_plot = 2024) {
  power_data %>%
    get_hourly_monthly_data_long() %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$hour, y = .data$month, fill = .data$value)
    ) +
    ggplot2::geom_tile() +
    ggplot2::facet_wrap(~type, ncol = 1) +
    ggplot2::scale_fill_gradient(low = "white", high = "blue") +
    ggplot2::labs(
      title = "Heatmap of Hourly Input and Output by Month",
      x = "Hour",
      y = "Month",
      fill = "Sum of Values"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Generate a Ridgeline Plot of Hourly Data by Month
#'
#' Creates a ridgeline plot to show distribution of input/output data by month.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
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
#' plot_ridgeline(power_data)
#' @export
plot_ridgeline <- function(power_data, year_to_plot = 2024) {
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
    ggridges::geom_density_ridges(stat = "identity", alpha = 0.5, scale = 0.9) +
    ggplot2::facet_wrap(~type, ncol = 1) +
    ggplot2::labs(
      title = "Ridgeline Plot of Hourly Input and Output by Month",
      x = "Hour",
      y = "Month"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::scale_fill_manual(
      values = c("total_input" = input_color, "total_output" = output_color)
    )
}

#' Generate a Stacked Area Chart of Hourly Data by Month
#'
#' Creates a stacked area chart to visualize input/output data by month.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
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
#' plot_stacked_area(power_data)
#' @export
plot_stacked_area <- function(power_data, year_to_plot = 2024) {
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
      title = "Stacked Area Chart of Hourly Input and Output by Month",
      x = "Hour",
      y = "Sum of Values"
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
#' plot_line_chart(power_data)
#' @export
plot_line_chart <- function(power_data, year_to_plot = 2024) {
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
      title = "Hourly Input and Output by Month",
      x = "Hour",
      y = "Sum of Values",
      color = "Month"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}


#' Generate a Bar Chart of top 10 days of power input vs output
#'
#' Creates a Bar Chart and also displays mean and percentile values.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
#' @return A ggplot object with a line chart.
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
#' plot_top_days(power_data)
#' @export
plot_top_days <- function(power_data, year_to_plot = 2024) {
  # Aggregate data by day
  daily_data <- power_data %>%
    dplyr::mutate(day = as.Date(.data$timestamp)) %>%
    dplyr::group_by(.data$day) %>%
    dplyr::summarise(
      total_input = sum(.data$INPUT, na.rm = TRUE),
      total_output = sum(.data$OUTPUT, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # Identify top 10 days for input and output
  top_input_days <- daily_data %>%
    dplyr::arrange(dplyr::desc(.data$total_input)) %>%
    dplyr::slice_head(n = 10) %>%
    dplyr::mutate(type = "Input", value = .data$total_input)

  top_output_days <- daily_data %>%
    dplyr::arrange(dplyr::desc(.data$total_output)) %>%
    dplyr::slice_head(n = 10) %>%
    dplyr::mutate(type = "Output", value = .data$total_output)

  # Combine both sets of top days for plotting
  top_days <- dplyr::bind_rows(top_input_days, top_output_days)

  # Calculate statistics for input and output separately
  avg_input <- mean(daily_data$total_input, na.rm = TRUE)
  avg_output <- mean(daily_data$total_output, na.rm = TRUE)
  percentile_90_input <- quantile(daily_data$total_input,
    probs = 0.9,
    na.rm = TRUE
  )
  percentile_90_output <- quantile(daily_data$total_output,
    probs = 0.9,
    na.rm = TRUE
  )
  percentile_10_input <- quantile(daily_data$total_input,
    probs = 0.1,
    na.rm = TRUE
  )
  percentile_10_output <- quantile(daily_data$total_output,
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
      title = "Top 10 Days of Power Input and Output",
      x = "Day",
      y = "Power (kWh)",
      fill = "Type"
    ) +
    ggplot2::geom_text(
      data = top_input_days,
      x = 1, y = avg_input, label = "Average",
      vjust = -1, hjust = 0
    ) +
    ggplot2::geom_text(
      data = top_input_days,
      x = 1, y = percentile_90_input,
      label = "90th Percentile",
      vjust = -1, hjust = 0
    ) +
    ggplot2::geom_text(
      data = top_input_days,
      x = 1, y = percentile_10_input,
      label = "10th Percentile",
      vjust = -1, hjust = 0
    ) +
    ggplot2::geom_text(
      data = top_output_days,
      x = 1, y = avg_output, label = "Average",
      vjust = -1, hjust = 0
    ) +
    ggplot2::geom_text(
      data = top_output_days,
      x = 1, y = percentile_90_output,
      label = "90th Percentile",
      vjust = -1, hjust = 0
    ) +
    ggplot2::geom_text(
      data = top_output_days,
      x = 1, y = percentile_10_output,
      label = "10th Percentile",
      vjust = -1, hjust = 0
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::scale_fill_manual(
      values = c(output_color, input_color)
    )
}

#' Generate Calendar Heatmap Chart
#'
#' Creates calendar heatmap chart to show input/output data per day.
#'
#' @param power_data data frame with `timestamp`, `INPUT`, and `OUTPUT` columns.
#' @param year_to_plot year for which to plot the dataframe.
#' @return A ggplot object with a line chart.
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
#' plot_calendar_heatmap(power_data, 2000)
#' @export
plot_calendar_heatmap <- function(power_data, year_to_plot = 2024) {
  calendaric_data <- get_calendaric_data(power_data, year_to_plot)
  cowplot::plot_grid(
    calendR::calendR(
      year = year_to_plot,
      special.days = calendaric_data$total_input,
      gradient = TRUE,
      special.col = "red",
      low.col = "white",
      start = "M",
      title = "Input"
    ),
    calendR::calendR(
      year = year_to_plot,
      special.days = calendaric_data$total_output,
      gradient = TRUE,
      special.col = "blue",
      low.col = "white",
      start = "M",
      title = "Output"
    )
  )
}
