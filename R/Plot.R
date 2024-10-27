library(ggplot2)
library(ggridges)

#' Map of Dropdown Items to Plot Functions
#'
#' A data frame mapping dropdown box items to corresponding plot functions.
#'
#' @format A data frame with dropdown items as row names and plot function
#' names as column values.
#' @usage data(plot_map)
#' @examples
#' plot_map["by month", ]
plot_map <- data.frame(
  map = c(
    "by month" = "plot_aggregated_by_month",
    "by hour" = "plot_aggregated_by_hour",
    "by hour and month" = "plot_by_hour_and_month",
    "heatmap" = "plot_heatmap",
    "line chart" = "plot_line_chart",
    "ridgeline" = "plot_ridgeline",
    "stacked area" = "plot_stacked_area"
  )
)

#' Generic Plot Function
#'
#' Selects a plot function based on the specified plot type and dataset.
#'
#' @param plot_type Character, the plot type from the dropdown menu.
#' @param data Data frame containing data for plotting.
#' @return A ggplot object created by the appropriate plotting function.
#' @examples
#' plot("by month", data)
plot <- function(plot_type, data) {
  function_name <- plot_map[plot_type, "map"]
  get(function_name)(data)
}

##################################################
# implement new plot functions below and add them
# to the list above to have them appear in the ui
##################################################

#' Plot Aggregated Data by Month
#'
#' Generates a bar plot showing monthly input and output sums.
#'
#' @param df Data frame containing monthly data in long format.
#' @return A ggplot object showing monthly aggregated values.
#' @importFrom rlang .data
#' @examples
#' plot_aggregated_by_month(df)
plot_aggregated_by_month <- function(df) {
  monthly_data_long <- get_monthly_data_long(df)

  # Plot the data as bars
  ggplot2::ggplot(
    monthly_data_long,
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
      values = c("total_input" = "blue", "total_output" = "red")
    )
}

#' Plot Aggregated Data by Hour
#'
#' Generates a bar plot showing hourly input and output sums.
#'
#' @param df Data frame containing hourly data in long format.
#' @return A ggplot object showing hourly aggregated values.
#' @importFrom rlang .data
#' @examples
#' plot_aggregated_by_hour(df)
plot_aggregated_by_hour <- function(df) {
  hourly_data_long <- get_hourly_data_long(df)

  # Plot the data as bars
  ggplot2::ggplot(
    hourly_data_long,
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
      values = c("total_input" = "blue", "total_output" = "red")
    )
}

#' Plot Hourly Data by Month
#'
#' Generates a bar plot showing hourly input and output sums by month.
#'
#' @param df Data frame containing hourly data by month in long format.
#' @return A ggplot object showing hourly values by month in facets.
#' @importFrom rlang .data
#' @examples
#' plot_by_hour_and_month(df)
plot_by_hour_and_month <- function(df) {
  hourly_monthly_data_long <- get_hourly_monthly_data_long(df)

  # Plot the data as bars with facets for each month
  ggplot2::ggplot(
    hourly_monthly_data_long,
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
      values = c("total_input" = "blue", "total_output" = "red")
    )
}

#' Generate a Heatmap of Hourly Data by Month
#'
#' Creates a heatmap to show hourly input and output data across months.
#'
#' @param df Data frame containing hourly data by month in long format.
#' @return A ggplot object with a heatmap representing input/output.
#' @importFrom rlang .data
#' @examples
#' plot_heatmap(df)
plot_heatmap <- function(df) {
  hourly_monthly_data_long <- get_hourly_monthly_data_long(df)

  # Heatmap
  ggplot2::ggplot(
    hourly_monthly_data_long,
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
#' @param df Data frame containing hourly data by month in long format.
#' @return A ggplot object with a ridgeline plot.
#' @importFrom rlang .data
#' @examples
#' plot_ridgeline(df)
plot_ridgeline <- function(df) {
  hourly_monthly_data_long <- get_hourly_monthly_data_long(df)

  # Ridgeline plot
  ggplot2::ggplot(
    hourly_monthly_data_long,
    ggplot2::aes(
      x = as.numeric(.data$hour),
      y = .data$month,
      height = .data$value,
      fill = .data$type
    )
  ) +
    ggridges::geom_density_ridges(stat = "identity", alpha = 0.5, scale = 0.9) +
    # Adjust 'scale' for better height separation
    ggplot2::facet_wrap(~type, ncol = 1) +
    ggplot2::labs(
      title = "Ridgeline Plot of Hourly Input and Output by Month",
      x = "Hour",
      y = "Month"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::scale_fill_manual(
      values = c("total_input" = "blue", "total_output" = "red")
    )
}

#' Generate a Stacked Area Chart of Hourly Data by Month
#'
#' Creates a stacked area chart to visualize input/output data by month.
#'
#' @param df Data frame containing hourly data by month in long format.
#' @return A ggplot object with a stacked area chart.
#' @importFrom rlang .data
#' @examples
#' plot_stacked_area(df)
plot_stacked_area <- function(df) {
  hourly_monthly_data_long <- get_hourly_monthly_data_long(df)

  # Stacked area chart
  ggplot2::ggplot(
    hourly_monthly_data_long,
    ggplot2::aes(
      x = .data$hour,
      y = .data$value,
      fill = .data$month,
      group = .data$month
    )
  ) +
    ggplot2::geom_area(position = "stack", alpha = 0.8) +
    ggplot2::facet_wrap(~type, ncol = 1) +
    # Separate panels for INPUT and OUTPUT
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
#' @param df Data frame containing hourly data by month in long format.
#' @return A ggplot object with a line chart.
#' @importFrom rlang .data
#' @examples
#' plot_line_chart(df)
plot_line_chart <- function(df) {
  hourly_monthly_data_long <- get_hourly_monthly_data_long(df)

  # Line plot
  ggplot2::ggplot(
    hourly_monthly_data_long,
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
