library(ggplot2)
library(ggridges)

# map of dropdownbox items mapped to plot functions
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

# generic plot function based on plot_map
plot <- function(plot_type, data) {
  function_name <- plot_map[plot_type, "map"]
  get(function_name)(data)
}

##################################################
# implement new plot functions below and add them
# to the list above to have them appear in the ui
##################################################

#' @importFrom rlang .data
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

#' @importFrom rlang .data
plot_aggregated_by_hour <- function(df) {
  hourly_data_long <- get_hourly_data_in_long_format(df)

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

#' @importFrom rlang .data
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

#' @importFrom rlang .data
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

#' @importFrom rlang .data
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

#' @importFrom rlang .data
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

#' @importFrom rlang .data
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
