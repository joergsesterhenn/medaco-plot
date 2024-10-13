library(ggplot2)
library(ggridges)
library(dplyr)
library(lubridate)
library(tidyr)
library(viridis)
library(purrr)


plot_aggregated_by_month <- function(df) {
  monthly_data_long <- get_monthly_data_in_long_format(df)

  # Plot the data as bars
  ggplot(monthly_data_long, aes(x = year_month, y = value, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Monthly Input and Output Sums",
         x = "Month",
         y = "Sum of Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("total_input" = "blue", "total_output" = "red"))
}

plot_aggregated_by_hour <- function(df) {
  hourly_data_long <- get_hourly_data_in_long_format(df)

  # Plot the data as bars
  ggplot(hourly_data_long, aes(x = hour, y = value, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Hourly Input and Output Sums",
         x = "Hour",
         y = "Sum of Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("total_input" = "blue", "total_output" = "red"))
}



plot_by_hour_and_month <- function(df) {
  hourly_monthly_data_long <- get_hourly_monthly_data_in_long_format(df)

  # Plot the data as bars with facets for each month
  ggplot(hourly_monthly_data_long, aes(x = hour, y = value, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~month, ncol = 3) + # Create separate plots for each month
    labs(title = "Hourly Input and Output Sums by Month",
         x = "Hour",
         y = "Sum of Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("total_input" = "blue", "total_output" = "red"))
}


plot_heatmap <- function(df) {
  hourly_monthly_data_long <- get_hourly_monthly_data_in_long_format(df)

  # Heatmap
  ggplot(hourly_monthly_data_long, aes(x = hour, y = month, fill = value)) +
    geom_tile() +
    facet_wrap(~type, ncol = 1) + # Separate panels for INPUT and OUTPUT
    scale_fill_gradient(low = "white", high = "blue") +
    labs(title = "Heatmap of Hourly Input and Output by Month",
         x = "Hour",
         y = "Month",
         fill = "Sum of Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_ridgeline <- function(df) {
  hourly_monthly_data_long <- get_hourly_monthly_data_in_long_format(df)

  # Ridgeline plot
  ggplot(hourly_monthly_data_long, aes(x = as.numeric(hour), y = month, height = value, fill = type)) +
    geom_density_ridges(stat = "identity", alpha = 0.5, scale = 0.9) + # Adjust 'scale' for better height separation
    facet_wrap(~type, ncol = 1) + # Separate panels for INPUT and OUTPUT
    labs(title = "Ridgeline Plot of Hourly Input and Output by Month",
         x = "Hour",
         y = "Month") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("total_input" = "blue", "total_output" = "red"))
}


plot_stacked_area <- function(df) {
  hourly_monthly_data_long <- get_hourly_monthly_data_in_long_format(df)

  # Stacked area chart
  ggplot(hourly_monthly_data_long, aes(x = hour, y = value, fill = month, group = month)) +
    geom_area(position = "stack", alpha = 0.8) +
    facet_wrap(~type, ncol = 1) + # Separate panels for INPUT and OUTPUT
    labs(title = "Stacked Area Chart of Hourly Input and Output by Month",
         x = "Hour",
         y = "Sum of Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_viridis_d() # Use a color gradient to distinguish months
}


plot_line_chart <- function(df) {
  hourly_monthly_data_long <- get_hourly_monthly_data_in_long_format(df)

  # Line plot
  ggplot(hourly_monthly_data_long, aes(x = hour, y = value, color = month, group = month)) +
    geom_line() +
    facet_wrap(~type, ncol = 1) + # Separate panels for INPUT and OUTPUT
    labs(title = "Hourly Input and Output by Month",
         x = "Hour",
         y = "Sum of Values",
         color = "Month") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

get_hourly_data_in_long_format <- function(df){
  # Create a new column for the year-month to aggregate by month
  df$hour <- format(df$timestamp, "%H")

  # Summing INPUT and OUTPUT values for each month
  hourly_data <- df %>%
    group_by(hour) %>%
    summarise(
      total_input = sum(INPUT, na.rm = TRUE),
      total_output = sum(OUTPUT, na.rm = TRUE),
      .groups = "drop" # Drop the grouping to avoid issues with subsequent operations
    )

  # Reshape the data into long format
  hourly_data_long <- hourly_data %>%
    pivot_longer(cols = c(total_input, total_output),
                 names_to = "type",
                 values_to = "value")

  return(hourly_data_long)
}


get_monthly_data_in_long_format <- function(df){
  # Create a new column for the year-month to aggregate by month
  df$year_month <- format(df$timestamp, "%Y-%m")

  # Summing INPUT and OUTPUT values for each month
  monthly_data <- df %>%
    group_by(year_month) %>%
    summarise(
      total_input = sum(INPUT, na.rm = TRUE),
      total_output = sum(OUTPUT, na.rm = TRUE),
      .groups = "drop" # Drop the grouping to avoid issues with subsequent operations
    )

  # Reshape the data into long format for easy plotting of both INPUT and OUTPUT
  monthly_data_long <- monthly_data %>%
    pivot_longer(cols = c(total_input, total_output),
                 names_to = "type",
                 values_to = "value")

  return(monthly_data_long)
}

get_hourly_monthly_data_in_long_format <- function (df){
  # Data preprocessing
  df$hour <- format(df$timestamp, "%H")
  df$month <- format(df$timestamp, "%Y-%m")

  # Summing INPUT and OUTPUT values for each hour and month
  hourly_monthly_data <- df %>%
    group_by(month, hour) %>%
    summarise(
      total_input = sum(INPUT, na.rm = TRUE),
      total_output = sum(OUTPUT, na.rm = TRUE),
      .groups = "drop" # Drop the grouping to avoid issues with subsequent operations
    )

  # Reshape data into long format
  hourly_monthly_data_long <- hourly_monthly_data %>%
    pivot_longer(cols = c(total_input, total_output),
                 names_to = "type",
                 values_to = "value")
  return(hourly_monthly_data_long)
}


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

plot <- function(plot_type, data) {
  function_name = plot_map[plot_type, "map"]
  get(function_name)(data)
}
