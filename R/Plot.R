library(ggplot2)
library(ggridges)
library(dplyr)
library(lubridate)
library(tidyr)
library(viridis)


plot_aggregated_by_month <- function(df) {
  # Create a new column for the year-month to aggregate by month
  df$year_month <- format(df$timestamp, "%Y-%m")

  # Summing INPUT and OUTPUT values for each month
  monthly_data <- df %>%
    group_by(year_month) %>%
    summarise(
      total_input = sum(INPUT, na.rm = TRUE),
      total_output = sum(OUTPUT, na.rm = TRUE),
      .groups = "drop"  # Drop the grouping to avoid issues with subsequent operations
    )

  # Reshape the data into long format for easy plotting of both INPUT and OUTPUT
  monthly_data_long <- monthly_data %>%
    pivot_longer(cols = c(total_input, total_output),
                 names_to = "type",
                 values_to = "value")

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
  # Create a new column for the year-month to aggregate by month
  df$hour <- format(df$timestamp, "%H")

  # Summing INPUT and OUTPUT values for each month
  hourly_data <- df %>%
    group_by(hour) %>%
    summarise(
      total_input = sum(INPUT, na.rm = TRUE),
      total_output = sum(OUTPUT, na.rm = TRUE),
      .groups = "drop"  # Drop the grouping to avoid issues with subsequent operations
    )

  # Reshape the data into long format for easy plotting of both INPUT and OUTPUT
  hourly_data_long <- hourly_data %>%
    pivot_longer(cols = c(total_input, total_output),
                 names_to = "type",
                 values_to = "value")

  # Plot the data as bars
  ggplot(hourly_data_long, aes(x = hour, y = value, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Hourly Input and Output Sums",
         x = "Hour",
         y = "Sum of Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("total_input" = "blue", "total_output" = "red"))
}



plot_by_hour_and_month <- function (df) {
  # Extract both the hour and the month
  df$hour <- format(df$timestamp, "%H")
  df$month <- format(df$timestamp, "%Y-%m")

  # Summing INPUT and OUTPUT values for each hour and month
  hourly_monthly_data <- df %>%
    group_by(month, hour) %>%
    summarise(
      total_input = sum(INPUT, na.rm = TRUE),
      total_output = sum(OUTPUT, na.rm = TRUE),
      .groups = "drop"  # Drop the grouping to avoid issues with subsequent operations
    )

  # Reshape the data into long format for easy plotting of both INPUT and OUTPUT
  hourly_monthly_data_long <- hourly_monthly_data %>%
    pivot_longer(cols = c(total_input, total_output),
                 names_to = "type",
                 values_to = "value")

  # Plot the data as bars with facets for each month
  ggplot(hourly_monthly_data_long, aes(x = hour, y = value, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ month, ncol = 3) +  # Create separate plots for each month
    labs(title = "Hourly Input and Output Sums by Month",
         x = "Hour",
         y = "Sum of Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("total_input" = "blue", "total_output" = "red"))
}


plot_heatmap <- function(df) {
  # Data preprocessing
  df$hour <- format(df$timestamp, "%H")
  df$month <- format(df$timestamp, "%Y-%m")

  hourly_monthly_data <- df %>%
    group_by(month, hour) %>%
    summarise(
      total_input = sum(INPUT, na.rm = TRUE),
      total_output = sum(OUTPUT, na.rm = TRUE),
      .groups = "drop"  # Drop the grouping to avoid issues with subsequent operations
    )

  hourly_monthly_data_long <- hourly_monthly_data %>%
    pivot_longer(cols = c(total_input, total_output),
                 names_to = "type",
                 values_to = "value")

  # Heatmap
  ggplot(hourly_monthly_data_long, aes(x = hour, y = month, fill = value)) +
    geom_tile() +
    facet_wrap(~ type, ncol = 1) +  # Separate panels for INPUT and OUTPUT
    scale_fill_gradient(low = "white", high = "blue") +
    labs(title = "Heatmap of Hourly Input and Output by Month",
         x = "Hour",
         y = "Month",
         fill = "Sum of Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_ridgeline <- function(df) {
  # Data preprocessing
  df$hour <- format(df$timestamp, "%H")
  df$month <- format(df$timestamp, "%Y-%m")

  # Summing INPUT and OUTPUT values for each hour and month
  hourly_monthly_data <- df %>%
    group_by(month, hour) %>%
    summarise(
      total_input = sum(INPUT, na.rm = TRUE),
      total_output = sum(OUTPUT, na.rm = TRUE),
      .groups = "drop"  # Drop the grouping to avoid issues with subsequent operations
    )

  # Reshape data into long format for ridgeline plot
  hourly_monthly_data_long <- hourly_monthly_data %>%
    pivot_longer(cols = c(total_input, total_output),
                 names_to = "type",
                 values_to = "value")

  # Ridgeline plot
  ggplot(hourly_monthly_data_long, aes(x = as.numeric(hour), y = month, height = value, fill = type)) +
    geom_density_ridges(stat = "identity", alpha = 0.5, scale = 0.9) +  # Adjust 'scale' for better height separation
    facet_wrap(~ type, ncol = 1) +  # Separate panels for INPUT and OUTPUT
    labs(title = "Ridgeline Plot of Hourly Input and Output by Month",
         x = "Hour",
         y = "Month") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("total_input" = "blue", "total_output" = "red"))
}


plot_stacked_area <- function(df) {
  # Data preprocessing
  df$hour <- format(df$timestamp, "%H")
  df$month <- format(df$timestamp, "%Y-%m")

  hourly_monthly_data <- df %>%
    group_by(month, hour) %>%
    summarise(
      total_input = sum(INPUT, na.rm = TRUE),
      total_output = sum(OUTPUT, na.rm = TRUE),
      .groups = "drop"  # Drop the grouping to avoid issues with subsequent operations
    )

  hourly_monthly_data_long <- hourly_monthly_data %>%
    pivot_longer(cols = c(total_input, total_output),
                 names_to = "type",
                 values_to = "value")

  # Stacked area chart
  ggplot(hourly_monthly_data_long, aes(x = hour, y = value, fill = month, group = month)) +
    geom_area(position = "stack", alpha = 0.8) +
    facet_wrap(~ type, ncol = 1) +  # Separate panels for INPUT and OUTPUT
    labs(title = "Stacked Area Chart of Hourly Input and Output by Month",
         x = "Hour",
         y = "Sum of Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_viridis_d()  # Use a color gradient to distinguish months
}


plot_line_chart <- function(df) {
  # Data preprocessing
  df$hour <- format(df$timestamp, "%H")
  df$month <- format(df$timestamp, "%Y-%m")

  hourly_monthly_data <- df %>%
    group_by(month, hour) %>%
    summarise(
      total_input = sum(INPUT, na.rm = TRUE),
      total_output = sum(OUTPUT, na.rm = TRUE),
      .groups = "drop"  # Drop the grouping to avoid issues with subsequent operations
    )

  hourly_monthly_data_long <- hourly_monthly_data %>%
    pivot_longer(cols = c(total_input, total_output),
                 names_to = "type",
                 values_to = "value")

  # Line plot
  ggplot(hourly_monthly_data_long, aes(x = hour, y = value, color = month, group = month)) +
    geom_line() +
    facet_wrap(~ type, ncol = 1) +  # Separate panels for INPUT and OUTPUT
    labs(title = "Hourly Input and Output by Month",
         x = "Hour",
         y = "Sum of Values",
         color = "Month") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
