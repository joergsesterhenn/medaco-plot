library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)
library(tsibble)
library(lubridate)





plot_aggregated_by_month <- function(df) {
  # Ensure the data is a tsibble
  df_tsibble <- df %>%
    as_tsibble(index = timestamp)

  # Use tsibble functions to aggregate by year-month
  monthly_data <- df_tsibble %>%
    index_by(year_month = yearmonth(timestamp)) %>%
    summarise(
      total_input = sum(INPUT, na.rm = TRUE),
      total_output = sum(OUTPUT, na.rm = TRUE)
    )

  # Reshape data for plotting
  monthly_data_long <- monthly_data %>%
    pivot_longer(cols = c(total_input, total_output),
                 names_to = "type",
                 values_to = "value")

  # Plot the data
  ggplot(monthly_data_long, aes(x = year_month, y = value, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Monthly Input and Output Sums",
         x = "Month",
         y = "Sum of Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("total_input" = "blue", "total_output" = "red"))
}

plot_aggregated_by_hour <- function(df) {
  # Convert to tsibble
  df_tsibble <- df %>%
    as_tsibble(index = timestamp)

  # Summing INPUT and OUTPUT values for each hour
  hourly_data <- df_tsibble %>%
    index_by(hour = hour(timestamp)) %>%
    summarise(
      total_input = sum(INPUT, na.rm = TRUE),
      total_output = sum(OUTPUT, na.rm = TRUE)
    )

  # Reshape data for plotting
  hourly_data_long <- hourly_data %>%
    pivot_longer(cols = c(total_input, total_output),
                 names_to = "type",
                 values_to = "value")

  # Plot the data
  ggplot(hourly_data_long, aes(x = hour, y = value, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Hourly Input and Output Sums",
         x = "Hour",
         y = "Sum of Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("total_input" = "blue", "total_output" = "red"))
}

plot_by_hour_and_month <- function(df) {
  # Convert to tsibble
  df_tsibble <- df %>%
    as_tsibble(index = timestamp)

  # Group by month and hour, summing for each hour across 15-minute intervals
  hourly_monthly_data <- df_tsibble %>%
    index_by(month = yearmonth(timestamp)) %>%
    mutate(hour = hour(timestamp)) %>%
    group_by(month, hour) %>%
    summarise(
      total_input = sum(INPUT, na.rm = TRUE),
      total_output = sum(OUTPUT, na.rm = TRUE)
    )

  # Reshape data for plotting
  hourly_monthly_data_long <- hourly_monthly_data %>%
    pivot_longer(cols = c(total_input, total_output),
                 names_to = "type",
                 values_to = "value")

  # Plot with correct hour and month
  ggplot(hourly_monthly_data_long, aes(x = hour, y = value, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ month, ncol = 3) +
    labs(title = "Hourly Input and Output Sums by Month",
         x = "Hour",
         y = "Sum of Values") +
    theme(axis.text.x = element_text(hjust = 1)) +
    scale_fill_manual(values = c("total_input" = "blue", "total_output" = "red")) +
    scale_x_continuous(breaks = 0:23)  # Force all hours to appear on the x-axis
}

plot_heatmap <- function(df) {
  # Convert to tsibble
  df_tsibble <- df %>%
    as_tsibble(index = timestamp)

  # Summing INPUT and OUTPUT values for each hour and month
  hourly_monthly_data <- df_tsibble %>%
    index_by(month = yearmonth(timestamp), hour = hour(timestamp)) %>%
    summarise(
      total_input = sum(INPUT, na.rm = TRUE),
      total_output = sum(OUTPUT, na.rm = TRUE)
    )

  # Reshape data for plotting
  hourly_monthly_data_long <- hourly_monthly_data %>%
    pivot_longer(cols = c(total_input, total_output),
                 names_to = "type",
                 values_to = "value")

  # Heatmap plot
  ggplot(hourly_monthly_data_long, aes(x = hour, y = month, fill = value)) +
    geom_tile() +
    facet_wrap(~ type, ncol = 1) +
    scale_fill_gradient(low = "white", high = "blue") +
    labs(title = "Heatmap of Hourly Input and Output by Month",
         x = "Hour",
         y = "Month",
         fill = "Sum of Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_ridgeline <- function(df) {
  # Convert to tsibble
  df_tsibble <- df %>%
    as_tsibble(index = timestamp)

  # Summing INPUT and OUTPUT values for each hour and month
  hourly_monthly_data <- df_tsibble %>%
    index_by(month = yearmonth(timestamp), hour = hour(timestamp)) %>%
    summarise(
      total_input = sum(INPUT, na.rm = TRUE),
      total_output = sum(OUTPUT, na.rm = TRUE)
    )

  # Reshape data for plotting
  hourly_monthly_data_long <- hourly_monthly_data %>%
    pivot_longer(cols = c(total_input, total_output),
                 names_to = "type",
                 values_to = "value")

  # Ridgeline plot
  ggplot(hourly_monthly_data_long, aes(x = as.numeric(hour), y = month, height = value, fill = type)) +
    geom_density_ridges(stat = "identity", alpha = 0.5, scale = 0.9) +
    facet_wrap(~ type, ncol = 1) +
    labs(title = "Ridgeline Plot of Hourly Input and Output by Month",
         x = "Hour",
         y = "Month") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("total_input" = "blue", "total_output" = "red"))
}

plot_stacked_area <- function(df) {
  # Convert to tsibble
  df_tsibble <- df %>%
    as_tsibble(index = timestamp)

  # Summing INPUT and OUTPUT values for each hour and month
  hourly_monthly_data <- df_tsibble %>%
    index_by(month = yearmonth(timestamp), hour = hour(timestamp)) %>%
    summarise(
      total_input = sum(INPUT, na.rm = TRUE),
      total_output = sum(OUTPUT, na.rm = TRUE)
    )

  # Reshape data for plotting
  hourly_monthly_data_long <- hourly_monthly_data %>%
    pivot_longer(cols = c(total_input, total_output),
                 names_to = "type",
                 values_to = "value")

  # Stacked area plot
  ggplot(hourly_monthly_data_long, aes(x = hour, y = value, fill = month, group = month)) +
    geom_area(position = "stack", alpha = 0.8) +
    facet_wrap(~ type, ncol = 1) +
    labs(title = "Stacked Area Chart of Hourly Input and Output by Month",
         x = "Hour",
         y = "Sum of Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_viridis_d()
}

plot_line_chart <- function(df) {
  # Convert to tsibble
  df_tsibble <- df %>%
    as_tsibble(index = timestamp)

  # Summing INPUT and OUTPUT values for each hour and month
  hourly_monthly_data <- df_tsibble %>%
    index_by(month = yearmonth(timestamp), hour = hour(timestamp)) %>%
    summarise(
      total_input = sum(INPUT, na.rm = TRUE),
      total_output = sum(OUTPUT, na.rm = TRUE)
    )

  # Reshape data for plotting
  hourly_monthly_data_long <- hourly_monthly_data %>%
    pivot_longer(cols = c(total_input, total_output),
                 names_to = "type",
                 values_to = "value")

  # Line plot
  ggplot(hourly_monthly_data_long, aes(x = hour, y = value, color = month, group = month)) +
    geom_line() +
    facet_wrap(~ type, ncol = 1) +
    labs(title = "Hourly Input and Output by Month",
         x = "Hour",
         y = "Sum of Values",
         color = "Month") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(breaks = 0:23)  # Force all hours to appear on the x-axis
}

# New Box Plot Function
plot_boxplot_by_month <- function(df) {
  # Convert to tsibble
  df_tsibble <- df %>%
    as_tsibble(index = timestamp)

  # Extract the month and plot boxplots for INPUT and OUTPUT
  df_tsibble <- df_tsibble %>%
    mutate(month = yearmonth(timestamp))

  # Boxplot
  ggplot(df_tsibble, aes(x = factor(month), y = INPUT)) +
    geom_boxplot(fill = "blue", alpha = 0.5) +
    geom_boxplot(aes(y = OUTPUT), fill = "red", alpha = 0.5) +
    labs(title = "Boxplot of Input and Output by Month",
         x = "Month",
         y = "Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_boxplot_by_hour <- function(df) {
  # Ensure df is a tibble and timestamp is properly formatted
  df <- df %>%
    as_tibble() %>%
    mutate(timestamp = as_datetime(timestamp))  # Convert timestamp to datetime if necessary

  # Convert to tsibble
  df_tsibble <- df %>%
    as_tsibble(index = timestamp)

  # Extract the hour from the timestamp
  df_tsibble <- df_tsibble %>%
    mutate(hour = hour(timestamp))

  # Plot boxplot for INPUT and OUTPUT by hour
  ggplot(df_tsibble, aes(x = factor(hour), y = INPUT)) +
    geom_boxplot(fill = "blue", alpha = 0.5) +
    geom_boxplot(aes(y = OUTPUT), fill = "red", alpha = 0.5) +
    labs(title = "Boxplot of Input and Output by Hour",
         x = "Hour",
         y = "Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_x_continuous(breaks = 0:23)  # Force all hours to appear on the x-axis
}
