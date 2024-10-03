source("R/Read.R")
source("R/Plot.R")

power_data = read_power_data(data_path="data")
plot_aggregated_by_month(power_data)
plot_aggregated_by_hour(power_data)
plot_by_hour_and_month(power_data)
plot_heatmap(power_data)
plot_ridgeline(power_data)
plot_stacked_area(power_data)
plot_line_chart(power_data)

