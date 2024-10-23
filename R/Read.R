library(readr)

# reads power data from data_path
read_power_data <- function(data_path) {
  # collect all datafiles
  power_data_files <- list.files(
    path = data_path, recursive = FALSE,
    pattern = "\\.CSV$",
    full.names = TRUE
  )

  # read to memory
  df <- readr::read_csv2(
    power_data_files,
    skip = 6,
    id = "origin_file",
    col_names = c("timestamp", "INPUT", "C", "OUTPUT", "E"),
    col_select = c("timestamp", "INPUT", "OUTPUT"),
    show_col_types = FALSE
  )

  # fix datetime
  df$timestamp <- readr::parse_datetime(df$timestamp, format = "%d.%m.%Y %H:%M")

  return(subset(df, select = c("timestamp", "INPUT", "OUTPUT")))
}
