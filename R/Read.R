library(readr)

read_power_data <- function(data_path) {
  # collect all datafiles
  power_data_files <- list.files(path = data_path, recursive = FALSE,
                                 pattern = "\\.CSV$",
                                 full.names = TRUE)

  # read to memory
  df <- read_csv2(
    power_data_files,
    skip=6,
    id = "origin_file",
    col_names = c("timestamp", "INPUT", "C","OUTPUT","E"),
    col_select =c("timestamp","INPUT", "OUTPUT")
  )

  # fix datetime
  df$timestamp <- parse_datetime(df$timestamp,format="%d.%m.%Y %H:%M")

  return(df)
}
