library(readr)

get_files_in_path <- function(data_path) {
  return(
    list.files(
      path = data_path, recursive = FALSE,
      pattern = "\\.CSV$",
      full.names = TRUE
    )
  )
}

# reads power data from data_path
read_power_data <- function(data_path) {
  # collect all datafiles
  power_data_files <- get_files_in_path(data_path)

  return(
    readr::read_delim(
      file = power_data_files,
      delim = ";",
      skip = 6,
      locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
      col_names = c("timestamp", "INPUT", "C", "OUTPUT", "E"),
      col_select = c("timestamp", "INPUT", "OUTPUT"),
      col_types = readr::cols(
        "timestamp" = readr::col_datetime(format = "%d.%m.%Y %H:%M"),
        "INPUT" = readr::col_double(),
        "OUTPUT" = readr::col_double()
      ),
      show_col_types = FALSE
    )
  )
}
