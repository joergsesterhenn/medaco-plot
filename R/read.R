library(readr, warn.conflicts = FALSE)

#' Get List of Files in Directory
#'
#' Retrieves a list of all `.CSV` files in the specified directory.
#'
#' @param data_path Character string specifying the directory path to search.
#' @return A character vector of file paths for `.CSV` files in the directory.
#' @examples
#' get_files_in_path("data/")
#' @export
get_files_in_path <- function(data_path) {
  return(
    list.files(
      path = data_path, recursive = TRUE,
      pattern = "\\.CSV$",
      full.names = TRUE
    )
  )
}

#' Read Power Data
#'
#' Reads power data from `.CSV` files in the specified directory,
#' selecting specific columns and using a custom locale for decimal
#' and grouping marks.
#'
#' @param data_path Character string specifying the directory containing
#' `.CSV` files to read.
#' @return A data frame containing timestamp, input, and output columns
#' from the files.
#' @importFrom readr read_delim locale
#' @examples
#' read_power_data("data/")
#' @export
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
