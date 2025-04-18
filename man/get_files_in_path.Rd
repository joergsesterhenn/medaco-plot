% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{get_files_in_path}
\alias{get_files_in_path}
\title{Get List of Files in Directory}
\usage{
get_files_in_path(data_path)
}
\arguments{
\item{data_path}{Character string specifying the directory path to search.}
}
\value{
A character vector of file paths for \code{.CSV} files in the directory.
}
\description{
Retrieves a list of all \code{.CSV} files in the specified directory.
}
\examples{
get_files_in_path("data/")
}
