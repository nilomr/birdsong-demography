#' Read CSV file
#'
#' This function reads a CSV file from a specified path and returns a data frame.
#'
#' @param file_path Path to the CSV file
#'
#' @return A data frame containing the CSV file data
#' @export
read_csv_file <- function(file_path) {
    readr::read_csv(
        file_path,
        guess_max = 10000,
        col_types = readr::cols()
    )
}
