#' Load a CSV file for AutoCDAScorer Dataframe
#'
#' This function loads a CSV file into a dataframe, ensuring that the file exists and contains the required columns: "img", "x1", "x2", "y1", and "y2".
#'
#' @param filepath A string representing the path to the CSV file.
#'
#' @return A dataframe containing the data from the CSV file if it exists and contains the required columns.
#' @export
#'
#' @examples
#' df <- load_cdascorer_dataframe("path/to/data.csv")
load_cdascorer_dataframe <- function(filepath) {
  if (!file.exists(filepath)) stop("File does not exist")

  df <- readr::read_csv(filepath, show_col_types = FALSE)

  print(colnames(df))

  required_cols <- c("img", "x1", "x2", "y1", "y2")
  if (!all(required_cols %in% colnames(df))) stop("Missing required columns")

  return(df)
}
