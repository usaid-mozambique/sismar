#' Read example SISMA CSV data into environment
#'
#' @param example_data The name of the example data file to read
#'
#' @return A data frame with the example data
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- example_data()}

read_example_csv <- function(example_data) {

  # Use system.file to get the full path of the CSV in the package
  csv_path <- system.file("extdata", example_data, package = "sismar")

  # Check if the file path returned by system.file is empty (meaning the file wasn't found)
  if (csv_path == "") {
    stop("Could not find the example data file in the package.")
  }

  # Check if the file actually exists at the specified path
  if (!file.exists(csv_path)) {
    stop("The file does not exist at the specified path: ", csv_path)
  }

  # Load the data as a data frame (you can use read_csv from readr or base R read.csv)
  data <- readr::read_csv(csv_path)

  return(data)

}
