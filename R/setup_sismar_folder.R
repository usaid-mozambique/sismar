#' Configure project folder hierarchy
#'
#' @param folder_list A list of desired folders
#'
#' @return A standard set of in-project folders to use with `sismar` functions
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- setup_sismar_folder()}

setup_sismar_folder <- function(

  folder_list = list("Data", "Data/sismar", "Data/disa", "Data/pepfar", # data
                     "Data/sismar/processed", "Data/disa/processed", "Data/pepfar/processed", # data processed
                     "Images", "Scripts", "AI", "Dataout", "Data_public", "GIS", "Documents", "Graphics", "markdown") # other
  )

  {

  if(!is.list(folder_list))
    stop("Please provide a list of directories to create for the project.")
  usethis::ui_info("The following directories will be created (if they do no already):")
  print(glue::glue(crayon::green('{folder_list}')))
  suppressWarnings(
    purrr::walk(folder_list, ~dir.create(.))
  )

}
