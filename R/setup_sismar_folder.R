#' Cria uma estrutura organizacional (pastas) que é comum a todos os projectos sismar
#'
#' @param folder_list lista de pastas a instalar
#'
#' @return Um conjunto padrão de pastas no projeto para utilizar com as funções `sismar`
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  setup_sismar_folder()}

setup_sismar_folder <- function(

  folder_list = list("Data", "Data/sisma", "Data/disa", "Data/pepfar", "Data/ine", # data
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
