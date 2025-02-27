#' Launhc ShinyApp
#'
#' @return running shinyapp
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- run_sismar_app()}

run_sismar_app <- function() {
  app_dir <- system.file("app", package = "sismar/shiny-app")
  shiny::runApp(app_dir)
}
