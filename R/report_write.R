#' Print out SNU, PSNU and Site detailes to different tabs in an excel sheet
#'
#' @param data completeness check dataset
#' @param file path and filename to save to disk
#'
#' @return excel workbook with 3 tabs - SNU, PSNU and Site
#' @export
#'
#' @examples
#'  \dontrun{
#'    report_write(data, file)
#'  }
#'

report_write <- function(data, file){

  wb <-  openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb,"provincia")
  openxlsx::addWorksheet(wb,"distrito")
  openxlsx::addWorksheet(wb,"us")

  provincia_table <- data %>%
    dplyr::select(provincia, periodo, tipo, esperado, submetido) %>%
    group_by(provincia, periodo, tipo ) %>%
    dplyr::summarise_all(sum,na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(submetido_perc = scales::percent(submetido / esperado, accuracy = 0.1))
  openxlsx::writeData(wb, sheet = "provincia", x = provincia_table)

  distrito_table <- data %>%
    dplyr::select(provincia, distrito, periodo, tipo, esperado, submetido) %>%
    dplyr::group_by(provincia, distrito, periodo, tipo) %>%
    dplyr::summarise_all(sum,na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(submetido_perc = scales::percent(submetido / esperado, accuracy = 0.1))
  openxlsx::writeData(wb, sheet = "distrito", x = distrito_table)

  us_table <- data %>%
    dplyr::select(provincia, distrito, us, periodo, tipo, esperado, submetido)
  openxlsx::writeData(wb, sheet = "us", x = data)

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

}

