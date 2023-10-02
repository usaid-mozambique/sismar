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
#'    write_report(data, file)
#'  }
#'

write_report <- function(data, file){

  wb <-  openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb,"snu")
  openxlsx::addWorksheet(wb,"psnu")
  openxlsx::addWorksheet(wb,"sites")

  snu_table <- data %>%
    dplyr::select(snu1, period, type, expected, delivered) %>%
    group_by(snu1, period, type ) %>%
    dplyr::summarise_all(sum,na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(complete_percentage = scales::percent(delivered / expected, accuracy = 0.1))
  openxlsx::writeData(wb, sheet = "snu", x = snu_table)

  psnu_table <- data %>%
    dplyr::select(snu1, psnu, period, type, expected, delivered) %>%
    dplyr::group_by(snu1, psnu, period, type) %>%
    dplyr::summarise_all(sum,na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(complete_percentage = scales::percent(delivered / expected, accuracy = 0.1))
  openxlsx::writeData(wb, sheet = "psnu", x = psnu_table)

  site_table <- data %>%
    dplyr::select(snu1, psnu, sisma_uid, sitename, period, type, expected, delivered)
  openxlsx::writeData(wb, sheet = "sites", x = site_table)

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

}
