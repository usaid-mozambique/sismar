#' Create a dataset for completeness check
#'
#' @param data Data received from all sites
#' @param report_type project required e.g. tarv
#' @param period_selected select period to show
#'
#' @return a tibble with expected vs actual data submitted at a site level
#' @export
#'
#' @examples
#'  \dontrun{
#'    report_create(data, report_type, period_selected)
#'  }
#'

report_create <- function(data, report_type, period_selected){

  reporting_index <- mozR::pull_sitemap(sheetname = "list_sisma_reporting") %>%
    janitor::clean_names() %>%
    dplyr::select(us_id, report_type, prov, dist, us) %>%
    dplyr::rename("sisma_uid" = "us_id",
                  "snu1" = "prov",
                  "psnu" = "dist",
                  "sitename" = "us",
                  "expected" = 2)%>%
    dplyr::mutate(type = report_type) %>%
    tidyr::drop_na(sisma_uid) #remove if the SISMA_UID is missing

  data <- data %>%
    janitor::clean_names() %>%
    dplyr::select(sisma_uid, period) %>%
    dplyr::filter(period == period_selected) %>% # new code but consider converting to argument
    dplyr::mutate("delivered" = 1) %>%
    dplyr::distinct(sisma_uid, delivered)

  report <- reporting_index %>%
    dplyr::left_join(data, by = "sisma_uid") %>%
    tidyr::replace_na(list("expected" = 0, "delivered" = 0)) %>%
    dplyr::mutate(period = period_selected) %>%
    dplyr::select(sisma_uid,
                  provincia = snu1,
                  distrito = psnu,
                  us = sitename,
                  tipo = type,
                  periodo = period,
                  esperado = expected,
                  submetido = delivered)

  return(report)

}
