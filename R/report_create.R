#' Create a dataset for completeness check
#'
#' @param data Data received from all sites
#' @param report_type sites that should deliver data
#'
#' @return a tibble with expected vs actual data submitted at a site level
#' @export
#'
#' @examples
#'  \dontrun{
#'    report_create(data_delivered, sites)
#'  }
#'

report_create <- function(data, report_type){

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

  val_period <- df_tarv %>%
    select(periodo) %>%
    filter(periodo == max(periodo)) %>%
    mutate(periodo == as.character(periodo))

  val_period <- unique(val_period$periodo)

  val_period <- as.character(val_period)

  data <- data %>%
    janitor::clean_names() %>%
    dplyr::select(sisma_uid, periodo) %>%
    dplyr::filter(periodo == max(periodo)) %>% # new code but consider converting to argument
    dplyr::mutate("delivered" = 1) %>%
    dplyr::distinct(sisma_uid, delivered)

  report <- reporting_index %>%
    dplyr::left_join(data, by = "sisma_uid") %>%
    tidyr::replace_na(list("expected" = 0, "delivered" = 0)) %>%
    dplyr::mutate(period = val_period) %>%
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
