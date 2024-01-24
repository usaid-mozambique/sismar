#' Create a report for all datasets in a specifical local folder.
#'
#' @param file dataset as a tibble
#' @param period_selected relevant month - "2023-01"
#'
#' @return a tibble with expected and actual results
#' @export
#'
#' @examples
#'  \dontrun{
#'    report_create_all(file, period_selected)
#'  }
#'
report_create_all <- function(file, period_selected) {

  temp <-  clean_sisma_csv(file) %>%
    mutate(file_name = tools::file_path_sans_ext(basename(file)),
           type = stringr::str_split_i(file_name, "_20", 1),
           type = dplyr::case_when(type == "apss" ~ "apss_pp",
                                   type == "smi_ccr" ~ "ccr",
                                   type == "smi_cpn" ~ "ptv",
                                   .default = type),
           name = dplyr::case_when(type == "tarv" ~ "HIV TARV",
                                   type == "apss_pp" ~ "HIV APSS",
                                   type == "prep" ~ "HIV PREP",
                                   type == "ccr" ~ "SMI-CCR",
                                   type == "ptv" ~ "SMI-CPN",
                                   .default = type
           )
    )

  #create report_type for use in report_create
  report_type <- temp %>%
    dplyr::select(type) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  #create report_name for use in parse_sisma_csv
  report_name <- temp %>%
    dplyr::select(name) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  temp_1 <- temp %>%
    parse_sisma_csv(report_name) %>%
    report_create(report_type = report_type, period_selected)

  return(temp_1)

}

