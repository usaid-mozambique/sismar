#' Create a dataset for completeness check
#'
#' @param data_delivered Data received from all sites
#' @param sites sites that should deliver data
#'
#' @return a tibble with expected vs actual data submitted at a site level
#' @export
#'
#' @examples
#'  \dontrun{
#'    create_report(data_delivered, sites)
#'  }
#'

create_report <- function(data_delivered, sites){

  actual_df <- data_delivered %>%
    janitor::clean_names() %>%
    dplyr::select(sisma_uid) %>%
    dplyr::mutate("delivered" = 1) %>%
    dplyr::distinct()

  complete_df <- sites %>%
    dplyr::left_join(actual_df, by = "sisma_uid") %>%
    tidyr::replace_na(list("expected" = 0, "delivered" = 0)) %>%
    dplyr::mutate(period = PERIOD)

}
