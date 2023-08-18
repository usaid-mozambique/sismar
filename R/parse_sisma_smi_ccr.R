#' A specific helper function for parsing a cleaned CSV export from SISMA
#'
#' @param file Dataframe cleaned via reshape_sisma
#'
#' @return A tidy format of SISMA CCR dataframe
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_smi_ccr()}


parse_sisma_smi_ccr <- function(file) {

  df <- file %>%

    dplyr::left_join(data_sisma_smi_ccr_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::select(starts_with("period"), snu, psnu, sitename, sisma_uid, indicator = indicator_new, disaggregate, value)

  return(df)

}
