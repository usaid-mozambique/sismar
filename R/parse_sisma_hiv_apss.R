#' A specific helper function for parsing a cleaned CSV export from SISMA
#'
#' @param file Dataframe cleaned via reshape_sisma
#'
#' @return A tidy format of SISMA prep dataframe
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_hiv_apss()}


parse_sisma_hiv_apss <- function(file) {

  df <- file %>%

    dplyr::left_join(data_sisma_hiv_apss_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::select(starts_with("period"), snu, psnu, sitename, sisma_uid, indicator = indicator_new, age, sex, value)

  return(df)

}
