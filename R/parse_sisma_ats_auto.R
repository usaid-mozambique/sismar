#' A specific helper function for parsing a cleaned CSV export from SISMA
#'
#' @param file Dataframe cleaned via reshape_sisma
#'
#' @return A tidy format of SISMA dataframe
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_ats_auto()}

parse_sisma_ats_auto <- function(file) {

  df_all <- file %>%

    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(data_sisma_ats_autoteste_map, by = "indicator") %>%
    dplyr::select(sisma_uid, snu, psnu, sitename, period, indicator = indicator_new, age, sex, disaggregate, value)

  return(df_all)

}
