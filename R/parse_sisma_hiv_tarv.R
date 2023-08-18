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
#'  df <- parse_sisma_hiv_tarv()}

parse_sisma_hiv_tarv <- function(file) {

  df_all <- file %>%

    dplyr::left_join(data_sisma_hiv_tarv_map, by = "indicator") %>%
    tidyr::drop_na(tidyselect::any_of(c("indicator_new", "value"))) %>%
    dplyr::select(sisma_uid, snu, psnu, sitename, period, indicator = indicator_new, age, sex, disaggregate, value)

  return(df_all)

}
