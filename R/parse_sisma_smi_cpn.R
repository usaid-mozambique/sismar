#' A specific helper function for parsing a cleaned CSV export from SISMA
#'
#' @param file Dataframe cleaned via reshape_sisma
#'
#' @return A tidy format of SISMA cpn dataframe
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_smi_cpn()}


parse_sisma_smi_cpn <- function(file) {

  df <- file %>%

    dplyr::left_join(data_sisma_smi_cpn_map, by = "indicator") %>%
    dplyr::mutate(period_cohort = dplyr::if_else(indicator %in% c("MG_1CON_MES", "MG_1CON_12SEM_MES"), period, period - months(6))) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::select(starts_with("period"), snu, psnu, sitename, sisma_uid, indicator = indicator_new, age, disaggregate, value)

  return(df)

}
