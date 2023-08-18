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
#'  df <- parse_sisma_ats_ccsd()}

parse_sisma_ats_ccsd <- function(file) {

  df_all <- file %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(data_sisma_ats_ccsd_map, by = "indicator") %>%
    dplyr::filter(!is.na(indicator_new)) %>%
    dplyr::mutate(
      source = "LdR SMI",
      sub_group = NA_character_,
      age = NA_character_,
      indicator = "ATS_TST")

  df_pos <- df_all %>%
    dplyr::filter(result_status == "Positivo") %>%
    dplyr::mutate(indicator = dplyr::case_when(indicator == "ATS_TST" ~ "ATS_TST_POS"))


  df_parse <- dplyr::bind_rows(df_all, df_pos) %>%
    dplyr::select(sisma_uid, snu, psnu, sitename, period, indicator = indicator_new, source, disaggregate, disaggregate_sub, sub_group, sex, age_coarse, age, result_status, value)


  return(df_parse)

}
