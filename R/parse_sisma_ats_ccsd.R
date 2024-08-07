#' Create tidy dataframes for well and sick-child HIV testing services
#'
#' `parse_sisma_ats_ccsd` produces a tidy dataframe from an object passed in by
#' `sisma_clean_csv`. It engineers useful data features such as sex, age,
#' indicator disaggregation, sub-group type, etc.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_ats_ccsd` returns a tidy object with 16 columns of
#'   site metadata, indicator features and results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_ats_ccsd()}

parse_sisma_ats_ccsd <- function(df) {

  df_all <- df %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(data_sisma_ats_ccsd_map, by = "indicator") %>%
    dplyr::filter(!is.na(indicator_new)) %>%
    dplyr::mutate(
      period_cohort = NA_character_,
      source = "LdR SMI",
      sub_group = NA_character_,
      age = NA_character_,
      indicator = "ATS_TST")

  df_pos <- df_all %>%
    dplyr::filter(result_status == "Positivo") %>%
    dplyr::mutate(indicator = dplyr::case_when(indicator == "ATS_TST" ~ "ATS_TST_POS"))


  df_parse <- dplyr::bind_rows(df_all, df_pos) %>%
    dplyr::select(sisma_uid,
                  snu, psnu,
                  sitename,
                  period,
                  period_cohort,
                  indicator = indicator_new,
                  source,
                  disaggregate,
                  disaggregate_sub,
                  sub_group,
                  sex,
                  age_coarse,
                  age,
                  result_status,
                  value)


  return(df_parse)

}
