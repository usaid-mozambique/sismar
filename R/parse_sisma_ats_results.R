#' A helper function for parsing a cleaned CSV export of ATS Results from SISMA
#'
#' @param file Dataframe cleaned via reshape_sisma
#'
#' @return A tidy dataframe of SISMA ATS Results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_ats_results()}

parse_sisma_ats_results <- function(file) {

  df_all <- file %>%

    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(data_sisma_ats_results_map, by = "indicator") %>%
    dplyr::mutate(
      indicator = "ATS_TST",
      age_coarse = dplyr::case_when(age == "<01"   ~ "<15",
                                    age == "01-09" ~ "<15",
                                    age == "10-14" ~ "<15"),
      age_coarse = tidyr::replace_na(age_coarse, "15+"),
      disaggregate_sub = NA_character_,
      source = "LdR ATS",
      sub_group = NA_character_)

  df_pos <- df_all %>%
    dplyr::filter(result_status == "Positivo") %>%
    dplyr::mutate(indicator = dplyr::case_when(indicator == "ATS_TST" ~ "ATS_TST_POS"),
                  period_cohort = NA)

  df_parse <- dplyr::bind_rows(df_all, df_pos) %>%
    dplyr::select(sisma_uid,
                  snu,
                  psnu,
                  sitename,
                  period,
                  period_cohort,
                  indicator,
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
