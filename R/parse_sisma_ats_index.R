#' Create tidy dataframes for HIV Index-Case Testing services
#'
#' `parse_sisma_ats_index` produces a tidy dataframe from an object passed in by
#' `sisma_clean_csv`. It engineers useful data features such as sex, age,
#' indicator disaggregation, sub-group type, etc.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_ats_index` returns a tidy object with 16 columns of
#'   site metadata, indicator features and results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_ats_index()}

parse_sisma_ats_index <- function(df) {

  df_all <- df %>%

    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(data_sisma_ats_ci_map, by = "indicator") %>%
    dplyr::mutate(
      period_cohort = NA_character_,
      disaggregate_sub = NA_character_,
      source = "LdR ATS",
      age = NA_character_,
      sex = NA_character_)

  df_pos <- df_all %>%
    dplyr::filter(result_status == "Positivo") %>%
    dplyr::mutate(indicator = dplyr::case_when(indicator == "ATS_CI_TST" ~ "ATS_CI_TST_POS"))

  df_parse <- dplyr::bind_rows(df_all, df_pos) %>%
    dplyr::select(sisma_uid,
                  snu,
                  psnu,
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
