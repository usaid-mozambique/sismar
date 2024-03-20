#' Create tidy dataframes for Antenatal Care
#'
#' `parse_sisma_smi_cpn` produces a tidy dataframe from an object passed in by
#' `sisma_clean_csv`. It engineers useful data features such as sex, age,
#' indicator disaggregation, sub-group type, etc.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_smi_cpn` returns a tidy object with 16 columns of
#'   site metadata, indicator features and results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_smi_cpn()}

parse_sisma_smi_cpn <- function(df) {

  df <- df %>%

    dplyr::left_join(data_sisma_smi_cpn_map, by = "indicator") %>%
    dplyr::mutate(period_cohort = dplyr::if_else(indicator %in% c("MG_1CON_MES", "MG_1CON_12SEM_MES"), period, period - months(6)),
                  sex = NA_character_,
                  sub_group = NA_character_,
                  age_coarse = NA_character_,
                  result_status = NA_character_,
                  disaggregate_sub = NA_character_) %>%
    dplyr::filter(!is.na(value)) %>%
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
                  age,
                  age_coarse,
                  result_status,
                  value)

  return(df)

}
