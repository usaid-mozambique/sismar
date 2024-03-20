#' Create tidy dataframes for CCD services
#'
#' `parse_sisma_smi_cpp` produces a tidy dataframe from an object passed in by
#' `sisma_clean_csv`. It engineers useful data features such as sex, age,
#' indicator disaggregation, sub-group type, etc.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_smi_ccd` returns a tidy object with 16 columns of
#'   site metadata, indicator features and results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_smi_cpp()}


parse_sisma_smi_ccd <- function(df) {

  df <- df %>%

    dplyr::mutate(period_cohort = NA,
                  sub_group = NA_character_) %>%
    dplyr::left_join(data_sisma_smi_ccd_map, by = "indicator") %>%
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
