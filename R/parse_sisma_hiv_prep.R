#' Create tidy dataframes for PrEP services
#'
#' `parse_sisma_hiv_prep` produces a tidy dataframe from an object passed in by
#' `sisma_clean_csv`. It engineers useful data features such as sex, age,
#' indicator disaggregation, sub-group type, etc.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_hiv_prep` returns a tidy object with 16 columns of
#'   site metadata, indicator features and results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_hiv_prep()}


parse_sisma_hiv_prep <- function(df) {

  df <- df %>%

    dplyr::left_join(data_sisma_hiv_prep_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(period_cohort = NA,
                  source = "LdR PrEP",
                  disaggregate_sub = NA_character_,
                  sub_group = NA_character_,
                  age_coarse = dplyr::case_when(age == "15-19" ~ "15+", # correct
                                                age == "20-24" ~ "15+",
                                                age == "25-29" ~ "15+",
                                                age == "25+" ~ "15+"),
                  result_status = NA_character_) %>%
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
