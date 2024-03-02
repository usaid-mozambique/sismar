#' A specific helper function for parsing a cleaned CSV export from SISMA
#'
#' @param file Dataframe cleaned via reshape_sisma
#'
#' @return A tidy format of SISMA PAV dataframe
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_smi_pav()}


parse_sisma_smi_pav <- function(file) {

  df <- file %>%

    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(data_sisma_smi_pav_map, by = "indicator") %>%
    dplyr::mutate(period_cohort = NA,
                  disaggregate_sub = NA_character_,
                  sub_group = NA_character_,
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
