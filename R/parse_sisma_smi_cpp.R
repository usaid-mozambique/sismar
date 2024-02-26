#' A specific helper function for parsing a cleaned CSV export from SISMA
#'
#' @param file Dataframe cleaned via reshape_sisma
#'
#' @return A tidy format of SISMA cpp dataframe
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_smi_cpp()}


parse_sisma_smi_cpp <- function(file) {

  df <- file %>%

    dplyr::mutate(period_cohort = NA_character_) %>%
    dplyr::left_join(data_sisma_smi_cpp_map, by = "indicator") %>%
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
