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
#'  df <- parse_sisma_ats_history()}

parse_sisma_ats_history <- function(file) {

  df_all <- file %>%
    dplyr::left_join(data_sisma_ats_hist_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(
      period_cohort = NA_character_,
      disaggregate_sub = NA_character_,
      age_coarse = NA_character_,
      source = "LdR ATS",
      age = NA_character_,
      sex = NA_character_)

  # commented code should be reviewed if there is a desire to include separate ATS_KP_POS indicator

  # df_pos <- df_all %>%
  #   dplyr::filter(result_status == "Positivo") %>%
  #   dplyr::mutate(indicator = dplyr::case_when(indicator == "ATS_KP" ~ "ATS_KP_POS"))

  df_parse <- df_all |>
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
