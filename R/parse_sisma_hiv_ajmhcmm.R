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
#'  df <- parse_sisma_hiv_ajm_hc_mm()}

parse_sisma_hiv_ajm_hc_mm <- function(file) {

  df <- file %>%

    dplyr::left_join(data_sisma_hiv_ajmhcmm_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(period_cohort = NA,
                  disaggregate_sub = NA_character_,
                  sub_group = NA_character_,
                  age_coarse = dplyr::case_when(age == "15-24" ~ "15+",
                                                age == "25-29" ~ "15+",
                                                age == "30+" ~ "15+",
                                                age == "25+" ~ "15+",
                                                age == "<05" ~ "<15",
                                                age == "05-09" ~ "<15",
                                                age == "10-14" ~ "<15")) %>%
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
