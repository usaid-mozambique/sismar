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
#'  df <- parse_sisma_hiv_tarv()}

parse_sisma_hiv_tarv <- function(file) {

  df <- file %>%

    dplyr::left_join(data_sisma_hiv_tarv_map, by = "indicator") %>%
    tidyr::drop_na(tidyselect::any_of(c("indicator_new", "source", "value"))) %>%
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

  df_activos_prev <- df %>%
    dplyr::filter(indicator == "TX_ACTIVO") %>%
    dplyr::mutate(indicator = "TX_ACTIVO_PREV") %>%
    dplyr::mutate(period = period + months(1))


  df <- dplyr::bind_rows(df, df_activos_prev)

  return(df)

}
