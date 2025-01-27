#' Arrumar quadros de dados para os serviços de Tratamento Antiretroviral (TARV)
#'
#' `parse_sisma_hiv_tarv` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_hiv_tarv` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_hiv_tarv()}

parse_sisma_hiv_tarv <- function(df) {

  df <- df %>%

    dplyr::left_join(data_sisma_hiv_tarv_map, by = "indicator") %>%
    tidyr::drop_na(tidyselect::any_of(c("indicator_new", "source", "value"))) %>%
    dplyr::mutate(period_cohort = as.Date(NA),
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
