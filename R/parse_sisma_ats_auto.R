#' Arrumar quadros de dados para os serviços de auto-diagnóstico do HIV
#'
#' `parse_sisma_ats_auto` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @param df Objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`
#'
#' @return  `parse_sisma_ats_auto` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_ats_auto()}

parse_sisma_ats_auto <- function(df) {

  df_all <- df %>%

    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(data_sisma_ats_autoteste_map, by = "indicator") %>%
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

  return(df_all)


}
