#' Arrumar quadros de dados para os serviços de testagem do HIV em populações-chave
#' e vulneráveis e dados da historial de testagem do HIV
#'
#' `parse_sisma_ats_history` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_ats_history` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_ats_history()}

parse_sisma_ats_history <- function(df) {

  df_all <- df %>%
    dplyr::left_join(data_sisma_ats_hist_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(
      period_cohort = NA_character_,
      disaggregate_sub = NA_character_,
      age_coarse = NA_character_,
      source = "LdR ATS",
      age = NA_character_,
      sex = NA_character_)

  df_pos <- df_all %>%
    dplyr::filter(result_status == "Positivo") %>%
    dplyr::mutate(indicator = dplyr::case_when(indicator == "ATS_KP" ~ "ATS_KP_POS"))

  df_parse <- dplyr::bind_rows(df_all, df_pos) %>%
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
