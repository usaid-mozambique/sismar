#' Arrumar quadros de dados para os serviços de testagem do HIV nas consultas
#' da criança sadia (CCS) e em risco (CCR)
#'
#' `parse_sisma_ats_ccsd` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_ats_ccsd` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_ats_ccsd()}

parse_sisma_ats_ccsd <- function(df) {

  df_all <- df %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(data_sisma_ats_ccsd_map, by = "indicator") %>%
    dplyr::filter(!is.na(indicator_new)) %>%
    dplyr::mutate(
      period_cohort = NA_character_,
      source = "LdR SMI",
      sub_group = NA_character_,
      age = NA_character_,
      indicator = "ATS_TST")

  df_pos <- df_all %>%
    dplyr::filter(result_status == "Positivo") %>%
    dplyr::mutate(indicator = dplyr::case_when(indicator == "ATS_TST" ~ "ATS_TST_POS"))


  df_parse <- dplyr::bind_rows(df_all, df_pos) %>%
    dplyr::select(sisma_uid,
                  snu, psnu,
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
