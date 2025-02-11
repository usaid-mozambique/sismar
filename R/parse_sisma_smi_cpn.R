#' Arrumar quadros de dados para os serviços da Consulta Pré-natal (CPN)
#'
#' `parse_sisma_smi_cpn` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_smi_cpn` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_smi_cpn()}

parse_sisma_smi_cpn <- function(df) {

  df <- df %>%

    dplyr::left_join(data_sisma_smi_cpn_map, by = "indicator") %>%
    dplyr::mutate(period_cohort = dplyr::if_else(indicator %in% c("MG_1CON_MES", "MG_1CON_12SEM_MES"), period, period - months(6))) %>%
    dplyr::filter(!is.na(value)) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}
