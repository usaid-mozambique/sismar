#' Arrumar quadros de dados para os serviços Apoio Psicossocial (APSS),
#' do PNC ITS-HIV/SIDA
#'
#' `parse_sisma_hiv_apss` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_hiv_apss` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_hiv_apss()}


parse_sisma_hiv_apss <- function(df) {

  df <- df %>%

    dplyr::left_join(data_sisma_hiv_apss_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}
