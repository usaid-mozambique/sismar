#' Arrumar quadros de dados para os serviços de Infecções de Transmissão
#' Sexual (ITS) do PNC ITS-HIV/SIDA
#'
#' `parse_sisma_hiv_its` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_hiv_its` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_hiv_its()}

parse_sisma_hiv_its <- function(df) {

  df <- df %>%

    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(data_sisma_hiv_its_map, by = "indicator") %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}
