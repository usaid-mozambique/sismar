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

  df_activos_prev <- df %>%
    dplyr::filter(indicator_new == "TX_ACTIVO") %>%
    dplyr::mutate(indicator_new = "TX_ACTIVO_PREV") %>%
    dplyr::mutate(period = period + months(1))

  df <- dplyr::bind_rows(df, df_activos_prev) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}
