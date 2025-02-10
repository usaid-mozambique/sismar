#' Arrumar quadros de dados para os serviços de testagem do HIV reportado a
#' partir do registo ATS
#'
#' `parse_sisma_ats_results` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_ats_results` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_ats_results()}

parse_sisma_ats_results <- function(df) {

  df <- df %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(data_sisma_ats_results_map, by = "indicator") %>%
    dplyr::mutate(
      indicator_new = "ATS_TST",
      source = "LdR ATS",
      age_coarse = dplyr::case_when(age == "<01"   ~ "<15",
                                    age == "01-09" ~ "<15",
                                    age == "10-14" ~ "<15"),
      age_coarse = tidyr::replace_na(age_coarse, "15+")
    )

  df_pos <- df %>%
    dplyr::filter(result_status == "Positivo") %>%
    dplyr::mutate(indicator_new = "ATS_TST_POS")

  df <- dplyr::bind_rows(df, df_pos) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}
