#' Arrumar quadros de dados para os serviços de Profilaxia Pré-Exposição (PrEP)
#'
#' `parse_sisma_hiv_prep` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_hiv_prep` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_hiv_prep()}


parse_sisma_hiv_prep <- function(df) {

  df <- df %>%

    dplyr::left_join(data_sisma_hiv_prep_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(period_cohort = NA,
                  source = "LdR PrEP",
                  disaggregate_sub = NA_character_,
                  sub_group = NA_character_,
                  age_coarse = dplyr::case_when(age == "15-19" ~ "15+", # correct
                                                age == "20-24" ~ "15+",
                                                age == "25-29" ~ "15+",
                                                age == "25+" ~ "15+"),
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

  return(df)

}
