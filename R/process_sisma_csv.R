#' Function that cleans sisma csv exports and parses them into tidy dataframes
#'
#' @description process_sisma_csv cleans and transforms programmatic
#' data sets exported in CSV format from SISMA into tidy data frames.
#' Such structured data sets are key to facilitating subsequent analysis
#' in R/PowerBI/Tableau.
#'
#' ## Overview of programmatic data type options
#' * SMI-CPN: CPN, Saude Materna Infantil
#' * SMI-MAT: Maternidade, Saude Materna Infantil
#' * SMI-CCR: CCR, Saude Materna Infantil
#' * ATS Result: ATS Resultados, Programa de HIV
#' * ATS History: ATS Historial e Populacoes Chave, Programa de HIV
#' * ATS CI: ATS Caso Indice e Ligacao, Programa de HIV
#' * ATS SAAJ: ATS Serviço Amigo Adolescente Joven
#' * ATS CCSD: Consulta da Crianca Sadia e Consulta da Crianca Doente
#' * ATS SMI: ATS especifico a SMI (outros acomopanhantes na CPN, etc.)
#' * ATS Auto: ATS Autotestagem, Programa de HIV
#' * HIV TARV: Tratamento Antiretroviral (TARV), Programa de HIV
#' * HIV PREP: Profilaxia Pré-Exposição (PrEP), Programa de HIV
#' * HIV APSS: Apoio Psicossocial (APSS), Programa de HIV
#'
#' @param file Path of sisma csv input
#' @param type Type of sisma csv export (SMI-CPN, ATS-Result, etc.)
#' @param language Language of variable names (Portuguese or English)
#'
#' @return A tidy data frame of sisma program results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- process_sisma_csv()}

process_sisma_csv <- function(file, type, language = "portuguese"){

  df <- clean_sisma_csv(file) %>%
    parse_sisma_csv(type) %>%
    dplyr::mutate_if(is.character, ~tidyr::replace_na(., ""))

  if(language == "portuguese"){

    df <- df %>% dplyr::rename(periodo = period,
                               periodo_coorte = period_cohort,
                               provincia = snu,
                               distrito = psnu,
                               us = sitename,
                               indicador = indicator,
                               fonte = source,
                               idade = age,
                               idade_agrupada = age_coarse,
                               sexo = sex,
                               sub_grupo = sub_group,
                               resultado_estado = result_status,
                               disagregacao = disaggregate,
                               disagregacao_sub = disaggregate_sub,
                               valor = value)

    return(df)

  }

  if(language == "english"){

    return(df)

  }

  return(df)

}
