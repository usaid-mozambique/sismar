#' Create tidy dataframes for user-specified SISMA .csv services
#'
#' @description `process_sisma_csv` is a wrapper function that cleans
#' and transforms SISMA tabular exports saved locally in .csv format
#'
#'   ## Input File Types
#' * SMI-CPN: CPN, Saude Materna Infantil
#' * SMI-MAT: Maternidade, Saude Materna Infantil
#' * SMI-CCR: CCR, Saude Materna Infantil
#' * SMI-CPP: CCP, Saude Materna Infantil
#' * SMI-CCD: CCD, Saude Materna Infantil
#' * SMI-CCS: CCS, Saude Materna Infantil
#' * SMI-UG: UG, Saude Materna Infantil
#' * SMI-PAV: PAV, Saude Materna Infantil
#' * ATS Result: ATS Resultados, Programa de HIV
#' * ATS History: ATS Historial e Populacoes Chave, Programa de HIV
#' * ATS CI: ATS Caso Indice e Ligacao, Programa de HIV
#' * ATS SAAJ: ATS Serviço Amigo Adolescente Joven
#' * ATS CCSD: Consulta da Crianca Sadia e Consulta da Crianca Doente
#' * ATS SMI: ATS especifico a SMI (outros acomopanhantes na CPN, etc.)data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==
#' * ATS Auto: ATS Autotestagem, Programa de HIV
#' * HIV TARV: Tratamento Antiretroviral (TARV), Programa de HIV
#' * HIV PREP: Profilaxia Pré-Exposição (PrEP), Programa de HIV
#' * HIV APSS: Apoio Psicossocial (APSS), Programa de HIV
#' * HIV ITS: Infecções de Transmissão Sexual (ITS), Programa de HIV
#' * HIV AJMHCMM: Adolescent Joven Mentor, Homen Campeao, Maes Mentora
#' * HIV DAH: Doenca Avancada do HIV
#'
#' @param file Path of sisma csv input
#' @param type Type of sisma csv export (SMI-CPN, ATS Result, etc.)
#' @param language Language of output variable names (portuguese or english)
#'
#' @return A tidy dataframe of SISMA program results
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
