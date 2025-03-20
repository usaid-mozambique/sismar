#' Limpar ficheiro bruto SISMA
#'
#' @description
#' `clean_sisma_df()` pega num objecto de dados importado através de `process_sisma_export` e devolve-o transformado numa estrutura longa, cria uma coluna de período formatada e devolve um objeto de dados limpo com 7 colunas
#'
#' @param df um objecto de dados importado através de `process_sisma_export`
#'
#' @return `clean_sisma_df()` devolve um objeto de dados limpo com 7 colunas (período, província, distrito, us, sisma_uid, indicador e valor)
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- clean_sisma_df()}

clean_sisma_df <- function(df) {

  df <- df |>

    dplyr::filter(!organisationunitid == "abg5UReivZX") |> # for whatever reason this site creates issues

    dplyr::select(!c(periodname,
                     periodid,
                     perioddescription,
                     organisationunitcode,
                     organisationunitdescription,
                     orgunitlevel1,
                     organisationunitname)) |>

    tidyr::pivot_longer(!c(periodcode,
                           orgunitlevel2,
                           orgunitlevel3,
                           orgunitlevel4,
                           organisationunitid),
                        names_to = "indicator",
                        values_to = "value",
                        values_transform = list(value = as.numeric)) |>

    dplyr::mutate(periodcode = stringr::str_replace_all(periodcode,
                                                        c("Q1" = "03",
                                                          "Q2" = "06",
                                                          "Q3" = "09",
                                                          "Q4" = "12")),
                  periodcode = paste0(periodcode, "01"),
                  periodcode = lubridate::ymd(periodcode),
                  dplyr::across(c(orgunitlevel2, orgunitlevel3), ~ stringr::str_to_title(.))) |>

    dplyr::select(period = periodcode,
                  snu = orgunitlevel2,
                  psnu = orgunitlevel3,
                  sitename = orgunitlevel4,
                  sisma_uid = organisationunitid,
                  indicator,
                  value) |>

    dplyr::arrange(period, snu, psnu, sitename, indicator)

  return(df)

}


#' Processar: Dados em formato .csv do SISMA
#'
#' @description `process_sisma_export()` S para uma exportação SISMA csv em bruto e executa várias acções de processamento, incluindo arrumação e criação de caraterísticas de dados.
#'
#' @param filename Caminho do ficheiro .csv exportado do SISMA
#' @param language Linga dos nomes das variáveis no quadro gerado (português ou inglês)
#'
#' @return devolve um quadro de dados arrumado com 16 colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- process_sisma_export()}


process_sisma_export <- function(filename, language = "portuguese") {

  # set column types for input file
  column_types <- cols(
    .default = col_double(),
    periodid  = col_character(), periodname = col_character(), periodcode = col_character(),
    perioddescription = col_character(), orgunitlevel1 = col_character(), orgunitlevel2 = col_character(),
    orgunitlevel3 = col_character(), orgunitlevel4 = col_character(), organisationunitid = col_character(),
    organisationunitname = col_character(), organisationunitcode = col_character(), organisationunitdescription = col_character()
  )

  # load input file
  df <- readr::read_csv(filename,
                        col_types = column_types) |>
    janitor::clean_names()

  templates <- input_templates

  # compare input file with templates and return match results
  match_results <- purrr::map_lgl(templates,
                                  ~ check_exact_match(names(df), .x))

  # filter match results to matching value
  matching_template <- tibble::enframe(match_results,
                                       name = "Template",
                                       value = "Match") |>
    filter(Match)

  # extract parse function name from match results
  parse_function <- matching_template |>
    dplyr::pull(Template)

  # call parse_function to engineer data features
  if (nrow(matching_template) > 0) {

    df <- clean_sisma_df(df) |>
      get(parse_function)() |>
      set_language(language = language)

    return(df)

  } else {

    # return error message when no template match is found
    cat(cli::col_red("\nPROCESSING ERROR ----"),
        cli::col_red("\nInput file not recognized. Please verify the path to your file and try again."),
        "\n")

  }

}


#' Importar e limpar ficheiro .csv do SISMA
#'
#' @description `clean_sisma_csv()` importa um ficheiro .csv bruto de SISMA e devolve um objecto transformado numa estrutura longa, cria uma coluna de período formatada e devolve um objeto de dados com 7 colunas (período, província, distrito, us, sisma_uid, indicador e valor)
#' `r lifecycle::badge("superseded")`
#' `clean_sisma_csv` foi substituída por `clean_sisma_df`
#'
#' @param file Caminho para um ficheiro .csv exportado do SISMA que contém variáveis de elementos no horizontal e período e unidades organizacionais no vertical
#'
#' @return `clean_sisma_csv()` devolve um objeto de dados limpo com 7 colunas (período, província, distrito, us, sisma_uid, indicador e valor)
#' @export
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'  df <- clean_sisma_csv()}


clean_sisma_csv <- function(file) {

  column_types <- cols(
    .default = col_double(),  # default to double
    periodid  = col_character(), periodname = col_character(), periodcode = col_character(),
    perioddescription = col_character(), orgunitlevel1 = col_character(), orgunitlevel2 = col_character(),
    orgunitlevel3 = col_character(), orgunitlevel4 = col_character(), organisationunitid = col_character(),
    organisationunitname = col_character(), organisationunitcode = col_character(), organisationunitdescription = col_character()
  )

  df <- readr::read_csv(file,
                        col_types = column_types) %>%

    dplyr::filter(!organisationunitid == "abg5UReivZX") %>% # for whatever reason this site creates issues

    janitor::clean_names() %>%

    dplyr::select(!c(periodname,
                     periodid,
                     perioddescription,
                     organisationunitcode,
                     organisationunitdescription,
                     orgunitlevel1,
                     organisationunitname)) %>%

    tidyr::pivot_longer(!c(periodcode,
                           orgunitlevel2,
                           orgunitlevel3,
                           orgunitlevel4,
                           organisationunitid),
                        names_to = "indicator",
                        values_to = "value",
                        values_transform = list(value = as.numeric)) %>%

    # dplyr::filter(!is.na(value)) %>%

    dplyr::mutate(periodcode = stringr::str_replace_all(periodcode,

                                                        c("Q1" = "03",
                                                          "Q2" = "06",
                                                          "Q3" = "09",
                                                          "Q4" = "12")),
                  periodcode = paste0(periodcode, "01"),
                  periodcode = lubridate::ymd(periodcode),
                  dplyr::across(c(orgunitlevel2, orgunitlevel3), ~ stringr::str_to_title(.))) %>%

    dplyr::select(period = periodcode,
                  snu = orgunitlevel2,
                  psnu = orgunitlevel3,
                  sitename = orgunitlevel4,
                  sisma_uid = organisationunitid,
                  indicator,
                  value)

  return(df)

}


#' Processar: Dados em formato .csv do SISMA
#'
#' @description `process_sisma_csv` é uma função que limpa e transforma as exportações tabulares do SISMA guardadas localmente no formato .csv
#' `r lifecycle::badge("superseded")`
#' `process_sisma_csv` foi substituído a favor de `process_sisma_export`
#'
#'   ## Tipos de ficheiros de entrada
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
#' * ATS SMI: ATS especifico a SMI (outros acomopanhantes na CPN, etc.)
#' * ATS Auto: ATS Autotestagem, Programa de HIV
#' * HIV TARV: Tratamento Antiretroviral (TARV), Programa de HIV
#' * HIV PREP: Profilaxia Pré-Exposição (PrEP), Programa de HIV
#' * HIV APSS: Apoio Psicossocial (APSS), Programa de HIV
#' * HIV ITS: Infecções de Transmissão Sexual (ITS), Programa de HIV
#' * HIV AJMHCMM: Adolescent Joven Mentor, Homen Campeao, Maes Mentora
#' * HIV DAH: Doenca Avancada do HIV
#'
#' @param file Caminho do ficheiro csv do sisma
#' @param type Tipo de exportação sisma csv (SMI-CPN, Resultado ATS, etc.)
#' @param language Lingua dos nomes das variáveis geradas (português ou inglês)
#'
#' @return Um quadro de dados arrumado dos resultados SISMA
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


#' Arrumar dados SISMA por tipo
#'
#' @description `parse_sisma_csv` função auxiliar que chama a função apropriada
#' com base no argumento definido pelo utilizador
#'
#'   ## Input File Types
#' * SMI-CPN: CPN, Saude Materna Infantil
#' * SMI-MAT: Maternidade, Saude Materna Infantil
#' * SMI-CCR: CCR, Saude Materna Infantil
#' * SMI-CPP: CCP, Saude Materna Infantil
#' * SMI-CCD: CCD, Saude Materna Infantil
#' * SMI-CCS: CCS, Saude Materna Infantil
#' * SMI-UG: UG, Saude Materna Infantil
#' * SMI-PF: PF, Saude Materna Infantil
#' * SMI-PAV: PAV, Saude Materna Infantil
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
#' * HIV ITS: Infecções de Transmissão Sexual (ITS), Programa de HIV
#' * HIV AJMHCMM: Adolescent Joven Mentor, Homen Campeao, Maes Mentora
#' * HIV DAH: Doenca Avancada do HIV
#'
#' @param data Caminho para o ficheiro de dados SISMA (guardado em formato .csv)
#' @param type Define o tipo de dados programáticos para processamento
#'
#' @return `parse_sisma_csv` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_csv()}

parse_sisma_csv <- function(data, type){

  switch(type,
         "SMI-CPN" = parse_sisma_smi_cpn(data), # SISMA search term "SMI-CPN"
         "SMI-MAT" = parse_sisma_smi_mat(data), # SISMA search term "SMI-MAT"
         "SMI-CCR" = parse_sisma_smi_ccr(data), # SISMA search term "SMI - CCR - "
         "SMI-CPP" = parse_sisma_smi_cpp(data), # SISMA search term "SMI-CPP"
         "SMI-PAV" = parse_sisma_smi_pav(data), # SISMA search term "SMI-PAV"
         "SMI-CCD" = parse_sisma_smi_ccd(data), # SISMA search term "SMI-CCD"
         "SMI-CCS" = parse_sisma_smi_ccs(data), # SISMA search term "SMI-CCS"
         "SMI-UG" = parse_sisma_smi_ug(data),   # SISMA search term "SMI-UG"
         "SMI-PF" = parse_sisma_smi_pf(data),   # SISMA search term "SMI-PF"
         "SMI-PF Int" = parse_sisma_smi_pf_int(data),
         "ATS Result" = parse_sisma_ats_results(data), # SISMA search term "ano" within Grupo de Elemento de Dados "ATS - Resumo Mensal (Nova)"
         "ATS History" = parse_sisma_ats_history(data), # SISMA search terms "Historial" & "chave" within Grupo de Elemento de Dados "ATS - Resumo Mensal (Nova)"
         "ATS CI" = parse_sisma_ats_index(data),  # SISMA search terms "diagonsticados" & "indice" within Grupo de Elemento de Dados "ATS - Resumo Mensal (Nova)"
         "ATS SAAJ" = parse_sisma_ats_saaj_cm(data), # SISMA search terms "MZ SAAJ - Testagem para HIV -" & "MZ C.MASC - Testados"
         "ATS CCSD" = parse_sisma_ats_ccsd(data),
         "ATS SMI" = parse_sisma_ats_smi(data),
         "ATS Auto" = parse_sisma_ats_auto(data), # SISMA search term "MZ HIV-AUTOTESTE"
         "HIV TARV" = parse_sisma_hiv_tarv(data), # SISMA search term "MZ HIV SIDA - "
         "HIV PREP" = parse_sisma_hiv_prep(data), # SISMA search term "MZ PREP"
         "HIV APSS" = parse_sisma_hiv_apss(data), # SISMA search term "MZ APSS PP"
         "HIV ITS" = parse_sisma_hiv_its(data), # SISMA search term "MZ ITS -"
         "HIV AJMHCMM" = parse_sisma_hiv_ajm_hc_mm(data), # SISMA search term "MZ AJMHCMM"
         "HIV DAH" = parse_sisma_hiv_dah(data), # SISMA search term "MZ DAH -"
  )

}


#' Arrumar: Tratamento Antiretroviral (TARV)
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
    tidyr::drop_na(tidyselect::any_of(c("indicator_new", "source", "value")))

  df_activos_prev <- df %>%
    dplyr::filter(indicator_new == "TX_ACTIVO") %>%
    dplyr::mutate(indicator_new = "TX_ACTIVO_PREV") %>%
    dplyr::mutate(period = period + months(1))


  df <- dplyr::bind_rows(df, df_activos_prev) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}


#' Arrumar: Adolescente joven mentor, homem campeão, e mães mentoras
#'
#' `parse_sisma_hiv_ajmhcmm` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_hiv_ajmhcmm` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_hiv_ajmhcmm()}

parse_sisma_hiv_ajmhcmm <- function(df) {

  df <- df %>%

    dplyr::left_join(data_sisma_hiv_ajmhcmm_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(age_coarse = dplyr::case_when(age == "15-24" ~ "15+",
                                                age == "25-29" ~ "15+",
                                                age == "30+" ~ "15+",
                                                age == "25+" ~ "15+",
                                                age == "<05" ~ "<15",
                                                age == "05-09" ~ "<15",
                                                age == "10-14" ~ "<15")) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}


#' Arrumar: Apoio psicossocial do HIV (APSS)
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


#' Arrumar: Auto-diagnóstico do HIV
#'
#' `parse_sisma_ats_auto` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @param df Objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`
#'
#' @return  `parse_sisma_ats_auto` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_ats_auto()}

parse_sisma_ats_auto <- function(df) {

  df <- df %>%

    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(data_sisma_ats_autoteste_map, by = "indicator") %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)


}


#' Arrumar: Testagem do HIV da CCS e CCR
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

  df <- df %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(data_sisma_ats_ccsd_map, by = "indicator") %>%
    dplyr::filter(!is.na(indicator_new)) %>%
    dplyr::mutate(
      source = "LdR SMI",
      indicator = "ATS_TST"
    )

  df_pos <- df %>%
    dplyr::filter(result_status == "Positivo") %>%
    dplyr::mutate(indicator = "ATS_TST_POS")


  df <- dplyr::bind_rows(df, df_pos) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}


#' Arrumar: Testagem do HIV caso índice
#'
#' `parse_sisma_ats_index` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_ats_index` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_ats_index()}

parse_sisma_ats_index <- function(df) {

  df <- df %>%

    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(data_sisma_ats_ci_map, by = "indicator") %>%
    dplyr::mutate(source = "LdR ATS")

  df_pos <- df %>%
    dplyr::filter(result_status == "Positivo") %>%
    dplyr::mutate(indicator = "ATS_CI_TST_POS")

  df <- dplyr::bind_rows(df, df_pos) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}


#' Arrumar: Testagem do HIV em populações-chave/vulnerável e historial
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

  df <- df %>%
    dplyr::left_join(data_sisma_ats_hist_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(source = "LdR ATS")

  df_pos <- df %>%
    dplyr::filter(result_status == "Positivo") %>%
    dplyr::mutate(indicator = "ATS_KP_POS")

  df <- dplyr::bind_rows(df, df_pos) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}


#' Arrumar: Testagem do HIV (Registo ATS)
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


#' Arrumar: Testagem do HIV SAAJ
#'
#' `parse_sisma_ats_saaj_cm` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_ats_saaj_cm` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_ats_saaj_cm()}

parse_sisma_ats_saaj_cm <- function(df) {

  df <- df %>%
    tidyr::pivot_wider(names_from = indicator, values_from = value) %>%

    dplyr::mutate(dplyr::across(tidyselect::starts_with("mz_saaj_"), ~ replace_na(.x, 0))) %>%

    dplyr::mutate(

      # adolescent testing
      mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_negativo_10_anos_feminino = mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_10_anos_feminino - mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_10_anos_feminino,
      mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_negativo_10_anos_masculino = mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_10_anos_masculino - mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_10_anos_masculino,

      mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_negativo_10_14_anos_feminino = mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_10_14_anos_feminino - mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_10_14_anos_feminino,
      mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_negativo_10_14_anos_masculino = mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_10_14_anos_masculino - mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_10_14_anos_masculino,

      mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_negativo_15_19_anos_feminino = mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_15_19_anos_feminino - mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_15_19_anos_feminino,
      mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_negativo_15_19_anos_masculino = mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_15_19_anos_masculino - mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_15_19_anos_masculino,

      mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_negativo_20_24_anos_feminino = mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_20_24_anos_feminino - mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_20_24_anos_feminino,
      mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_negativo_20_24_anos_masculino = mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_20_24_anos_masculino - mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_20_24_anos_masculino,

      mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_negativo_25_anos_feminino = mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_25_anos_feminino - mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_25_anos_feminino,
      mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_negativo_25_anos_masculino = mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_25_anos_masculino - mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_25_anos_masculino,


      # partner testing
      mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_negativo_10_anos_feminino = mz_saaj_testagem_para_hiv_numero_parceiros_testados_10_anos_feminino - mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_10_anos_feminino,
      mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_negativo_10_anos_masculino = mz_saaj_testagem_para_hiv_numero_parceiros_testados_10_anos_masculino - mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_10_anos_masculino,

      mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_negativo_10_14_anos_feminino = mz_saaj_testagem_para_hiv_numero_parceiros_testados_10_14_anos_feminino - mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_10_14_anos_feminino,
      mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_negativo_10_14_anos_masculino = mz_saaj_testagem_para_hiv_numero_parceiros_testados_10_14_anos_masculino - mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_10_14_anos_masculino,

      mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_negativo_15_19_anos_feminino = mz_saaj_testagem_para_hiv_numero_parceiros_testados_15_19_anos_feminino - mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_15_19_anos_feminino,
      mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_negativo_15_19_anos_masculino = mz_saaj_testagem_para_hiv_numero_parceiros_testados_15_19_anos_masculino - mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_15_19_anos_masculino,

      mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_negativo_20_24_anos_feminino = mz_saaj_testagem_para_hiv_numero_parceiros_testados_20_24_anos_feminino - mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_20_24_anos_feminino,
      mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_negativo_20_24_anos_masculino = mz_saaj_testagem_para_hiv_numero_parceiros_testados_20_24_anos_masculino - mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_20_24_anos_masculino,

      mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_negativo_25_anos_feminino = mz_saaj_testagem_para_hiv_numero_parceiros_testados_25_anos_feminino - mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_25_anos_feminino,
      mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_negativo_25_anos_masculino = mz_saaj_testagem_para_hiv_numero_parceiros_testados_25_anos_masculino - mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_25_anos_masculino) %>%


    tidyr::pivot_longer(cols = tidyselect::starts_with("mz_"), names_to = "indicator", values_to = "value") %>%

    dplyr::filter(!value == 0) %>%

    dplyr::mutate(

      age = dplyr::case_when(stringr::str_detect(indicator, "_10_an")    ~ "<10", # saaj ages
                             stringr::str_detect(indicator, "10_14")     ~ "10-14",
                             stringr::str_detect(indicator, "15_19")     ~ "15-19",
                             stringr::str_detect(indicator, "20_24")     ~ "20-24",
                             stringr::str_detect(indicator, "_25_an")    ~ "25+",

                             stringr::str_detect(indicator, "15_19")     ~ "15-19", # cm ages
                             stringr::str_detect(indicator, "20_24")     ~ "20-24",
                             stringr::str_detect(indicator, "25_29")     ~ "25-29",
                             stringr::str_detect(indicator, "30_34")     ~ "30-34",
                             stringr::str_detect(indicator, "35_39")     ~ "35-39",
                             stringr::str_detect(indicator, "40_49")     ~ "40-49",
                             stringr::str_detect(indicator, "_50_an")    ~ "50+"),

      age_coarse = dplyr::case_when(age %in% c("<10", "10-14") ~ "<15",
                                    TRUE ~ "15+"),

      sex = dplyr::case_when(stringr::str_detect(indicator, "feminino")   ~ "Feminino",
                             stringr::str_detect(indicator, "masculino")  ~ "Masculino",
                             stringr::str_detect(indicator, "mz_c_masc_") ~ "Masculino"),

      disaggregate = dplyr::case_when(stringr::str_detect(indicator, "mz_saaj_")    ~ "SAAJ",
                                      stringr::str_detect(indicator, "mz_c_masc_")  ~ "CM"),

      result_status = dplyr::case_when(stringr::str_detect(indicator, "positivo")   ~ "Positivo",
                                       stringr::str_detect(indicator, "negativo")   ~ "Negativo",
                                       stringr::str_detect(indicator, "indetermin") ~ "Indet."),

      source = dplyr::case_when(stringr::str_detect(indicator, "mz_saaj_")      ~ "LdR SAAJ",
                                stringr::str_detect(indicator, "mz_c_masc_")    ~ "LdR CM"),

      sub_group = dplyr::case_when(stringr::str_detect(indicator, "arceiro")   ~ "Parceiro",
                                   TRUE ~ NA_character_),

      indicator = "ATS_TST") %>%

    dplyr::filter(!is.na(result_status))


  df_pos <- df %>%
    dplyr::filter(result_status == "Positivo") %>%
    dplyr::mutate(indicator = "ATS_TST_POS")


  df <- dplyr::bind_rows(df, df_pos) %>%
    dplyr::mutate(indicator_new = indicator) %>%
    add_missing_vars() %>%
    seq_vars()


  return(df)

}


#' Arrumar: Testagem do HIV SMI
#'
#' `parse_sisma_ats_smi` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_ats_smi` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_ats_smi()}

parse_sisma_ats_smi <- function(df) {

  df <- df %>%

    tidyr::pivot_wider(names_from = indicator, values_from = value) %>%

    # make sure the columns that need values for subsequent calculations are not in na
    tidyr::replace_na(list(smi_cpp_puerperas_testadas_na_consulta_pos_parto = 0,
                           smi_cpp_puerperas_hiv_das_puerperas_testadas_na_consulta_pos_parto = 0,
                           smi_cpn_mulheres_gravidas_testadas_hiv_positivas_durante_a_cpn = 0,
                           smi_mat_total_testadas_para_hiv_na_maternidade = 0,
                           smi_mat_hiv_identificadas_na_maternidade = 0,
                           smi_pf_hiv_utentes_testadas_na_consulta_de_saude_reprodutiva = 0,
                           smi_pf_utentes_hiv_das_mulheres_testadas_na_csr_pf = 0, # PERHAPS NOT COUNTING IN POSITIVE IN THE CODE BELOW
                           smi_ug_total_testadas_para_hiv_nas_urgencias_de_ginecologia = 0,
                           smi_ug_hiv_identificadas_na_ginecologia = 0)) %>%

    # calculate negative test values based on total and positive
    dplyr::mutate(smi_cpp_hiv_negative = smi_cpp_puerperas_testadas_na_consulta_pos_parto - smi_cpp_puerperas_hiv_das_puerperas_testadas_na_consulta_pos_parto,
                  smi_mat_hiv_negative = smi_mat_total_testadas_para_hiv_na_maternidade - smi_mat_hiv_identificadas_na_maternidade,
                  smi_pf_hiv_negative = smi_pf_hiv_utentes_testadas_na_consulta_de_saude_reprodutiva - smi_pf_utentes_hiv_das_mulheres_testadas_na_csr_pf,
                  smi_ug_hiv_negative = smi_ug_total_testadas_para_hiv_nas_urgencias_de_ginecologia - smi_ug_hiv_identificadas_na_ginecologia) %>%

    # remove total testing indicators
    dplyr::select(-c(smi_cpp_puerperas_testadas_na_consulta_pos_parto,
                     smi_mat_total_testadas_para_hiv_na_maternidade,
                     smi_pf_hiv_utentes_testadas_na_consulta_de_saude_reprodutiva,
                     smi_ug_total_testadas_para_hiv_nas_urgencias_de_ginecologia)) %>%

    tidyr::pivot_longer(cols = tidyselect::where(is.numeric),
                        names_to = "indicator",
                        values_to = "value") %>%

    dplyr::mutate(
      disaggregate = dplyr::case_when(stringr::str_detect(indicator, "smi_cpn") ~ "SMI-CPN",
                                      stringr::str_detect(indicator, "smi_mat") ~ "SMI-MAT",
                                      stringr::str_detect(indicator, "smi_ccr") ~ "SMI-CCR",
                                      stringr::str_detect(indicator, "smi_cpp") ~ "SMI-CPP",
                                      stringr::str_detect(indicator, "smi_pf") ~ "SMI-PF",
                                      stringr::str_detect(indicator, "smi_ug") ~ "SMI-UG"),

      result_status = dplyr::case_when(stringr::str_detect(indicator, "negativ") ~ "Negativo",
                                       stringr::str_detect(indicator, "positi") ~ "Positivo",
                                       stringr::str_detect(indicator, "HIV+") ~ "Positivo",
                                       stringr::str_detect(indicator, "rperas_hiv_das") ~ "Positivo",
                                       stringr::str_detect(indicator, "utentes_hiv_das") ~ "Positivo",
                                       stringr::str_detect(indicator, "identificad") ~ "Positivo"),

      sub_group = dplyr::case_when(stringr::str_detect(indicator, "parceir") ~ "Parceiro",
                                   stringr::str_detect(indicator, "expost") ~ "Infante"),

      age_coarse = dplyr::case_when(stringr::str_detect(indicator, "smi_ccr") ~ "<15"),

      age_coarse = tidyr::replace_na(age_coarse, "15+"),

      sex = dplyr::case_when(stringr::str_detect(indicator, "parceir") ~ "Masculino",
                             stringr::str_detect(disaggregate, ", smi_ccr") ~ "Desconh."),

      sex = tidyr::replace_na(sex, "Feminino"),

      indicator = "ATS_TST",

      source = "LdR SMI"
    )

  df_pos <- df %>%
    dplyr::filter(result_status == "Positivo") %>%
    dplyr::mutate(indicator = "ATS_TST_POS")

  df <- dplyr::bind_rows(df, df_pos) %>%
    dplyr::mutate(indicator_new = indicator) %>%
    add_missing_vars() %>%
    seq_vars()


  return(df)

}


#' Arrumar: Doença avançada do HIV
#'
#' `parse_sisma_hiv_dah` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_hiv_dah` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_hiv_dah()}

parse_sisma_hiv_dah <- function(df) {

  df <- df %>%

    dplyr::left_join(data_sisma_hiv_dah_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}


#' Arrumar: Infecções de transmissão sexual (ITS)
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


#' Arrumar: Profilaxia Pré-Exposição (PrEP)
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
    dplyr::mutate(source = "LdR PrEP",
                  age_coarse = dplyr::case_when(age == "15-19" ~ "15+", # correct
                                                age == "20-24" ~ "15+",
                                                age == "25-29" ~ "15+",
                                                age == "25+" ~ "15+")
    ) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}


#' Arrumar: Consulta da criança doente (CCD)
#'
#' `parse_sisma_smi_cpp` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_smi_ccd` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_smi_cpp()}


parse_sisma_smi_ccd <- function(df) {

  df <- df %>%

    dplyr::left_join(data_sisma_smi_ccd_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}


#' Arrumar: Consulta da criança de risco (CCR)
#'
#' `parse_sisma_smi_ccr` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_smi_ccr` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_smi_ccr()}

parse_sisma_smi_ccr <- function(df) {

  df <- df %>%

    dplyr::left_join(data_sisma_smi_ccr_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}


#' Arrumar: Consulta da criança sadia (CCS)
#'
#' `parse_sisma_smi_ccs` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_smi_ccs` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_smi_ccs()}


parse_sisma_smi_ccs <- function(df) {

  df <- df %>%

    dplyr::left_join(data_sisma_smi_ccs_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}


#' Arrumar: Consulta pré-natal (CPN)
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


#' Arrumar: Consulta pós-parto (CPP)
#'
#' `parse_sisma_smi_cpp` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_smi_cpp` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_smi_cpp()}


parse_sisma_smi_cpp <- function(df) {

  df <- df %>%

    dplyr::left_join(data_sisma_smi_cpp_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}


#' Arrumar: Maternidade (Mat)
#'
#' `parse_sisma_smi_mat` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_smi_mat` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_smi_mat()}

parse_sisma_smi_mat <- function(df) {

  df <- df %>%

    dplyr::left_join(data_sisma_smi_mat_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}


#' Arrumar: Programa alargado de vacinação (PAV)
#'
#' `parse_sisma_smi_pav` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_smi_pav` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_smi_pav()}


parse_sisma_smi_pav <- function(df) {

  df <- df %>%

    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(data_sisma_smi_pav_map, by = "indicator") %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}


#' Arrumar: Planeamento familiar (PF)
#'
#' `parse_sisma_smi_pf` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_smi_pf` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_smi_pf()}


parse_sisma_smi_pf <- function(df) {

  df <- df %>%

    dplyr::left_join(data_sisma_smi_pf_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}


#' Arrumar: Planeamento familiar (PF) integrado
#'
#' `parse_sisma_smi_pf_int` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_smi_pf_int` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_smi_pf_int()}


parse_sisma_smi_pf_int <- function(df) {

  df <- df %>%

    dplyr::left_join(data_sisma_smi_pf_int_saaj_ape_map, by = "indicator") |>
    dplyr::filter(!is.na(value)) |>
    add_missing_vars() %>%
    seq_vars()

  return(df)

}


#' Arrumar: Urgência de ginecológica (UG)
#'
#' `parse_sisma_smi_ug` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_smi_ug` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_smi_ug()}


parse_sisma_smi_ug <- function(df) {

  df <- df %>%

    dplyr::left_join(data_sisma_smi_ug_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}


#' Arrumar: Boletim epidemiológico semanal (BES)
#'
#' `parse_sisma_bes` Uma função auxiliar que  gera um quadro de dados arrumado
#' a partir  de um objecto relatório padrão introduzido por `process_sisma_export`
#' ou `process_sisma_csv`. Variáveis úteis que são criados por esta função incluem
#' sexo, idade, tipo de subgrupo, e outras desagregações presentes na fonte.
#'
#' @inheritParams parse_sisma_ats_auto
#'
#' @return `parse_sisma_bes` devolve um quadro de dados arrumado com 16
#' colunas de metadados da us, desagregações do indicador, e valores
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_bes()}


parse_sisma_bes <- function(df) {

  df <- df %>%

    dplyr::left_join(data_sisma_bes_map, by = "indicator") %>%
    dplyr::filter(!is.na(value)) %>%
    add_missing_vars() %>%
    seq_vars()

  return(df)

}

