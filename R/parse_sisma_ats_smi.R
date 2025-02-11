#' Arrumar quadros de dados para os serviços de testagem do HIV dos Serviços
#' Saúde Materno-Infantil (SMI)
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

  df_parse <- dplyr::bind_rows(df, df_pos) %>%
    df <- dplyr::bind_rows(df, df_pos) %>%
    add_missing_vars() %>%
    seq_vars()


  return(df)

}
