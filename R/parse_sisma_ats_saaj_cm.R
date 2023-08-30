#' A specific helper function for parsing a cleaned CSV export from SISMA
#'
#' @param file Dataframe cleaned via reshape_sisma
#'
#' @return A tidy format of SISMA dataframe
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_ats_saaj_cm()}

parse_sisma_ats_saaj_cm <- function(file) {

  df_all <- file %>%
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

    dplyr::mutate(period_cohort = NA_character_,
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

                  disaggregate_sub = NA_character_,

                  source = dplyr::case_when(stringr::str_detect(indicator, "mz_saaj_")      ~ "LdR SAAJ",
                                            stringr::str_detect(indicator, "mz_c_masc_")    ~ "LdR CM"),

                  sub_group = dplyr::case_when(stringr::str_detect(indicator, "arceiro")   ~ "Parceiro",
                                               TRUE ~ NA_character_),

                  indicator = "ATS_TST") %>%

    dplyr::filter(!is.na(result_status))


  df_pos <- df_all %>%
    dplyr::filter(result_status == "Positivo") %>%
    dplyr::mutate(indicator = dplyr::case_when(indicator == "ATS_TST" ~ "ATS_TST_POS"))


  df_parse <- dplyr::bind_rows(df_all, df_pos) %>%
    dplyr::select(sisma_uid,
                  snu,
                  psnu,
                  sitename,
                  period,
                  period_cohort,
                  indicator,
                  source,
                  disaggregate,
                  disaggregate_sub,
                  sub_group,
                  sex,
                  age_coarse,
                  age,
                  result_status,
                  value)


  return(df_parse)

}
