#' Processar: Carga Viral (CV) da DISA OpenLDR
#'
#' @param file Caminho do ficheiro CV .xlsx
#' @param month Mês dos dados CV (formatado “AAAA-MM-DD”)
#'
#' @return Um quadro de dados arrumado dos resultados do CV
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- process_disa_cv()}


process_disa_cv <- function(file, month) {

  # import
  df_age <- readxl::read_excel(file,
                               sheet = "Age & Sex",
                               col_types = c("text",
                                             "text", "text", "text", "text", "text",
                                             "text", "text", "text", "numeric",
                                             "numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric",
                                             "numeric"),
                               skip = 4)


  df_pw <- readxl::read_excel(file,
                              sheet = "S. Viral (M. Gravidas)",
                              col_types = c("text",
                                            "text", "text", "text", "text", "text",
                                            "text", "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric"),
                              skip = 4)


  df_lw <- readxl::read_excel(file,
                              sheet = "S. Viral (M. Lactantes)",
                              col_types = c("text",
                                            "text", "text", "text", "text", "text",
                                            "text", "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric"),
                              skip = 4)


  df_tat <- readxl::read_excel(file,
                               sheet = "TRL - AVG",
                               col_types = c("text",
                                             "text", "text", "text", "text", "text",
                                             "text", "numeric", "numeric", "numeric",
                                             "numeric", "numeric"),
                               skip = 4)

  # tidy
  df <- dplyr::bind_rows(df_age, df_pw, df_lw, df_tat) %>%
    dplyr::select(!c(datim_uid, sitetype, contains("total"))) %>%
    tidyr::pivot_longer(cols = tidyselect::starts_with("vl"),
                        names_to = c("indicador", "sub_grupo", "disagregacao", "resultado_estado", "disagregacao_sub"),
                        names_sep = "_",
                        values_to = "valor") %>%
    dplyr::filter(valor > 0)


  # feature engineering
  df_1 <- df %>%
    dplyr::mutate(periodo = as.Date(month, "%Y-%m-%d"),

                  sexo = dplyr::case_when(indicador == "vl" & sex == "F" ~ "Feminino",
                                          indicador == "vl" & sex == "M" ~ "Masculino",
                                          indicador == "vl" & sex == "UNKNOWN" ~ "Desconh.",
                                          stringr::str_detect(sex, "specif") ~ "Desconh."),

                  idade = dplyr::case_when(age == "<1" ~ "<01",
                                           age == "NS" ~ "Desconh.",
                                           stringr::str_detect(age, "specif") ~ "Desconh.",
                                           TRUE ~ age),

                  sub_grupo = dplyr::case_when(sub_grupo == "age" ~ "Idade",
                                               sub_grupo == "pw" ~ "MG",
                                               sub_grupo == "lw" ~ "ML"),

                  disagregacao = dplyr::case_when(disagregacao == "routine" ~ "Rotina",
                                                  disagregacao == "failure" ~ "Falencia Terapeutica",
                                                  disagregacao == "repeat" ~ "Pos-Amamentacao",
                                                  disagregacao == "unspecified" ~ "Nao Especificado",
                                                  stringr::str_detect(disagregacao_sub, "s1.") ~ "P1: Recolha a Recepcao",
                                                  stringr::str_detect(disagregacao_sub, "s2.") ~ "P2: Recepcao a Registo",
                                                  stringr::str_detect(disagregacao_sub, "s3.") ~ "P3: Registo a Analise",
                                                  stringr::str_detect(disagregacao_sub, "s4.") ~ "P4: Analise a Validacao",
                                                  disagregacao == "" ~ NA_character_),

                  resultado_estado = dplyr::case_when(resultado_estado == "suppress" ~ "Suprimida",
                                                      resultado_estado == "unsuppress" ~ "Nao Suprimida"),

                  disagregacao_sub = disa_uid,

                  VL = dplyr::case_when(resultado_estado == "Suprimida" | resultado_estado == "Nao Suprimida" & indicador == "vl" ~ valor),

                  VLS = dplyr::case_when(resultado_estado == "Suprimida" & indicador == "vl" ~ valor,
                                         is.na(resultado_estado) ~ 0),

                  TAT = dplyr::case_when(indicador == "vltat" ~ valor,
                                         TRUE ~ NA_integer_)) %>%

    dplyr::select(!c(indicador, valor, sex, age, site_nid, snu, psnu, sitename)) |>
    dplyr::group_by(dplyr::across(disa_uid:idade), .drop = TRUE) %>%
    dplyr::summarise(dplyr::across(tidyselect::where(is.double), ~ sum(.x, na.rm = TRUE))) |>
    dplyr::ungroup() |>
    dplyr::mutate(fonte = "DISA",
                  periodo_coorte = NA_character_,
                  # resultado_estado = NA_character_,
                  idade_agrupada = dplyr::case_when(idade == "Unknown Age" ~ "Desconh.",
                                                    idade %in% c("<01", "01-04", "05-09", "10-14") ~ "<15",
                                                    idade %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+") ~ "15+")
    ) |>
    tidyr::pivot_longer(c(VL, VLS, TAT), names_to = "indicador", values_to = "valor") |>
    dplyr::filter(valor > 0)


  df_2 <- df_1 |>
    dplyr::left_join(data_disa_uid_map, dplyr::join_by(disa_uid)) |>
    dplyr::left_join(data_sisma_sitelist, dplyr::join_by(sisma_uid)) |>
    dplyr::select(!disa_uid) |>
    dplyr::select(sisma_uid,
                  provincia,
                  distrito,
                  us,
                  periodo,
                  periodo_coorte,
                  indicador,
                  fonte,
                  disagregacao,
                  disagregacao_sub,
                  sub_grupo,
                  sexo,
                  idade,
                  idade_agrupada,
                  resultado_estado,
                  valor)


    return(df_2)

}
