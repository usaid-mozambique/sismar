#' Tidy DISA DPI .xlsx files (extracted from OpenLDR)
#'
#' @param file Path of DISA DPI .xlsx file
#' @param period Month of DISA DPI extract (passed as "%Y-%m-%d")
#' @param type Type of DISA data file to tidy ("old" or "new")
#'
#' @return A tidy dataframe of DISA DPI results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- process_disa_dpi()}


process_disa_dpi <- function(file, period, type = "new") {

  if (type == "new") {

    df_dpi <- readxl::read_excel(file,
                                 sheet = "Provincia - Genero",
                                 col_types = c("text", "text", "text", "text",
                                               "text", "text", "text", "text",
                                               rep("numeric", 50)),
                                 skip = 8) %>%

      tidyr::pivot_longer(cols = starts_with("disa.dpi"),
                          names_to = c("indicador", "fonte", "disagregacao", "idade", "disagregacao_sub", "resultado_estado"),
                          names_sep = "_",
                          values_to = "valor",
                          values_transform = list(valor = as.numeric)) %>%

      dplyr::mutate(periodo = period) %>%

      clean_disa_dpi()

  } else if (type == "old") {

    df_dpi <- readxl::read_excel(file,
                                 col_types = c("text", "text", "text",
                                               "text", "text", "text", "text", "text",
                                               "numeric", "numeric", "text", "text",
                                               "text", "text", "text"),
                                 skip = 5) %>%

      tidyr::pivot_longer(cols = starts_with("dpi_"),
                          names_to = c("indicador", "disagregacao", "resultado_estado"),
                          names_sep = "_",
                          values_to = "valor",
                          values_transform = list(valor = as.numeric)) %>%

      dplyr::rename(sexo = sex) %>%

      dplyr::mutate(periodo = month,
                    fonte = "DISA",
                    indicador = "DPI",
                    idade = NA_character_,
                    disagregacao_sub = NA_character_) %>%

      dplyr::filter(!resultado_estado == "total",
                    valor > 0) %>%

      clean_disa_dpi()

  }

  # create dpi pos dataframe
  df_dpi_pos <- df_dpi %>%
    dplyr::filter(resultado_estado == "Positivo") %>%
    dplyr::mutate(indicador = "DISA_DPI_POS")


  df_dpi_tat <- readxl::read_excel(file,
                                   sheet = "TRL-AVG",
                                   col_types = c("text",
                                                 "text", "text", "text", "text", "text",
                                                 "text", "numeric", "numeric", "numeric",
                                                 "numeric", "numeric", "numeric"),
                                   skip = 4) %>%

    # pivot tat variables
    tidyr::pivot_longer(cols = starts_with("dpi"),
                        names_to = c("indicador", "disagregacao", "resultado_estado", "remove", "tat_step"),
                        names_sep = "_",
                        values_to = "valor",
                        values_transform = list(valor = as.numeric)) %>%

    dplyr::select(!c(site_nid,
                     datim_uid,
                     sitetype,
                     remove)) %>%

    dplyr::mutate(periodo = period) %>%

    # Feature engineering
    dplyr::mutate(indicador = "DISA_DPI_TAT",
                  sexo = NA_character_,
                  disagregacao = dplyr::case_when(
                    stringr::str_detect(tat_step, "s1.") ~ "P1: Colheita a Disa-Link",
                    stringr::str_detect(tat_step, "s2.") ~ "P2: Disa-Link a Laboratorio",
                    stringr::str_detect(tat_step, "s3.") ~ "P3: Laboratorio a Registo",
                    stringr::str_detect(tat_step, "s4.") ~ "P4: Registo a Analise",
                    stringr::str_detect(tat_step, "s5.") ~ "P5: Analise a Validacao",
                    TRUE ~ tat_step),
                  disagregacao_sub = disa_uid) %>%

    dplyr::filter(!tat_step == "total")


  df <- dplyr::bind_rows(df_dpi, df_dpi_pos, df_dpi_tat) %>%
    dplyr::select(!c(snu, psnu, sitename, tat_step)) %>%
    dplyr::mutate(
      periodo_coorte = NA_character_,
      fonte = "DISA",
      idade_agrupada = NA_character_
    ) %>%
    dplyr::left_join(data_disa_uid_map, dplyr::join_by(disa_uid)) |>
    dplyr::left_join(data_sisma_sitelist, dplyr::join_by(sisma_uid)) |>
    dplyr::rename(sub_grupo = disa_uid) %>%
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

  return(df)

}
