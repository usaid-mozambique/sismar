#' Tidy DISA DPI .xlsx files (extracted from OpenLDR)
#'
#' @param file Path of DISA DPI .xlsx file
#' @param month Month of DISA DPI extract (passed as "%Y-%m-%d")
#'
#' @return A tidy dataframe of DISA DPI results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- process_disa_dpi()}


process_disa_dpi <- function(file, month) {

  # tidy aphl disa dpi dataset
  dpi <- readxl::read_excel(file,
                            col_types = c("text", "text", "text",
                                          "text", "text", "text", "text", "text",
                                          "numeric", "numeric", "text", "text",
                                          "text", "text", "text"),
                            skip = 5) %>%

    dplyr::select(!c(site_nid,
                     datim_uid,
                     sitetype)) %>%

    # pivot dataframe variables
    tidyr::pivot_longer(cols = starts_with("dpi_"),
                        names_to = c("indicador", "disagregacao", "resultado_estado"),
                        names_sep = "_",
                        values_to = "valor",
                        values_transform = list(valor = as.numeric)) %>%

    dplyr::filter(!resultado_estado == "total",
                  valor > 0) %>%

    # feature engineering
    dplyr::mutate(periodo = month,
                  indicador = "DISA_DPI",
                  resultado_estado = dplyr::case_when(resultado_estado == "positive" ~ "Positivo",
                                                      resultado_estado == "negative" ~ "Negativo",
                                                      resultado_estado == "indet" ~ "Indet.",
                                                      TRUE ~ resultado_estado),
                  disagregacao = dplyr::case_when(disagregacao == "conventional" ~ "Convencional",
                                                  disagregacao == "mpima" ~ "MPIMA",
                                                  TRUE ~ disagregacao),
                  disagregacao_sub = disa_uid,
                  sexo = dplyr::case_when(sex == "F" ~ "Feminino",
                                          sex == "M" ~ "Masculino",
                                          stringr::str_detect(sex, "speci") ~ "Desconh.",
                                          TRUE ~ sex),
                  tat_step = NA_character_) %>%

    dplyr::relocate(indicador, .after = sitename) %>%
    dplyr::relocate(periodo, .before = everything())

  # create dpi pos dataframe
  dpi_pos <- dpi %>%
    dplyr::filter(resultado_estado == "Positivo") %>%
    dplyr::mutate(indicador = "DISA_DPI_POS")

  # read tat dataset
  df_tat <- readxl::read_excel(file,
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

    # Feature engineering
    dplyr::mutate(periodo = month,
                  indicador = "TAT",
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

  # bind dataframes
  df <- dplyr::bind_rows(dpi, dpi_pos, df_tat) %>%
    dplyr::select(!c(snu, psnu, sitename, sex, tat_step)) |>
    dplyr::mutate(
      periodo_coorte = NA_character_,
      fonte = "DISA",
      sub_grupo = NA_character_,
      idade = NA_character_,
      idade_agrupada = NA_character_
    )

  # fetch site maps for recoding site metadata
  map_disa <- mozR::pull_sitemap(sheetname = "map_disa") |> dplyr::select(!c("note", "ajuda", "site_nid", "datim_uid"))
  map_ou <- mozR::pull_sitemap(sheetname = "list_sisma") |> dplyr::select(c("sisma_uid", "provincia", "distrito", "us"))

  df_1 <- df |>
    dplyr::left_join(map_disa, dplyr::join_by(disa_uid)) |>
    dplyr::left_join(map_ou, dplyr::join_by(sisma_uid)) |>
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

  return(df_1)

}
