#' Helper function for recoding DISA dataframe
#'
#' @param df Semi-processed dataframe passed in via process_disa_dpi
#'
#' @return Recoded DISA dataframe
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- clean_disa_dpi()}

clean_disa_dpi <- function(df){

  df <- df %>%

    dplyr::select(!c(site_nid, datim_uid, sitetype)) %>%

    dplyr::mutate(

      indicador = dplyr::case_match(indicador,
                                    "disa.dpi" ~ "DISA_DPI",
                                    "disa.dpi.cascata" ~ "DISA_DPI_CASCATA",
                                    .default = indicador),

      idade = dplyr::case_match(idade,
                                "u2m" ~ "<2m",
                                "o2m" ~ "2+m",
                                "u9m" ~ "<9m",
                                "9.17m" ~ "9-17m",
                                .default = idade),

      disagregacao = dplyr::case_match(disagregacao,
                                       "conheita.1a" ~ "Colheita 1a",
                                       "confirm.1a" ~ "Confirmacao",
                                       "repeticao.rej" ~ "Repiticao Rej",
                                       "conheita.seg" ~ "Colheita Seg",
                                       "nao.reportado" ~ "Nao Reportado",
                                       .default = disagregacao),

      disagregacao_sub = dplyr::case_match(disagregacao_sub,
                                           "u2m" ~ "<2m",
                                           "o2m" ~ "2+m",
                                           .default = disagregacao_sub),

      resultado_estado = dplyr::case_match(resultado_estado,
                                           "positivo" ~ "Positivo",
                                           "positive" ~ "Positivo",
                                           "negativo" ~ "Negativo",
                                           "negative" ~ "Negativo",
                                           "indet" ~ "Indet.",
                                           .default = resultado_estado),

      fonte = dplyr::case_match(fonte,
                                "convencional" ~ "Convencional",
                                "mpima" ~ "MPIMA",
                                .default = fonte),

      sexo = dplyr::case_when(
        sexo == "F" ~ "Feminino",
        sexo == "M" ~ "Masculino",
        stringr::str_detect(sexo, "speci") ~ "Desconh.",
        TRUE ~ sexo)) %>%

    dplyr::relocate(indicador, .after = sitename) %>%
    dplyr::relocate(periodo, .before = everything())

}
