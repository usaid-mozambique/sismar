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
#' * ATS History: ATS Historial, Programa de HIV
#' * ATS CI: ATS Caso Indice, Programa de HIV
#'
#' @param file Path of sisma csv input
#' @param type Type of sisma csv export (CPN, ATS-R, etc.)
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
    parse_sisma_csv(type)

  if(language == "portuguese"){

    df <- df %>% dplyr::rename(periodo = period,
                               provincia = snu,
                               distrito = psnu,
                               us = sitename,
                               indicador = indicator,
                               idade = age,
                               sexo = sex,
                               disagregacao = disaggregate,
                               valor = value)

    return(df)

  }

  if(language == "english"){

    return(df)

  }

  return(df)

}
