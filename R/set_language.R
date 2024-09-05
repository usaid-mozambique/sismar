#' Set language of output variables
#'
#' @param df input dataframe
#' @param language variable output language
#'
#' @return dataframe with desired variable language
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- set_language()}

set_language <- function(df, language = "portuguese") {

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

}
