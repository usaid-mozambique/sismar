#' Reordenar variáveis padrão
#'
#' @param df Objecto quadro de dados contendo todos variáveis padrão
#'
#' @return devolve um quadro de dados arrumado com 16
#' variáveis reordenados
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- seq_vars()}

seq_vars <- function(df) {

  df <- df |>
    dplyr::select(sisma_uid,
                  snu,
                  psnu,
                  sitename,
                  period,
                  period_cohort,
                  indicator = indicator_new,
                  source,
                  disaggregate,
                  disaggregate_sub,
                  sub_group,
                  sex,
                  age_coarse,
                  age,
                  result_status,
                  value)

  return(df)

}
