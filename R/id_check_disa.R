#' Detetar casos problemáticos de mapeamento de UID DISA para investigação
#'
#' @param df Dataframe já processado por `process_disa_cv` ou `process_disa_dpi`
#' @param period_window Número de meses analisados do dataframe providenciado (contado a partir do mês mais recente)
#'
#' @return Dataframe com 1) UID DISA sem UID SISMA correspondentes; e 2) UID SISMA com múltiplos UID DISA
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- id_check_disa()}

id_check_disa <- function(df, period_window = 6) {

  df <- df |>

    dplyr::filter(periodo >= max(periodo) - months(period_window)) |>
    dplyr::group_by(sisma_uid) |>
    dplyr::filter(dplyr::n_distinct(disagregacao_sub) > 1) |>
    dplyr::distinct(sisma_uid, disagregacao_sub, periodo, provincia, distrito, us) |>
    dplyr::arrange(sisma_uid)

  return(df)

}
