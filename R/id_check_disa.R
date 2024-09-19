#' Check for DISA UID's without corresponding SISMA UID's and for SISMA UID's with multiple DISA UID's
#'
#' @param df Dataframe already processed by either `process_disa_cv` or `process_disa_dpi`
#' @param period_window Number of months returned in the output (counted from the most current month in the input dataframe)
#'
#' @return Dataframe with multiple DISA UID's mapping to a SISMA UID or DISA UID's without a corresponding SISMA UID
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
