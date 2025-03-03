#' Função auxiliar que adiciona variáveis em falta nos quadros arrumados pelas funções `parse`
#'
#' @param df Objecto quadro de dados processado pela família de funções `parse`
#'
#' @return Objecto quadro de dados contendo todos variáveis padrão
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- add_missing_vars()}

add_missing_vars <- function(df) {

  # Required variables
  required_cols <- c("period_cohort",
                     "age",
                     "age_coarse",
                     "sex",
                     "sub_group",
                     "result_status",
                     "disaggregate",
                     "disaggregate_sub"
  )

  # Identify missing variables
  missing_cols <- dplyr::setdiff(required_cols, names(df))

  # Define NA values with explicit types
  add_cols <- list(
    period_cohort = as.Date(NA),
    age = NA_character_,
    age_coarse = NA_character_,
    sex = NA_character_,
    result_status = NA_character_,
    disaggregate = NA_character_,
    disaggregate_sub = NA_character_,
    sub_group = NA_character_
  )

  # Ensure missing variables exist with the correct data types
  df <- df |>
    dplyr::bind_cols(tibble::tibble(!!!setNames(lapply(missing_cols, function(col) add_cols[[col]]), missing_cols)))

  return(df)

}
