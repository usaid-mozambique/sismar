#' Função auxiliar para mapear ficheiro de insumo ao modelo de processamento
#'
#' @param df_vars variáveis de dataframe
#' @param template_vars variáveis dos modelos de referência
#'
#' @return matriz de correspondência
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- check_exact_match()}

check_exact_match <- function(df_vars, template_vars) {

  identical(sort(df_vars), sort(template_vars))

}
