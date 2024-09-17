#' Helper function to check for input matches against template
#'
#' @param df_vars dataframe variables
#' @param template_vars reference input variables
#'
#' @return matrix of match vs. no match
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- check_exact_match()}

check_exact_match <- function(df_vars, template_vars) {

  identical(sort(df_vars), sort(template_vars))

}
