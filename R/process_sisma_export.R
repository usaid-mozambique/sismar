#' Create tidy dataframes for standard SISMA .csv exports
#'
#' @param filename Path of sisma csv input
#' @param language Language of output variable names (portuguese or english)
#'
#' @return A tidy dataframe of SISMA program results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- process_sisma_export()}


process_sisma_export <- function(filename, language = "portuguese") {

  # load input file
  df <- readr::read_csv(filename) |>
    janitor::clean_names()

  templates <- input_templates

  # compare input file with templates and return match results
  match_results <- purrr::map_lgl(templates,
                                  ~ check_exact_match(names(df), .x))

  # filter match results to matching value
  matching_template <- tibble::enframe(match_results,
                                       name = "Template",
                                       value = "Match") |>
    filter(Match)

  # extract parse function name from match results
  parse_function <- matching_template |>
    dplyr::pull(Template)

  # call parse_function to engineer data features
  if (nrow(matching_template) > 0) {

    df <- clean_sisma_df(df) |>
      get(parse_function)() |>
      set_language(language = language)

    return(df)

  } else {

    # return error message when no template match is found
    cat(col_red("\nPROCESSING ERROR ----"),
        col_red("\nInput file not recognized. Please verify the path to your file and try again."),
        "\n")

  }

}
