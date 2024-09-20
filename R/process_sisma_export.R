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

  # set column types for input file
  column_types <- cols(
    .default = col_double(),
    periodid  = col_character(), periodname = col_character(), periodcode = col_character(),
    perioddescription = col_character(), orgunitlevel1 = col_character(), orgunitlevel2 = col_character(),
    orgunitlevel3 = col_character(), orgunitlevel4 = col_character(), organisationunitid = col_character(),
    organisationunitname = col_character(), organisationunitcode = col_character(), organisationunitdescription = col_character()
  )

  # load input file
  df <- readr::read_csv(filename,
                        col_types = column_types) |>
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
    cat(cli::col_red("\nPROCESSING ERROR ----"),
        cli::col_red("\nInput file not recognized. Please verify the path to your file and try again."),
        "\n")

  }

}
