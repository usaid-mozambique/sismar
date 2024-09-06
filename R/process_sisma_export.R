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

  # load submission templates into named list
  # templates <- list(
  #   parse_sisma_ats_results = template_ats_results |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_ats_history = template_ats_hist |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_ats_index = template_ats_ci |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_ats_ccsd = template_ats_ccsd |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_ats_saaj_cm = template_ats_saaj |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_ats_smi = template_ats_smi |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_ats_auto = template_ats_auto |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_hiv_dah = template_hiv_dah |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_hiv_tarv = template_hiv_tarv |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_hiv_prep = template_hiv_prep |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_hiv_apss = template_hiv_apss |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_hiv_its = template_hiv_its |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_hiv_ajm_hc_mm = template_hiv_ajmhcmm |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_smi_cpn = template_smi_cpn |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_smi_mat = template_smi_mat |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_smi_ccr = template_smi_ccr |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_smi_cpp = template_smi_cpp |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_smi_pav = template_smi_pav |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_smi_ccd = template_smi_ccd |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_smi_ccs = template_smi_ccs |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_smi_ug = template_smi_ug |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_smi_pf = template_smi_pf |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols),
  #   parse_sisma_smi_pf_int = template_smi_pf_int |> dplyr::distinct(input_cols) |> dplyr::pull(input_cols)
  # )

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
