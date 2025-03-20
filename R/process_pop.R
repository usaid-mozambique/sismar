#' Title
#'
#' @param file_inventory xxx
#' @param input_sheets yyy
#' @param output_type zzz
#'
#' @return df
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- process_pop_ine()}

process_pop_ine <- function(file_inventory, input_sheets, output_type = "MISAU") {

  # Process all files and sheets
  df <- purrr::map(file_inventory, .progress = TRUE, function(file) {

    # Process each sheet for the current file
    file_data <- purrr::map(input_sheets, .progress = TRUE, function(sheet) {
      load_pop_ine(file, sheet)
    })

    # Combine data from all sheets for the current file
    dplyr::bind_rows(file_data)

  }) |>

    # Combine data from all files
    dplyr::bind_rows() |>

    # Clean and finalize dataset
    tidyr::pivot_longer(cols = total_total:rural_female,
                        names_to = c("urban_rural", "sex"),
                        names_sep = "_",
                        values_to = "value") |>

    dplyr::filter(!urban_rural == "total",
                  !sex == "total") |>

    dplyr::mutate(district = stringr::str_trim(district),
                  age = dplyr::if_else(age == "80+", "80", age),
                  age = as.numeric(age),
                  value = as.numeric(value),
                  value = tidyr::replace_na(value, 0)) |>

    dplyr::left_join(map_psnu, by = "district") |>
    dplyr::select(!district) |>
    dplyr::relocate(dplyr::any_of(c("provincia", "distrito", "snuuid", "psnuuid")), .before = everything()) |>
    dplyr::select(!c("snu", "psnu")) |>
    dplyr::mutate(
      sex = dplyr::if_else(sex == "male", "Masculino", "Feminino"),
      urban_rural = dplyr::if_else(urban_rural == "urban", "Urbano", "Rural"),
    ) |>
    dplyr::rename(periodo = year,
                  idade = age,
                  disaggregacao = urban_rural,
                  sexo = sex,
                  valor = value)

  # Return the cleaned dataframe
  return(df)

}





#' Esta função lê todos os conjuntos de dados da população de uma pasta especificada
#'
#' @param filename utiliza um caminho fornecido pelo utilizador
#' @param input_tabs folhas no ficheiro a ser processado
#'
#' @return um conjunto de dados de todos os ficheiros e separadores
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- load_pop_ine()}

load_pop_ine <- function(filename, input_tabs) {

  df <- suppressMessages({
    readxl::read_excel(path = filename,
                       sheet = input_tabs,
                       skip = 87,
                       col_names = FALSE) |>

      dplyr::rename(
        age = `...1`,
        total_total = `...2`,
        total_male = `...3`,
        total_female = `...4`,
        urban_total = `...5`,
        urban_male = `...6`,
        urban_female = `...7`,
        rural_total = `...8`,
        rural_male = `...9`,
        rural_female = `...10`
      ) |>

      dplyr::mutate(
        district = dplyr::case_when(stringr::str_detect(age, pattern = "idade.") ~ age,
                                    TRUE ~ NA_character_)) |>

      tidyr::fill(district, .direction = "down") |>
      dplyr::filter(!stringr::str_detect(age, pattern = "Idade|Total|Quadro")) |>

      dplyr::mutate(district = stringr::str_extract(district, "(?<=idade\\.).*"),
                    year = input_tabs,
                    district = stringr::str_extract(district, "^.*(?=,)")) |>

      dplyr::relocate(tidyselect::any_of(c("district", "year")), .before = everything())
  })

  return(df)

}


#' Recodificar a idade em faixas etárias de cinco anos
#'
#' @param df
#'
#' @return df
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- recode_ine_age()}

recode_ine_age <- function(df) {

  # Categorize idade into 5-year age brackets
  df <- df |>
    mutate(age_bracket = cut(idade,
                             breaks = seq(0, 80, by = 5),
                             labels = sprintf("%02d-%02d", seq(0, 75, by = 5), seq(4, 79, by = 5)),
                             include.lowest = TRUE,
                             right = FALSE)) |>
    mutate(age_bracket = ifelse(idade >= 80, "80+", as.character(age_bracket)))

  # Group by the new age_bracket, along with other grouping variables
  df <- df |>
    group_by(periodo, age_bracket, sexo, across(starts_with("disaggregacao")), across(starts_with("provincia")), across(starts_with("distrito"))) |>
    summarize(valor = sum(valor, na.rm = TRUE), .groups = 'drop') |>
    filter(valor > 0)

  return(df)

}
