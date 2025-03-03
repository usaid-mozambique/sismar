#' Processar ficheiro .csv da Naomi
#'
#' `process_naomi` processa ficheiros .csv da Naomi e junta metadados para facilitar mapeamento geoespacial de indicadores
#'
#' @param filename Caminho para Naomi .csv
#' @param quarter_lab Define o trimestre para o qual as estimativas dos indicadores serão extraídas
#' @param area_level_lab Define o nível geográfico das estimativas geradas
#' @return `process_naomi` devolve um objeto de dados que contém resultados do indicador Naomi deduplicados
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- process_naomi(df)}


process_naomi <- function(filename, quarter_lab = c("September 2023"), area_level_lab = c("Distrito")) {

  ind_absolute <- c("anc_already_art",
                    "anc_art_new",
                    "anc_clients",
                    "anc_known_pos",
                    "anc_plhiv",
                    "anc_tested_neg",
                    "anc_tested_pos",
                    "art_current",
                    "art_current_residents",
                    "aware_plhiv_attend",
                    "aware_plhiv_num",
                    "infections",
                    "plhiv",
                    "plhiv_attend",
                    "population",
                    "unaware_plhiv_attend",
                    "unaware_plhiv_num",
                    "untreated_plhiv_attend",
                    "untreated_plhiv_num")

  age_ind_absolute <- c("<1",
                        "1-4",
                        "5-9",
                        "10-14",
                        "15-19",
                        "20-24",
                        "25-29",
                        "30-34",
                        "35-39",
                        "40-44",
                        "45-49",
                        "50-54",
                        "55-59",
                        "60-64",
                        "65-69",
                        "70-74",
                        "75-79",
                        "80+")

  col_keep <- c("area_name",
                "area_id",
                "quarter_label",
                "sex",
                "age_group_label",
                "indicator_label",
                "mean",
                "lower",
                "upper")

  # load area_id map which links naomi var area_id with shape file snu1uid and psnuuid
  area_id_map <- googlesheets4::read_sheet("1HSKvJ8Tk2EbhXaxvtK5oFI9WPmAzt24G7ZCs_YVzzpE") |>
    dplyr::select(area_id, psnuuid, snu1uid)

  # load data and apply top-level de-duplication
  df <- readr::read_csv(filename) |>
    dplyr::filter(quarter_label %in% c(quarter_lab), # argument September 2023
                  area_level_label %in% c(area_level_lab), # argument Distrito
                  !sex == "both")

  # subset absolute indicators and apply secondary de-duplication
  df_abs <- df |>
    dplyr::filter(dplyr::case_when(indicator %in% ind_absolute ~ age_group_label %in% age_ind_absolute)) |>
    dplyr::arrange(area_name, indicator, sex, age_group_label) |>
    dplyr::mutate(sex = str_to_title(sex)) |>
    dplyr::select(tidyselect::all_of(col_keep))

  # subset relative (percentage) indicators and apply secondary de-duplication
  df_rel <- df |>
    dplyr::filter(!indicator %in% ind_absolute) |>
    dplyr::arrange(area_name, indicator, sex, age_group_label) |>
    dplyr::mutate(sex = str_to_title(sex)) |>
    dplyr::select(tidyselect::all_of(col_keep))

  # bind absolute and relative dataframes
  df_final <- dplyr::bind_rows(df_abs, df_rel) |>
    dplyr::left_join(area_id_map, by = "area_id") |>
    dplyr::relocate(c("snu1uid", "psnuuid", "indicator_label"), .after = area_name) |>
    dplyr::select(!c("area_id")) |>
    dplyr::rename(
      "area" = area_name,
      "indicator" = indicator_label,
      "quarter" = quarter_label,
      "age" = age_group_label)


  return(df_final)

}
