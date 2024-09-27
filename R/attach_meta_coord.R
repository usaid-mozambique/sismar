#' Anexar coordenadas geográficas das unidades sanitárias
#'
#' `attach_meta_coord` utiliza as credenciais de utilizador armazenadas para aceder
#' a uma folha de cálculo do Google onde as coordenadas geográficas das unidades de
#' saúde estão armazenadas.
#'
#' @param df Dataframe já transformado através de `process_sisma_csv` ou `process_sisma_export`
#' @param sheetname Nome da folha do MISAU_Site_Map
#' @return Dataframe contendo coordenadas geográficas das unidades sanitárias
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- attach_meta_uid(df)}


attach_meta_coord <- function(df, sheetname = "map_us_coord") {

  #save google id
  path_meta <- googlesheets4::as_sheets_id("1cs8dC6OIFsjIJPIew3puPd1NkjGVeijz4jRnk3yiMR4")

  site_map <- googlesheets4::read_sheet(path_meta, sheet = sheetname) %>%
    dplyr::select(sisma_uid = organisationunitid,
                  latitude,
                  longitude) %>%
    dplyr::filter(!is.na(latitude))

  df <- df %>%
    dplyr::left_join(site_map,
                     by = dplyr::join_by(sisma_uid)) %>%
    dplyr::relocate(latitude, .after = us) %>%
    dplyr::relocate(longitude, .after = us)

  return(df)

}
