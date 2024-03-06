#' Retreive and attach health facility geocoordinates metadata
#'
#'  `attach_meta_coord` utilizes stored user credentials to access
#'  a GoogleSheet where health facility geocoordinates are
#'  stored.
#'
#' @param df Dataframe cleaned via reshape_sisma
#' @param sheetname Sheet name from the MISAU_Site_Map
#' @return A dataframe containing site metadata
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- attach_meta_uid(df)}


attach_meta_coord <- function(df, sheetname = "lista_us_sisma") {

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
