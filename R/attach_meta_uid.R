#' Retreive and attach province and district uid metadata
#'
#'  `attach_meta_uid` utilizes stored user credentials to access
#'  a GoogleSheet where province and district uid metadata is
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


attach_meta_uid <- function(df, sheetname = "map_distrito_psnuuid") {

  #save google id
  path_meta <- googlesheets4::as_sheets_id("1cs8dC6OIFsjIJPIew3puPd1NkjGVeijz4jRnk3yiMR4")

  site_map <- googlesheets4::read_sheet(path_meta, sheet = sheetname) %>%
    dplyr::select(distrito, snuuid, psnuuid)

  df <- df %>%
    dplyr::left_join(site_map,
                     by = dplyr::join_by(distrito)) %>%
    dplyr::relocate(snuuid, .after = provincia) %>%
    dplyr::relocate(psnuuid, .after = distrito)

  return(df)

}
