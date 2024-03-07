#' Retreive and attach partner metadata
#'
#'  `attach_meta_partner` utilizes stored user credentials to access
#'  a GoogleSheet where province and district uid metadata is
#'  stored.
#'
#' @param df Dataframe cleaned via reshape_sisma
#' @param sheetname Sheet name from the PCMD_Site_Map
#' @return A dataframe containing partner metadata
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- attach_meta_partner(df)}


attach_meta_partner <- function(df, sheetname = "Alcancar") {

  #save google id
  path_meta <- googlesheets4::as_sheets_id("1OwvyfilL4yitA1Xot3Kdbm7vNThBrJb6QhQm6ScDga8")

  site_map <- googlesheets4::read_sheet(path_meta, sheet = sheetname) %>%
    dplyr::select(sisma_uid,
                  tidyselect::starts_with("support_partner"))

  df <- df %>%
    dplyr::left_join(site_map,
                     by = dplyr::join_by(sisma_uid)) %>%
    dplyr::relocate(tidyselect::starts_with("support_partner"), .after = us)

  return(df)

}
