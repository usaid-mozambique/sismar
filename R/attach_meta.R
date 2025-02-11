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


#' Retreive and attach MQ metadata
#'
#'  `attach_meta_mq` utilizes stored user credentials to access
#'  a GoogleSheet where province and district uid metadata is
#'  stored.
#'
#' @param df Dataframe cleaned via reshape_sisma
#' @return A dataframe containing mq metadata
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- attach_meta_mq(df)}


attach_meta_mq <- function(df) {

  #save google id
  path_meta <- googlesheets4::as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U")

  site_map <- googlesheets4::read_sheet(path_meta, sheet = "list_mq") |>
    dplyr::filter(status == 1) |>
    dplyr::distinct(sisma_uid) |>
    pull(sisma_uid)


  df <- df |>
    dplyr::mutate(
      service_mq = ifelse(sisma_uid %in% site_map, "MQ", "Non-MQ")
    ) |>
    dplyr::relocate(tidyselect::starts_with("service_"), .after = us)

  return(df)

}

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


#' Retreive and attach partner metadata
#'
#'  `attach_meta_pepfar_ip` utilizes stored user credentials to access
#'  a GoogleSheet where province and district uid metadata is
#'  stored.
#'
#' @param df Dataframe cleaned via reshape_sisma
#' @return A dataframe containing partner metadata
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- attach_meta_pepfar_ip(df)}


attach_meta_pepfar_ip <- function(df) {

  #save google id
  path_meta <- googlesheets4::as_sheets_id("1CG-NiTdWkKidxZBDypXpcVWK2Es4kiHZLws0lFTQd8U")

  site_map <- googlesheets4::read_sheet(path_meta, sheet = "list_ajuda") |>
    dplyr::select(sisma_uid,
                  support_partner = partner_pepfar_clinical) |>
    dplyr::filter(support_partner != "ITECH")

  df <- df |>
    dplyr::left_join(site_map,
                     by = dplyr::join_by(sisma_uid)) |>
    dplyr::mutate(
      support_partner = dplyr::case_match(
        support_partner,
        "MISAU" ~ "AJUDA Graduation",
        "CCS" ~ "CCS",
        "ECHO" ~ "ECHO",
        "FGH" ~ "FGH",
        "ARIEL" ~ "ARIEL",
        "EGPAF" ~ "EGPAF",
        "ICAP" ~ "ICAP",
        .default = "Sustainability"),
      support_ajuda = dplyr::case_when(
        support_partner != "Sustainability" ~ "AJUDA",
        TRUE ~ "Sustainability"
      )
    ) |>
    dplyr::relocate(tidyselect::starts_with("support_"), .after = us)

  return(df)

}


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
