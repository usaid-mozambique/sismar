#' Anexar: Coordenadas geográficas
#'
#' `attach_meta_coord` Une as coordenadas geográficas da unidade sanitária gravadas no pacote `sismar` ao objeto de dados introduzido pelo utilizador
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


#' Anexar: Metadados da melhoria da qualidade
#'
#' `attach_meta_mq` Une iniciativas de melhoria da qualidade gravadas no pacote `sismar` ao objeto de dados introduzido pelo utilizador
#'
#' @param df Dataframe limpo através de `process_sisma_export`
#' @return Um objeto de dados que contém metadados MQ
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

#' Anexar: Metadados do apoio dos parceiros
#'
#' `attach_meta_partner` Une informações sobre o apoio dos parceiros gravadas no pacote `sismar` ao objeto de dados introduzido pelo utilizador
#'
#' @param df Quandro de dados limpo através de `process_sisma_export`
#' @param sheetname Nome da folha do PCMD_Site_Map
#' @return Quadro de dados contendo coluna que indica o apoio dos parceiros
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


#' Anexar: Metadados do apoio dos parceiros PEPFAR
#'
#'  `attach_meta_pepfar_ip` Une informações sobre o apoio dos parceiros PEPFAR gravadas no pacote `sismar` ao objeto de dados introduzido pelo utilizador
#'
#' @param df Quadro de dados limpo através de `process_sisma_export`
#' @return Quadro de dados contendo coluna que indica o apoio dos parceiros PEPFAR
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


#' Anexar: Metadados uid das provincias e distritos
#'
#'  `attach_meta_uid` Une informações dos UIDs das provincias e distritos
#'
#' @param df Quadro de dados limpo através de `process_sisma_export`
#' @param sheetname Nome da folha do MISAU_Site_Map
#' @return Quadro de dados contendo colunas para metadados das provincias e distritos
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
