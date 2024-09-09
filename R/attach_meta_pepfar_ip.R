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
