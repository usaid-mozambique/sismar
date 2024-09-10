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

