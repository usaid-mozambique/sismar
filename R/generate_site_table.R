#' Generate cleaned health facility table
#'
#' @param file Path to a .csv file exported from SISMA that contains
#'   a full list of health facilities
#'
#' @return `generate_site_table` returns a object with 7 deduplicated columns (sisma_uid,
#'   snu, psnu, sitename)
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'  df <- generate_site_table()}

generate_site_table <- function(file) {

  df <- readr::read_csv(file) |>
    dplyr::distinct(orgunitlevel2, orgunitlevel3, orgunitlevel4, organisationunitid, .keep_all = TRUE) |>
    dplyr::mutate(dplyr::across(c(orgunitlevel2, orgunitlevel3), ~ stringr::str_to_title(.))) |>
    dplyr::select(sisma_uid = organisationunitid,
                  provincia = orgunitlevel2,
                  distrito = orgunitlevel3,
                  us = orgunitlevel4
    )

  return(df)

}
