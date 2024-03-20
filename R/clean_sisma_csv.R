#' Clean SISMA .csv exports
#'
#' `clean_sisma_csv()` is a helper function for cleaning a .csv pivot table
#' exported from SISMA
#'
#' @param file The path to a .csv file exported from SISMA that contains
#'   data element or indicator variables running wide
#'
#' @return `clean_sisma_csv()` returns an object with 7 columns (period, snu,
#'   psnu, sitename, sisma_uid, indicator, value). Unnecessary and duplicate
#'   columns from the input file have been removed, a period column coded from
#'   the original periodcode, and all remaining numeric columns pivoted long
#' @export
#'
#' @examples
#'  \dontrun{
#'  df <- clean_sisma_csv()}


clean_sisma_csv <- function(file) {

  df <- readr::read_csv(file) %>%

    dplyr::filter(!organisationunitid == "abg5UReivZX") %>% # for whatever reason this site creates issues

    janitor::clean_names() %>%

    dplyr::select(!c(periodname,
                     periodid,
                     perioddescription,
                     organisationunitcode,
                     organisationunitdescription,
                     orgunitlevel1,
                     organisationunitname)) %>%

    tidyr::pivot_longer(!c(periodcode,
                           orgunitlevel2,
                           orgunitlevel3,
                           orgunitlevel4,
                           organisationunitid),
                        names_to = "indicator",
                        values_to = "value",
                        values_transform = list(value = as.numeric)) %>%

    # dplyr::filter(!is.na(value)) %>%

    dplyr::mutate(periodcode = stringr::str_replace_all(periodcode,

                                                        c("Q1" = "03",
                                                          "Q2" = "06",
                                                          "Q3" = "09",
                                                          "Q4" = "12")),
                  periodcode = paste0(periodcode, "01"),
                  periodcode = lubridate::ymd(periodcode),
                  dplyr::across(c(orgunitlevel2, orgunitlevel3), ~ stringr::str_to_title(.))) %>%

    dplyr::select(period = periodcode,
                  snu = orgunitlevel2,
                  psnu = orgunitlevel3,
                  sitename = orgunitlevel4,
                  sisma_uid = organisationunitid,
                  indicator,
                  value)

  return(df)

}
