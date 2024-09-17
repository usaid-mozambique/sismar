#' Clean SISMA .csv exports
#'
#' @description `clean_sisma_csv()` A helper function that cleans .csv pivot tables
#' exported from SISMA
#' `r lifecycle::badge("superseded")`
#' `clean_sisma_csv` has been superseded in favor of `clean_sisma_df`
#'
#' @param file Path to a .csv file exported from SISMA that contains
#'   data element variables running wide and period and organizational
#'   units running long
#'
#' @return `clean_sisma_csv()` returns an object with 7 columns (period, snu,
#'   psnu, sitename, sisma_uid, indicator, value). Unnecessary columns from
#'   the input file are removed and a period column coded from the input
#'   variable periodcode is created. All numeric data element columns that
#'   represent indicator or data element values are pivoted long
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'  df <- clean_sisma_csv()}


clean_sisma_csv <- function(file) {

  column_types <- cols(
    .default = col_double(),  # default to double
    periodid  = col_character(), periodname = col_character(), periodcode = col_character(),
    perioddescription = col_character(), orgunitlevel1 = col_character(), orgunitlevel2 = col_character(),
    orgunitlevel3 = col_character(), orgunitlevel4 = col_character(), organisationunitid = col_character(),
    organisationunitname = col_character(), organisationunitcode = col_character(), organisationunitdescription = col_character()
  )

  df <- readr::read_csv(file,
                        col_types = column_types) %>%

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
