#' Clean SISMA input file prior to parsing
#'
#' @param df
#'
#' @return Cleaned SISMA dataframe
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- clean_sisma_df()}

clean_sisma_df <- function(df) {

  df <- df |>

    dplyr::filter(!organisationunitid == "abg5UReivZX") |> # for whatever reason this site creates issues

    dplyr::select(!c(periodname,
                     periodid,
                     perioddescription,
                     organisationunitcode,
                     organisationunitdescription,
                     orgunitlevel1,
                     organisationunitname)) |>

    tidyr::pivot_longer(!c(periodcode,
                           orgunitlevel2,
                           orgunitlevel3,
                           orgunitlevel4,
                           organisationunitid),
                        names_to = "indicator",
                        values_to = "value",
                        values_transform = list(value = as.numeric)) |>

    dplyr::mutate(periodcode = stringr::str_replace_all(periodcode,
                                                        c("Q1" = "03",
                                                          "Q2" = "06",
                                                          "Q3" = "09",
                                                          "Q4" = "12")),
                  periodcode = paste0(periodcode, "01"),
                  periodcode = lubridate::ymd(periodcode),
                  dplyr::across(c(orgunitlevel2, orgunitlevel3), ~ stringr::str_to_title(.))) |>

    dplyr::select(period = periodcode,
                  snu = orgunitlevel2,
                  psnu = orgunitlevel3,
                  sitename = orgunitlevel4,
                  sisma_uid = organisationunitid,
                  indicator,
                  value) |>

    dplyr::arrange(period, snu, psnu, sitename, indicator)

  return(df)

}
