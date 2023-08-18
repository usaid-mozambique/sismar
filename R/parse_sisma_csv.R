#' Cleans and transforms SISMA programmatic data sets into a
#' tidy format useful for analysis in R/PowerBI/Tableau.
#'
#' @description parse_sisma_csv cleans and transforms programmatic
#' data sets exported from SISMA into tidy data frames. Such
#' structured data sets are key to facilitating subsequent analysis
#' in R/PowerBI/Tableau.
#'
#' ## Overview of programmatic data type options
#' * SMI-CPN: CPN, Saude Materna Infantil
#' * SMI-MAT: Maternidade, Saude Materna Infantil
#' * SMI-CCR: CCR, Saude Materna Infantil
#' * ATS Result: ATS Resultados, Programa de HIV
#' * ATS History: ATS Historial, Programa de HIV
#' * ATS CI: ATS Caso Indice, Programa de HIV
#'
#' @param data Path to SISMA dataset (saved in .csv format)
#' @param type Defines programmatic data type for processing
#' @return A tidy dataframe with monthly sisma results
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_csv()}

parse_sisma_csv <- function(data, type){

  switch(type,
         "SMI-CPN" = parse_sisma_smi_cpn(data),
         "SMI-MAT" = parse_sisma_smi_mat(data),
         "SMI-CCR" = parse_sisma_smi_ccr(data),
         "ATS Result" = parse_sisma_ats_results(data), # search term "ano"
         "ATS History" = parse_sisma_ats_history(data),
         "ATS CI" = parse_sisma_ats_index(data),
         "ATS SAAJ" = parse_sisma_ats_saaj_cm(data),
         "ATS CCSD" = parse_sisma_ats_ccsd(data),
         "ATS SMI" = parse_sisma_ats_smi(data),
         "ATS Auto" = parse_sisma_ats_auto(data),
         "HIV TARV" = parse_sisma_hiv_tarv(data),
         "HIV PREP" = parse_sisma_hiv_prep(data),
         "HIV APSS" = parse_sisma_hiv_apss(data)
  )

}
