#' Cleans and transforms SISMA programmatic data sets into a
#' tidy format useful for analysis in R/PowerBI/Tableau.
#'
#' @description parse_sisma_csv cleans and transforms programmatic
#' data sets exported from SISMA into tidy data frames. Such
#' structured data sets are key to facilitating indicator analysis
#' in R/PowerBI/Tableau.
#'
#' ## Overview of programmatic data type options
#' * SMI-CPN: CPN, Saude Materna Infantil
#' * SMI-MAT: Maternidade, Saude Materna Infantil
#' * SMI-CCR: CCR, Saude Materna Infantil
#' * SMI-CPP: CCP, Saude Materna Infantil
#' * SMI-CCD: CCD, Saude Materna Infantil
#' * SMI-CCS: CCS, Saude Materna Infantil
#' * SMI-PAV: PAV, Saude Materna Infantil
#' * ATS Result: ATS Resultados, Programa de HIV
#' * ATS History: ATS Historial e Populacoes Chave, Programa de HIV
#' * ATS CI: ATS Caso Indice e Ligacao, Programa de HIV
#' * ATS SAAJ: ATS Serviço Amigo Adolescente Joven
#' * ATS CCSD: Consulta da Crianca Sadia e Consulta da Crianca Doente
#' * ATS SMI: ATS especifico a SMI (outros acomopanhantes na CPN, etc.)
#' * ATS Auto: ATS Autotestagem, Programa de HIV
#' * HIV TARV: Tratamento Antiretroviral (TARV), Programa de HIV
#' * HIV PREP: Profilaxia Pré-Exposição (PrEP), Programa de HIV
#' * HIV APSS: Apoio Psicossocial (APSS), Programa de HIV
#' * HIV ITS: Infecções de Transmissão Sexual (ITS), Programa de HIV
#' * HIV AJMHCMM: Adolescent Joven Mentor, Homen Campeao, Maes Mentora
#' * HIV DAH: Doenca Avancada do HIV
#'
#' @param data Path to SISMA dataset (saved in .csv format)
#' @param type Defines programmatic data type for processing
#' @return A tidy SISMA programmatic dataframe
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  df <- parse_sisma_csv()}

parse_sisma_csv <- function(data, type){

  switch(type,
         "SMI-CPN" = parse_sisma_smi_cpn(data), # SISMA search term "SMI-CPN"
         "SMI-MAT" = parse_sisma_smi_mat(data), # SISMA search term "SMI-MAT"
         "SMI-CCR" = parse_sisma_smi_ccr(data), # SISMA search term "SMI - CCR - "
         "SMI-CPP" = parse_sisma_smi_cpp(data), # SISMA search term "SMI-CPP"
         "SMI-PAV" = parse_sisma_smi_pav(data), # SISMA search term "SMI-PAV"
         "SMI-CCD" = parse_sisma_smi_ccd(data), # SISMA search term "SMI-CCD"
         "SMI-CCS" = parse_sisma_smi_ccs(data), # SISMA search term "SMI-CCS"
         "SMI-UG" = parse_sisma_smi_ug(data),   # SISMA search term "SMI-UG"
         "ATS Result" = parse_sisma_ats_results(data), # SISMA search term "ano" within Grupo de Elemento de Dados "ATS - Resumo Mensal (Nova)"
         "ATS History" = parse_sisma_ats_history(data), # SISMA search terms "Historial" & "chave" within Grupo de Elemento de Dados "ATS - Resumo Mensal (Nova)"
         "ATS CI" = parse_sisma_ats_index(data),  # SISMA search terms "diagonsticados" & "indice" within Grupo de Elemento de Dados "ATS - Resumo Mensal (Nova)"
         "ATS SAAJ" = parse_sisma_ats_saaj_cm(data), # SISMA search terms "MZ SAAJ - Testagem para HIV -" & "MZ C.MASC - Testados"
         "ATS CCSD" = parse_sisma_ats_ccsd(data),
         "ATS SMI" = parse_sisma_ats_smi(data),
         "ATS Auto" = parse_sisma_ats_auto(data), # SISMA search term "MZ HIV-AUTOTESTE"
         "HIV TARV" = parse_sisma_hiv_tarv(data), # SISMA search term "MZ HIV SIDA - "
         "HIV PREP" = parse_sisma_hiv_prep(data), # SISMA search term "MZ PREP"
         "HIV APSS" = parse_sisma_hiv_apss(data), # SISMA search term "MZ APSS PP"
         "HIV ITS" = parse_sisma_hiv_its(data), # SISMA search term "MZ ITS -"
         "HIV AJMHCMM" = parse_sisma_hiv_ajm_hc_mm(data), # SISMA search term "MZ AJMHCMM"
         "HIV DAH" = parse_sisma_hiv_dah(data), # SISMA search term "MZ DAH -"
  )

}
