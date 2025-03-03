#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Função auxiliar que cria uma estrutura padrão de pastas
#'
#' @param folder_list lista de pastas a instalar
#'
#' @return Um conjunto padrão de pastas no projeto para utilizar com as funções `sismar`
#' @export
#'
#' @examples
#'  \dontrun{
#'
#'  setup_sismar_folder()}

setup_sismar_folder <- function(

  folder_list = list("Data", "Data/sisma", "Data/disa", "Data/pepfar", "Data/ine", # data
                     "Data/sismar/processed", "Data/disa/processed", "Data/pepfar/processed", # data processed
                     "Images", "Scripts", "AI", "Dataout", "Data_public", "GIS", "Documents", "Graphics", "markdown") # other
)

{

  if(!is.list(folder_list))
    stop("Please provide a list of directories to create for the project.")
  usethis::ui_info("The following directories will be created (if they do no already):")
  print(glue::glue(crayon::green('{folder_list}')))
  suppressWarnings(
    purrr::walk(folder_list, ~dir.create(.))
  )

}


utils::globalVariables(c("organisationunitcode","organisationunitdescription","organisationunitid",
                         "organisationunitname", "orgunitlevel1", "orgunitlevel2", "orgunitlevel3",
                         "orgunitlevel4", "period", "period_cohort", "periodcode", "perioddescription",
                         "periodid", "periodname", "psnu", "result_status", "sex", "sisma_uid", "sitename",
                         "smi_cpp_puerperas_hiv_das_puerperas_testadas_na_consulta_pos_parto",
                         "smi_cpp_puerperas_testadas_na_consulta_pos_parto",
                         "smi_mat_hiv_identificadas_na_maternidade",
                         "smi_mat_total_testadas_para_hiv_na_maternidade",
                         "smi_pf_hiv_utentes_testadas_na_consulta_de_saude_reprodutiva",
                         "smi_pf_utentes_hiv_das_mulheres_testadas_na_csr_pf",
                         "smi_ug_hiv_identificadas_na_ginecologia",
                         "smi_ug_total_testadas_para_hiv_nas_urgencias_de_ginecologia", "snu",
                         "sub_group", "value", "age", "age_coarse", "data_sisma_ats_autoteste_map",
                         "data_sisma_ats_ccsd_map", "data_sisma_ats_ci_map", "data_sisma_ats_hist_map",
                         "data_sisma_ats_results_map", "data_sisma_hiv_apss_map",
                         "data_sisma_hiv_prep_map", "data_sisma_hiv_tarv_map",
                         "data_sisma_smi_ccr_map", "data_sisma_smi_cpn_map", "data_sisma_smi_mat_map",
                         "disaggregate", "disaggregate_sub", "indicator", "indicator_new",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_10_14_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_10_14_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_10_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_10_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_15_19_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_15_19_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_20_24_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_20_24_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_25_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_25_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_10_14_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_10_14_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_10_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_10_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_15_19_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_15_19_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_20_24_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_20_24_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_25_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_de_adolescentes_e_jovens_testados_positivo_25_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_10_14_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_10_14_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_10_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_10_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_15_19_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_15_19_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_20_24_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_20_24_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_25_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_de_parceiros_testados_positivos_25_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_parceiros_testados_10_14_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_parceiros_testados_10_14_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_parceiros_testados_10_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_parceiros_testados_10_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_parceiros_testados_15_19_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_parceiros_testados_15_19_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_parceiros_testados_20_24_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_parceiros_testados_20_24_anos_masculino",
                         "mz_saaj_testagem_para_hiv_numero_parceiros_testados_25_anos_feminino",
                         "mz_saaj_testagem_para_hiv_numero_parceiros_testados_25_anos_masculino"))


