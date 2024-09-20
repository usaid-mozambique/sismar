
#' Deduplicated SISMA site list
#'
#' List of indicators
#'
#' @usage data(data_sisma_sitelist)
#'
#' @format A list of the variables included in the site list
#' \describe{
#'   \item{data_sisma_sitelist}{sisma_uid}
#'   \item{data_sisma_sitelist}{provincia}
#'   \item{data_sisma_sitelist}{distrito}
#'   \item{data_sisma_sitelist}{us}
#' }
"data_sisma_sitelist"


#' UID map for linking DISA, SISMA and DATIM
#'
#' List of indicators
#'
#' @usage data(data_disa_uid_map)
#'
#' @format A list of the variables included in the site list
#' \describe{
#'   \item{data_disa_uid_map}{disa_uid}
#'   \item{data_disa_uid_map}{sisma_uid}
#'   \item{data_disa_uid_map}{datim_uid}
#' }
"data_disa_uid_map"


#' Map for linking province and district shape codes to SISMA data
#'
#' List of indicators
#'
#' @usage data(data_sisma_shape_map)
#'
#' @format A list of the variables included in the site list
#' \describe{
#'   \item{data_sisma_shape_map}{provincia}
#'   \item{data_sisma_shape_map}{snuuid}
#'   \item{data_sisma_shape_map}{distrito}
#'   \item{data_sisma_shape_map}{psnuuid}
#' }
"data_sisma_shape_map"
