
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
#' @usage data(data_sisma_geo_above_site)
#'
#' @format A list of the variables included in the site list
#' \describe{
#'   \item{data_sisma_geo_above_site}{provincia}
#'   \item{data_sisma_geo_above_site}{snuuid}
#'   \item{data_sisma_geo_above_site}{distrito}
#'   \item{data_sisma_geo_above_site}{psnuuid}
#' }
"data_sisma_geo_above_site"


#' Map for linking geocoordinates to SISMA health facility data
#'
#' List of indicators
#'
#' @usage data(data_sisma_geo_sites)
#'
#' @format A list of the variables included in the site list
#' \describe{
#'   \item{data_sisma_geo_sites}{sisma_uid}
#'   \item{data_sisma_geo_sites}{latitude}
#'   \item{data_sisma_geo_sites}{longitude}
#' }
"data_sisma_geo_sites"
