
#' Lista de unidades sanitárias SISMA
#'
#' @usage data(data_sisma_sitelist)
#'
#' @format Uma lista das variáveis incluídas no objecto de dados
#' \describe{
#'   \item{sisma_uid}{UID SISMA (e.g. x9jhds3ie6)}
#'   \item{provincia}{Provincia}
#'   \item{distrito}{Distrito}
#'   \item{us}{Unidade Sanitária}
#' }
"data_sisma_sitelist"


#' Mapa de UID para ligação entre DISA, SISMA e DATIM
#'
#' @usage data(data_disa_uid_map)
#'
#' @format Uma lista das variáveis incluídas no objecto de dados
#' \describe{
#'   \item{disa_uid}{UID DISA (e.g. CHAEM)}
#'   \item{sisma_uid}{UID SISMA (e.g. x9jhds3ie6)}
#'   \item{datim_uid}{UID DATIM (e.g. jIem8wI1Rx)}
#' }
"data_disa_uid_map"


#' Mapa para ligar os códigos de forma da província e do distrito aos dados SISMA
#'
#' @usage data(data_sisma_geo_above_site)
#'
#' @format Uma lista das variáveis incluídas no objecto de dados
#' \describe{
#'   \item{provincia}{Provincia}
#'   \item{snuuid}{UID da provincia}
#'   \item{distrito}{Distrito}
#'   \item{psnuuid}{UID do distrito}
#' }
"data_sisma_geo_above_site"


#' Mapa para ligar as coordenadas geográficas aos dados das unidades de saúde SISMA
#'
#' @usage data(data_sisma_geo_sites)
#'
#' @format Uma lista das variáveis incluídas no objecto de dados
#' \describe{
#'   \item{sisma_uid}{UID SISMA (e.g. x9jhds3ie6)}
#'   \item{latitude}{Latitude geográfica}
#'   \item{longitude}{Longitude geográfica}
#' }
"data_sisma_geo_sites"
