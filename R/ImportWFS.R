#' Import ScheldeMonitor related WFS layers
#'
#' Import ScheldeMonitor related WFS layers. Get a list of the available layers using
#' [scheldemonitoR::scheldemonitor_wfs_layers]
#'
#' @param layer_name Character. Check available `layer_name`(s) using `scheldemonitoR::scheldemonitor_wfs_layers`
#'
#' @return
#' @export
#'
#' @examples
#' # get a layer_name, for example, the first one of the dataset
#' layer_name <- scheldemonitor_wfs_layers[9, "layer_name"]
#'
#'
#'
# importWFS <- function(layer_name) {
#   common_url = "https://www.scheldemonitor.be/geoviewer/proxy//"
#
#   # Function for constructing call url
#   name_proxy <- scheldemonitor_wfs_layers
#   # get service proxy url
#   service <- name_proxy[name_proxy$layer_name == layer_name, ]$url_server
#   # assert("No WFS for this parameter!", {
#   #   name_proxy[name_proxy$l==name,]$wfs=='Y'
#   # })
#   # service <- paste(service,'/wfs', sep='')
#   wfs_bwk <- paste(common_url, service, sep = '')
#   print(wfs_bwk)
#   # get shapes
#   gjson <- get_sf(wfs_bwk, layer_name)
#   return(gjson)
# }
# helper -------------------------------------------------------------

# get_sf <- function(wfs_url, layer_name, bbox = NaN) {
#   # Helper function for constructing query and
#   # perform Get request for reading shapes
#
#   # construct url
#   url <- httr::parse_url(wfs_url)
#   if (is.nan(bbox)) {
#     url$query <- list(service = "WFS",
#                       request = "GetFeature",
#                       outputFormat = "application/json",
#                       typename = layer_name)
#   }
#   else {
#     url$query <- list(service = "WFS",
#                       request = "GetFeature",
#                       typename = layer_name,
#                       outputFormat = "application/json",
#                       bbox = bbox)
#   }
#   request <- httr::build_url(url)
#   # read chapes from url
#   gjson <- sf::read_sf(request)
#   return(gjson)
# }
