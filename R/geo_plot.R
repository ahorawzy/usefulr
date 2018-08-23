#' Plot geographical point.
#'
#' This function helps you plot georaphy points easily when given a dataframe.
#'
#' This function is built based on package 'leaflet' and 'leafletCN'. 'leaflet' is a brilliant
#' package of GIS and 'leafletCN' contains useful China geographical information.
#' This function receives a dataframe contains points and their geographical information of
#' longitude and latitude perhaps as well as other information about points.
#' It returns a map with points on it. Other information can also be added such as
#' administrative area distinction and type of points.
#'
#' @param pointdf The dataframe contains points information. It must contains columns as follows:
#' \itemize{
#'     \item label: The name of point which will added as label of point on map.
#'     \item lng: The longitude of point, range from [-180,180].
#'     \item lat: The latitude of point, range from [-90,90].
#' }
#' It can contains columns as follows:
#' \itemize{
#'     \item type: The type of point, which can be character.
#'     \item popup: Set the popup of point.
#' }
#' @param region Default value is FALSE. It can be a string of Chinese administrative area name.
#' @param type Default value is FALSE. When set it as True, different type of points will be drawn
#' with different colour.
#' @param map The basemap to be chosen. Default value is 'OpenStreet'. It also can be:
#' \itemize{
#'     \item 'amap' which is provided by GaoDe.
#'     \item 'landform' which is provided by Esri of satellite basemap.
#'     \item 'watercolor' which is provided by Stamen of watercolor basemap.
#' }
#'
#' @export

geo_pointplot <- function(pointdf, region = NULL, type = FALSE, map = "OpenStreet", popup = NULL, na.rm = FALSE) {
  if (na.rm) {
    pointdf <- na.omit(pointdf)
  }

  if (sum(is.na(pointdf)) != 0) {
    stop("There is NA value in the dataframe. Please clear it.")
  }

  m <- leaflet::leaflet(pointdf)
  if (map == "OpenStreet") {
    m <- leaflet::addTiles(m)
  } else if (map == "amap") {
    m <- leafletCN::amap(m)
  } else if (map == "landform") {
    m <- leaflet::addProviderTiles(m, "Esri.WorldImagery")
  } else if (map == "watercolor") {
    m <- leaflet::addProviderTiles(m, "Stamen.Watercolor")
  }
  if (!is.null(region)) {
    reg <- leafletCN::leafletGeo(region)
    m <- leaflet::addPolygons(m, data = reg, stroke = TRUE, smoothFactor = 1, fillOpacity = 0, weight = 3)
  }

  if (isTRUE(type)){
    types <- unique(pointdf[, "type"])
    ntype <- length(types)
    if (ntype <= 5) {
      cols <- as.character(wesanderson::wes_palette(n = ntype, name = "BottleRocket2"))
    } else {
      cols <- as.character(wesanderson::wes_palette(n = ntype, name = "Darjeeling1", type = "continuous"))
    }
    pal <- leaflet::colorFactor(cols, domain = types)
    m <- leaflet::addCircleMarkers(m, lng = ~lng, lat = ~lat, label = ~label, color = ~pal(type), popup = ~popup)
    m <- leaflet::addLegend(m, "bottomright", pal = pal, values = types)
  } else {
    m <- leaflet::addCircleMarkers(m, lng = ~lng, lat = ~lat, label = ~label, popup = ~popup)
  }

  return(m)
}
