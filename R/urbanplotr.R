#' urbanplotr.
#'
#' @name urbanplotr
#' @docType package
#' @import RCurl sp spatstat spatialkernel osmar XML igraph ggm
NULL

#' datA 
#'
#' A SpatialPolygonsDataFrame containing OpenStreetMap polygons for amenities
#' (key=amenity) in a small part of central London, U.K.
#' (bbox = -0.15, 51.5, -0.1, 51.52).
#'
#' @docType data
#' @keywords datasets
#' @name datA
#' @usage data(datA)
#' @format A SpatialPolygonsDataFrame with 1,157 polygons
NULL

#' datBU 
#'
#' A SpatialPolygonsDataFrame containing OpenStreetMap polygons for buildings
#' (key=building) in a small part of central London, U.K.
#' (bbox = -0.15, 51.5, -0.1, 51.52).
#'
#' @docType data
#' @keywords datasets
#' @name datBU
#' @usage data(datBU)
#' @format A SpatialPolygonsDataFrame with 6,209 polygons
NULL

#' datG 
#'
#' A SpatialPolygonsDataFrame containing OpenStreetMap polygons for grassed
#' areas (key=landuse, value=grass) in a small part of central London, U.K.
#' (bbox = -0.15, 51.5, -0.1, 51.52).
#'
#' @docType data
#' @keywords datasets
#' @name datG
#' @usage data(datG)
#' @format A SpatialPolygonsDataFrame with 50 polygons
NULL

#' datH 
#'
#' A SpatialLinesDataFrame containing OpenStreetMap lines for all highways
#' (key=highway) in a small part of central London, U.K.
#' (bbox = -0.15, 51.5, -0.1, 51.52).
#'
#' @docType data
#' @keywords datasets
#' @name datH
#' @usage data(datH)
#' @format A SpatialLinesDataFrame with 4,517 lines 
NULL

#' datN 
#'
#' A SpatialPolygonsDataFrame containing OpenStreetMap polygons for natural
#' areas (key=natural) in a small part of central London, U.K.
#' (bbox = -0.15, 51.5, -0.1, 51.52).
#'
#' @docType data
#' @keywords datasets
#' @name datN
#' @usage data(datN)
#' @format A SpatialPolygonsDataFrame with 36 polygons
NULL

#' datP 
#'
#' A SpatialPolygonsDataFrame containing OpenStreetMap polygons for parks
#' (key=leisure, value=park) in a small part of central London, U.K.
#' (bbox = -0.15, 51.5, -0.1, 51.52).
#'
#' @docType data
#' @keywords datasets
#' @name datP
#' @usage data(datP)
#' @format A SpatialPolygonsDataFrame with 49 polygons
NULL
