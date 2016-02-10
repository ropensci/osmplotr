#' urbanplotr.
#'
#' @name urbanplotr
#' @docType package
#' @import RCurl sp spatstat spatialkernel osmar XML igraph ggm
NULL

#' london 
#'
#' A list of SpatialPolygonsDataFrames (SPDF) and SpatialLinesDataFrames (SLDF)
#' containing OpenStreetMap polygons and lines for various OpenStreetMap
#' structures in a small part of central London, U.K.  (bbox = -0.15, 51.5,
#' -0.1, 51.52). The list includes:
#' \enumerate{
#'  \item dat_A an SPDF of amenities with 1,157 polygons 
#'  \item dat_B an SPDF of buildings with 6,208 polygons 
#'  \item dat_G an SPDF of grassed areas with 50 polygons 
#'  \item dat_H an SLDF of highways with 4,524 polygons 
#'  \item dat_N an SPDF of natural areas with 36 polygons 
#'  \item dat_P an SPDF of parks with 49 polygons 
#' }
#'
#' @docType data
#' @keywords datasets
#' @name london
#' @usage data(london)
#' @format A list of spatial objects
NULL
