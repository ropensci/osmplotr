#' urbanplotr.
#'
#' @name urbanplotr
#' @docType package
#' @import RCurl sp spatstat spatialkernel osmar XML igraph ggm rgeos
NULL

#' london 
#'
#' A list of SpatialPolygonsDataFrames (SPDF) and SpatialLinesDataFrames (SLDF)
#' containing OpenStreetMap polygons and lines for various OpenStreetMap
#' structures in a small part of central London, U.K.  (bbox = -0.15, 51.5,
#' -0.1, 51.52). The list includes:
#' \enumerate{
#'  \item dat_H an SLDF of non-primary highways with 3,868 lines 
#'  \item dat_HP an SLDF of primary highways with 659 lines 
#'  \item dat_B an SPDF of non-commerical buildings with 6,177 polygons 
#'  \item dat_BC an SPDF of commerical buildings with 30 polygons 
#'  \item dat_A an SPDF of amenities with 1,157 polygons 
#'  \item dat_G an SPDF of grassed areas with 50 polygons 
#'  \item dat_P an SPDF of parks with 49 polygons 
#'  \item dat_N an SPDF of natural areas with 36 polygons 
#'  \item dat_RFH an SPDF containing 1 polygon representing Royal Festival Hall
#'  \item dat_ST an SPDF containing 1 polygon representing 150 Stamford Street
#' }
#'
#' @docType data
#' @keywords datasets
#' @name london
#' @usage data(london)
#' @format A list of spatial objects
NULL
