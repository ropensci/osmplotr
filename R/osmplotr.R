#' osmplotr.
#'
#' Produces customisable images of OpenStreetMap data.  Extracts OpenStreetMap
#' data for specified key-value pairs (e.g.  key='building') using the overpass
#' API. Different OSM objects can be plotted in different colours using the
#' function add_osm_objects().  The function group_osm_objects()
#' enables customised highlighting of selected regions using different graphical
#' schemes designed to contrast with surrounding backgrounds.
#'
#' @name osmplotr
#' @docType package
#' @import RCurl sp spatstat osmar XML ggm rgeos
#' @importFrom igraph graph_from_edgelist shortest_paths
#' @importFrom graphics polypath lines points locator plot.new par rect text
#' @importFrom methods slot as
#' @importFrom grDevices dev.new dev.list dev.off dev.cur rainbow rgb col2rgb png
#' @importFrom utils txtProgressBar setTxtProgressBar combn tail
#' @importFrom stats runif
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
#'  \item highways1 A SpatialPoints object containing 178 points representing
#'  the circular perimeter of ('Kingsway', 'Holborn', 'Farringdon.St', 'Strand',
#'  'Fleet.St', 'Aldwych')
#'  \item highways2 A SpatialPoints object containing 126 points representing
#'  the circular perimeter of ('Queen.s.Walk', 'Blackfriars', 'Waterloo', 
#'  'The.Cut')
#'  \item highways2 A SpatialPoints object containing 126 points representing
#'  the circular perimeter of ('Regent.St', 'Oxford.St', 'Shaftesbury')
#' }
#'
#' The vignette 'Downloading Data' contains the script used to generate these
#' data.
#'
#' @docType data
#' @keywords datasets
#' @name london
#' @format A list of spatial objects
NULL
