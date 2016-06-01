#' osmplotr.
#'
#' Produces customisable images of OpenStreetMap (OSM) data and enables data
#' visualisation using OSM objects.  Extracts data using the overpass API.
#' Contains the following functions, data, and vignettes.
#'
#' @section Data Functions:
#' \tabular{ll}{
#' \code{\link{extract_osm_objects}} \tab Download arbitrary OSM objects \cr
#' \code{\link{connect_highways}} \tab Returns points sequentially connecting
#' list of named highways \cr
#' }
#'
#' @section Basic Plotting Functions (without data):
#' \tabular{ll}{
#' \code{\link{add_axes}} \tab Overlay longitudinal and latitudinal axes on plot \cr 
#' \code{\link{add_osm_objects}} \tab Overlay arbitrary OSM objects \cr 
#' \code{\link{make_osm_map}} \tab Automate map production with structures
#' defined in \code{\link{osm_structures}} \cr 
#' \code{\link{osm_structures}} \tab Define structures and graphics schemes for
#' automating map production \cr 
#' \code{\link{osm_basemap}} \tab Initiate a \code{ggplot2} object for an OSM
#' map \cr 
#' \code{\link{print_osm_map}} \tab Print a map to specified graphics
#' device \cr
#' } 
#'
#' @section Advanced Plotting Functions (with data):
#' \tabular{ll}{
#' \code{\link{add_osm_groups}} \tab Overlay groups of objects using specified
#' colour scheme \cr 
#' \code{\link{add_osm_surface}} \tab Overlay data surface by interpolating given
#' data \cr 
#' \code{\link{add_colourbar}} \tab Overlay a scaled colourbar for data added
#' with \code{\link{add_osm_surface}} \cr 
#' }
#'
#' @section Colour Manipulation Functions:
#' \tabular{ll}{
#' \code{\link{adjust_colours}} \tab Lighted or darken given colours by specified
#' amount \cr 
#' \code{\link{colour_mat}} \tab Generate continuous 2D spatial matrix of
#' colours \cr 
#' }
#'
#' @section Other Functions:
#' \tabular{ll}{
#' \code{\link{get_bbox}} \tab return bounding box from input vector \cr 
#' }
#'
#' @section Data:
#' \tabular{ll}{
#' \code{\link{london}} \tab OSM Data from a small portion of central London \cr 
#' }
#'
#' @section Vignettes:
#' \tabular{ll}{
#' \code{basic-maps} \tab Describes basics of downloading data and making custom
#' maps \cr
#' \code{data-maps} \tab Describes how map elements can be coloured according to
#' user-provided data, whether categorical or continuous \cr
#' }
#'
#' @name osmplotr
#' @docType package
#' @import httr sp spatstat osmar XML ggm rgeos
#' @importFrom igraph graph_from_edgelist shortest_paths
#' @importFrom graphics lines locator plot.new par rect text
#' @importFrom methods slot as is hasArg
#' @importFrom grDevices dev.cur dev.new dev.off rainbow rgb col2rgb heat.colors
#' @importFrom grDevices bmp jpeg pictex png postscript svg tiff 
#' @importFrom utils txtProgressBar setTxtProgressBar combn tail
#' @importFrom stats runif
NULL

#' london 
#'
#' A list of \code{SpatialPolygonsDataFrames} (SPDF),
#' \code{SpatialLinesDataFrames} (SLDF), and \code{SpatialPointsDataFrames}
#' (SPtDf) objects containing OpenStreetMap polygons and lines for various
#' OpenStreetMap structures in a small part of central London,
#' U.K.  (\code{bbox = -0.15, 51.5, -0.1, 51.52}). The list includes:
#' \enumerate{
#'  \item \code{dat_H}: an SLDF of non-primary highways with 1.764 lines 
#'  \item \code{dat_HP}: an SLDF of primary highways with 378 lines 
#'  \item \code{dat_BNR}: an SPDF of non-residential buildings with 2,138 polygons 
#'  \item \code{dat_BR}: an SPDF of residential buildings with 40 polygons 
#'  \item \code{dat_BC}: an SPDF of commerical buildings with 17 polygons 
#'  \item \code{dat_A}: an SPDF of amenities with 442 polygons 
#'  \item \code{dat_G}: an SPDF of grassed areas with 23 polygons 
#'  \item \code{dat_P}: an SPDF of parks with 24 polygons 
#'  \item \code{dat_N}: an SPDF of natural areas with 18 polygons 
#'  \item \code{dat_T}: an SPtDF of trees with 1,310 points 
#'  \item \code{dat_RFH}: an SPDF containing 1 polygon representing Royal
#'      Festival Hall 
#'  \item \code{dat_ST}: an SPDF containing 1 polygon representing
#'      150 Stamford Street
#'  \item \code{highways1}: A \code{SpatialPoints} object containing 55 points
#'      representing the circular perimeter of \code{c ('Monmouth.St',
#'      'Short.?s.Gardens', 'Endell.St', 'Long.Acre', 'Upper.Saint.Martin')}
#'  \item \code{highways2}: A \code{SpatialPoints} object containing 47 points
#'      representing the circular perimeter of 
#'      \code{c ('Endell.St', 'High.Holborn', 'Drury.Lane', 'Long.Acre')}
#'  \item \code{highways3}: A \code{SpatialPoints} object containing 55 points
#'      representing the circular perimeter of 
#'      \code{c ('Drury.Lane', 'High.Holborn', 'Kingsway', 'Great.Queen.St')}
#' }
#'
#' The vignette \code{basic-maps} details how these data were downloaded.
#'
#' @docType data
#' @keywords datasets
#' @name london
#' @format A list of spatial objects
NULL
