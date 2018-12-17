#' osmplotr.
#'
#' Produces customisable images of OpenStreetMap (OSM) data and enables data
#' visualisation using OSM objects.  Extracts data using the overpass API.
#' Contains the following functions, data, and vignettes.
#'
#' @section Data Functions:
#' \itemize{
#' \item \code{\link{extract_osm_objects}}: Download arbitrary OSM objects
#' \item \code{\link{connect_highways}}: Returns points sequentially connecting
#' list of named highways
#' }
#'
#' @section Basic Plotting Functions (without data):
#' \itemize{
#' \item \code{\link{add_axes}}: Overlay longitudinal and latitudinal axes on plot
#' \item \code{\link{add_osm_objects}}: Overlay arbitrary OSM objects  
#' \item \code{\link{make_osm_map}}: Automate map production with structures
#' defined in \code{\link{osm_structures}}  
#' \item \code{\link{osm_structures}}: Define structures and graphics schemes for
#' automating map production  
#' \item \code{\link{osm_basemap}}: Initiate a \code{ggplot2} object for an OSM
#' map  
#' \item \code{\link{print_osm_map}}: Print a map to specified graphics
#' device 
#' } 
#'
#' @section Advanced Plotting Functions (with data):
#' \itemize{
#' \item \code{\link{add_osm_groups}}: Overlay groups of objects using specified
#' colour scheme  
#' \item \code{\link{add_osm_surface}}: Overlay data surface by interpolating
#' given data  
#' \item \code{\link{add_colourbar}}: Overlay a scaled colourbar for data added
#' with \code{\link{add_osm_surface}}  
#' }
#'
#' @section Colour Manipulation Functions:
#' \itemize{
#' \item \code{\link{adjust_colours}}: Lighted or darken given colours by
#' specified amount  
#' \item \code{\link{colour_mat}}: Generate continuous 2D spatial matrix of
#' colours  
#' }
#'
#' @section Other Functions:
#' \itemize{
#' \item \code{\link{get_bbox}}: return bounding box from input vector  
#' }
#'
#' @section Data:
#' \itemize{
#' \item \code{\link{london}}: OSM Data from a small portion of central London  
#' }
#'
#' @section Vignettes:
#' \itemize{
#' \item \code{basic-maps}: Describes basics of downloading data and making
#' custom maps 
#' \item \code{data-maps}: Describes how map elements can be coloured according
#' to user-provided data, whether categorical or continuous 
#' }
#'
#' @name osmplotr
#' @docType package
#' @importFrom curl has_internet
#' @importFrom e1071 allShortestPaths extractPath
#' @importFrom ggm fundCycles
#' @importFrom graphics lines plot plot.new par rect text
#' @importFrom grDevices dev.cur dev.new dev.off rainbow rgb col2rgb heat.colors
#' @importFrom grDevices bmp jpeg pictex png postscript svg tiff 
#' @importFrom httr content GET
#' @importFrom methods slot as is hasArg
#' @importFrom osmdata add_osm_feature opq osmdata_sf osmdata_sp
#' @importFrom rgeos gIntersection
#' @importFrom sp coordinates point.in.polygon Line SpatialLines SpatialPoints 
#' @importFrom spatstat convexhull idw ppp Smooth
#' @importFrom stats runif
#' @importFrom utils combn head setTxtProgressBar tail txtProgressBar
NULL

#' london 
#'
#' A list of \code{Simple Features} (\code{sf}) \code{data.frame} objects
#' containing OpenStreetMap polygons, lines, and points for various
#' OpenStreetMap structures in a small part of central London, U.K.  (\code{bbox
#' = -0.13, 51.51, -0.11, 51.52}). The list includes:
#' \enumerate{
#'  \item \code{dat_H}: 974 non-primary highways as linestrings
#'  \item \code{dat_HP}: 159 primary highways as linestrings
#'  \item \code{dat_BNR}: 1,716 non-residential buildings as polygons
#'  \item \code{dat_BR}: 43 residential buildings as polygons
#'  \item \code{dat_BC}: 67 commerical buildings as polygons
#'  \item \code{dat_A}: 372 amenities as polygons
#'  \item \code{dat_P}: 13 parks as polygons
#'  \item \code{dat_T}: 688 trees as points
#'  \item \code{dat_RFH}: 1 polygon representing Royal Festival Hall 
#'  \item \code{dat_ST}: 1 polygon representing 150 Stamford Street
#' }
#'
#' The vignette \code{basic-maps} details how these data were downloaded. Note
#' that these internal versions have had all descriptive data removed other than
#' their names, geometries, and their OSM identification numbers.
#'
#' @docType data
#' @keywords datasets
#' @name london
#' @format A list of spatial objects
NULL
