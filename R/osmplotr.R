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
