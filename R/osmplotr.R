#' osmplotr.
#'
#' Produces customisable images of OpenStreetMap (OSM) data and enables data
#' visualisation using OSM objects.  Extracts data using the overpass API.
#' Contains the following functions, data, and vignettes.
#'
#' @section Data Functions:
#' \tabular{ll}{
#' 'extract_osm_objects'\tab Download arbitrary OSM objects\cr
#' 'connect_highways'\tab Returns points sequentially connecting list of named
#' highways\cr
#' }
#'
#' @section Basic Plotting Functions (without data):
#' \tabular{ll}{
#' 'add_axes'\tab Overlay longitudinal and latitudinal axes on plot\cr 
#' 'add_colourbar'\tab Overlay a scaled colourbar for data added with
#' 'add_osm_surface'\cr 
#' 'add_osm_objects'\tab Overlay arbitrary OSM objects\cr 
#' 'make_osm_map'\tab Automate map production with structures defined in
#' 'osm_structures'\cr 
#' 'osm_structures'\tab Define structures and graphics schemes for automating
#' map production \cr 
#' 'plot_osm_basemap'\tab Initiate a plotting device for an OSM map\cr 
#' } 
#'
#' @section Advanced Plotting Functions (with data):
#' \tabular{ll}{
#' 'add_osm_groups'\tab Overlay groups of objects using specified colour
#' scheme\cr 
#' 'add_osm_surface'\tab Overlay data surface by interpolating given data \cr 
#' }
#'
#' @section Colour Manipulation Functions:
#' \tabular{ll}{
#' 'adjust_colours'\tab Lighted or darken given colours by specified amount\cr 
#' 'colour_mat'\tab Generate continuous 2D spatial matrix of colours\cr 
#' }
#'
#' @section Other Functions:
#' \tabular{ll}{
#' 'get_bbox'\tab return bounding box from input vector\cr 
#' }
#'
#' @section Data:
#' \tabular{ll}{
#' 'london'\tab OSM Data from a small portion of central London\cr 
#' }
#'
#' @section Vignettes:
#' \tabular{ll}{
#' 'making-maps'\tab Describes basics of downloading data and making custom
#' maps\cr
#' 'making-maps-with-data'\tab 
#' Describes how map elements can be coloured
#' according to user-provided data, whether 
#' categorical or continuous.
#' }
#'
#' @name osmplotr
#' @docType package
#' @import httr sp spatstat osmar XML ggm rgeos
#' @importFrom igraph graph_from_edgelist shortest_paths
#' @importFrom graphics lines locator plot.new par rect text
#' @importFrom methods slot as is
#' @importFrom grDevices dev.new dev.list rainbow rgb col2rgb png
#' @importFrom grDevices terrain.colors heat.colors
#' @importFrom utils txtProgressBar setTxtProgressBar combn tail
#' @importFrom stats runif
NULL

#' london 
#'
#' A list of SpatialPolygonsDataFrames (SPDF), SpatialLinesDataFrames (SLDF),
#' and SpatialPointsDataFrames (SPtDf) containing OpenStreetMap polygons and
#' lines for various OpenStreetMap structures in a small part of central London,
#' U.K.  (bbox = -0.15, 51.5, -0.1, 51.52). The list includes:
#' \enumerate{
#'  \item dat_H an SLDF of non-primary highways with 1.764 lines 
#'  \item dat_HP an SLDF of primary highways with 378 lines 
#'  \item dat_BNR an SPDF of non-residential buildings with 2,138 polygons 
#'  \item dat_BR an SPDF of residential buildings with 40 polygons 
#'  \item dat_BC an SPDF of commerical buildings with 17 polygons 
#'  \item dat_A an SPDF of amenities with 442 polygons 
#'  \item dat_G an SPDF of grassed areas with 23 polygons 
#'  \item dat_P an SPDF of parks with 24 polygons 
#'  \item dat_N an SPDF of natural areas with 18 polygons 
#'  \item dat_T an SPtDF of trees with 1,310 points 
#'  \item dat_RFH an SPDF containing 1 polygon representing Royal Festival Hall
#'  \item dat_ST an SPDF containing 1 polygon representing 150 Stamford Street
#'  \item highways1 A SpatialPoints object containing 55 points representing
#'  the circular perimeter of c ('Monmouth.St', 'Short.?s.Gardens', 'Endell.St', 
#' 'Long.Acre', 'Upper.Saint.Martin')
#'  \item highways2 A SpatialPoints object containing 47 points representing
#'  the circular perimeter of 
#' c ('Endell.St', 'High.Holborn', 'Drury.Lane', 'Long.Acre')
#'  \item highways3 A SpatialPoints object containing 55 points representing
#'  the circular perimeter of 
#' c ('Drury.Lane', 'High.Holborn', 'Kingsway', 'Great.Queen.St')
#' }
#'
#' The vignette 'making-maps' demonstrates how these data were downloaded.
#'
#' @docType data
#' @keywords datasets
#' @name london
#' @format A list of spatial objects
NULL
