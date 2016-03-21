#' osmplotr.
#'
#' Produces customisable images of OpenStreetMap (OSM) data and enables data
#' visualisation using OSM objects.  Extracts data using the overpass API.
#' Contains the following functions, data, and vignettes.
#'
#' @section Data Functions:
#' \tabular{ll}{
#' 'extract_highway'\tab Download a single highway\cr
#' 'extract_highways'\tab Download a series of named highways\cr
#' 'extract_osm_objects'\tab Download arbitrary OSM objects\cr
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
#' 'click_map'\tab Return lat-lon coordinates of series of clicked points\cr 
#' 'connect_highways'\tab Connect lists of highways (used in 'highways2polygon')
#' \cr 
#' 'get_bbox'\tab return bounding box from input vector\cr 
#' 'highways2polygon'\tab Return a polygonal boundary circularly connecting a
#' given series of named highways \cr 
#' 'order_lines'\tab Spatially order highway lines in a given set of OSM data\cr 
#' }
#'
#' @section Data:
#' \tabular{ll}{
#' 'london'\tab OSM Data from a small portion of central London\cr 
#' }
#'
#' @section Vignettes:
#' \tabular{ll}{
#' 'downloading-data'\tab Describes how 'london' data were downloaded\cr 
#' 'making-maps'\tab Main package vignette
#' }
#'
#' @name osmplotr
#' @docType package
#' @import httr sp spatstat osmar XML ggm rgeos
#' @importFrom igraph graph_from_edgelist shortest_paths
#' @importFrom graphics polypath lines points locator plot.new par rect text
#' @importFrom graphics strwidth strheight
#' @importFrom methods slot as
#' @importFrom grDevices dev.new dev.list dev.off dev.cur rainbow rgb col2rgb png
#' @importFrom grDevices terrain.colors
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
