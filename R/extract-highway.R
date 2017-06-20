#' extract_highway
#'
#' Extracts an OpenStreetMap highway by name, within the given bounding box.
#'
#' @param name Name of highway. Lines components are return for *any* OSM way
#' with a partially-matched. Both wildcards and whitespace should be represented
#' by `.'. 
#' @param bbox the bounding box for the map.  A 2-by-2 matrix of 4 elements with
#' columns of min and max values, and rows of x and y values.  
#' @return A \code{SpatialLinesDataFrame} containing the highway.
#'
#' @noRd
extract_highway <- function (name = '', bbox)
{
    check_arg (name, 'name', 'character')
    bbox <- check_bbox_arg (bbox)

    qry <- osmdata::opq (bbox = bbox)
    qry <- osmdata::add_feature (qry, key = 'highway')
    qry <- osmdata::add_feature (qry, key = 'name', value = name,
                                 key_exact = FALSE, value_exact = FALSE,
                                 match_case = FALSE)

    osmdata::osmdata_sp (qry)$osm_lines
}
