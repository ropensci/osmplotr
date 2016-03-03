#' get_xylims
#'
#' Extracts lat-lon limits from data frames of spatial polygons or lines
#' returned by get_osm_polygons(), or from bounding boxes of form 
#' (xmin, ymin, xmax, ymax).
#'
#' @param obj A spatial polygon or line data.frame returned by
#' get_osm_objects().
#' @return list of lat-lon ranges
#' @export

get_xylims <- function (obj)
{
    if (class (obj) == 'numeric' & length (obj) == 4) # bounding box
        ranges <- array (obj, dim=c(2, 2))
    else if (class (obj) == 'SpatialPointsDataFrame')
    {
        ranges <- t (slot (obj, 'coords'))
    } else
    {
        if (class (obj) == 'SpatialPolygonsDataFrame')
            slNames <- c ('polygons', 'Polygons')
        else if (class (obj) == 'SpatialLinesDataFrame')
            slNames <- c ('lines', 'Lines')

        ranges <- sapply (slot (obj, slNames [1]),  function (x)
                      range (slot (slot (x, slNames [2]) [[1]], 'coords')))
    }
    list (xrange=range (ranges [1,]), yrange=range (ranges [2,]))
}
