#' get.xylims
#'
#' Extracts lat-lon limits from data frames of spatial polygons or lines
#' returned by get.osm.polygons()
#'
#' @param obj a spatial polygon or line data.frame returned by get.osm.objects ()
#' @return list of lat-lon ranges

get.xylims <- function (obj)
{
    if (class (obj) == "SpatialPolygonsDataFrame")
        slNames <- c ("polygons", "Polygons")
    else if (class (obj) == "SpatialLinesDataFrame")
        slNames <- c ("lines", "Lines")

    ranges <- sapply (slot (obj, slNames [1]),  function (x)
                  range (slot (slot (x, slNames [2]) [[1]], "coords")))
    list (xrange=range (ranges [1,]), yrange=range (ranges [2,]))
}
