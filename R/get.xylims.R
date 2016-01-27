#' get.xylims
#'
#' Extracts lat-lon limits from a set of polygons returned by get.osm.polygons()
#'
#' @param poly a polygon object returned by get.osm.polygons ()
#' @return list of lat-lon ranges

get.xylims <- function (obj)
{
    if (class (obj) == "SpatialPolygonsDataFrame")
    {
        xrange <- range (sapply (slot (obj, "polygons"),  
                                 function (x) min (slot (x, "labpt") [1])))
        yrange <- range (sapply (slot (obj, "polygons"),  
                                 function (x) min (slot (x, "labpt") [2])))
    } else if (class (obj) == "SpatialLinesDataFrame")
    {
        # TODO: This does not work
        xrange <- range (sapply (slot (obj, "lines"),  
                                 function (x) min (slot (x, "labpt") [1])))
        yrange <- range (sapply (slot (obj, "lines"),  
                                 function (x) min (slot (x, "labpt") [2])))
    }
    return (list (x=xrange, y=yrange))
}
