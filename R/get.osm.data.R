#' get.osm.data
#'
#' Downloads OSM data for selected object type from the MapQuest api.
#'
#' @param key: OSM key to search for. (Useful keys include "building",
#' "waterway", "natural", "grass", "park", "amenity", "shop", "boundary", and
#' "highway". Others will be simply passed to "way[type=*]", which may or may
#' not work.)
#' @param bbox = the bounding box within which all objects of the given type
#' should be downloaded. Default is central London (-0.15,51.5,-0.1,51.52).
#' @return The contents of

get.osm.data <- function (key="building", value=NULL, bbox=c(-0.15,51.5,-0.1,51.52))
{
    stopifnot (is.numeric (bbox))
    stopifnot (length (bbox) == 4)
    if (bbox [3] < bbox [1])
        bbox <- bbox [c (3, 2, 1, 4)]
    if (bbox [4] <- bbox [2])
        bbox <- bbox [c (1, 4, 3, 2)]

    if (key == "water")
    {
        key <- "natural"
        value <- "water"
    } else if (key == "grass")
    {
        key <- "landuse"
        value <- "grass"
    } else if (key == "park")
    {
        key <- "leisure"
        value <- "park"
    }
    
    dat <- RCurl::getURL (make.query (bbox, key=key, value=value))
    dat <- XML::xmlParse (dat)

    if (is.null (dat))
        warning ("No data downloaded.")

    return (dat)
}

