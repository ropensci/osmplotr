#' extract.osm.objects
#'
#' Downloads OSM XML objects and extracts "sp" polygons or lines.  Requires
#' conversion to osmar object which can be quite slow, as can final conversion
#' to sp object for large numbers of objects
#'
#' @param key: OSM key to search for. Useful keys include "building",
#' "waterway", "natural", "grass", "park", "amenity", "shop", "boundary", and
#' "highway". Others will be passed directly to the overpass API and may not
#' necessarily return results.
#' @param value: OSM value to match to key. If NULL, all keys will be returned.
#' @param bbox = the bounding box within which all key-value objects should be
#' downloaded. Default is a small part of central London.
#' @return Data frame of either spatial polygons or spatial lines

extract.osm.objects <- function (key="building", value=NULL,
                                 bbox=c(-0.15,51.5,-0.1,51.52))
{
    if (is.null (bbox))
        stop ("bbox must be provided")

    stopifnot (is.numeric (bbox))
    stopifnot (length (bbox) == 4)
    if (bbox [3] < bbox [1])
        bbox <- bbox [c (3, 2, 1, 4)]
    if (bbox [4] < bbox [2])
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
    } else if (key == "tree")
    {
        key <- "natural"
        value <- "tree"
    }

    
    # Then construct the actual overpass query
    valold <- value
    if (!is.null (value))
        value <- paste ("'='", value, sep="")

    bbox <- paste ("(", bbox [2], ",", bbox [1], ",",
                   bbox[4], ",", bbox [3], ")", sep="")

    query <- paste ("(way['", key, value, "']", bbox, 
                    ";node['", key, value, "']", bbox, 
                    ";rel['", key, value, "']", bbox, ";", sep="")
    url.base <- 'http://overpass-api.de/api/interpreter?data='
    query <- paste (url.base, query, ");(._;>;);out;", sep="")
    value <- valold

    dat <- RCurl::getURL (query)
    dat <- XML::xmlParse (dat)

    dato <- osmar::as_osmar (dat)
    if (value=="grass") 
        pids <- osmar::find (dato, osmar::way (osmar::tags(
                                                k == "landuse" & v == key)))
    else if (value == "park") 
        pids <- osmar::find (dato, osmar::way (osmar::tags(v == key)))
    else 
        pids <- osmar::find (dato, osmar::way (osmar::tags(k == key)))
    
    if (value == "tree") # no pids needed
        sp <- osmar::as_sp (dato, "points")
    else
    {
        pids <- osmar::find_down (dato, osmar::way (pids))
        sp <- subset (dato, ids = pids)
        # TODO: Extract names of objects (at least for streets, buildings)

        if (key=="boundary" | key == "highway" | key == "waterway") 
            sp <- osmar::as_sp (sp, "lines")
        else 
            sp <- osmar::as_sp (sp, "polygons")
    }

    return (sp)
}

