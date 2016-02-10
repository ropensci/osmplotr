#' extract_osm_objects
#'
#' Downloads OSM XML objects and extracts \code{sp} polygons or lines.  Requires
#' conversion to osmar object which can be quite slow, as can final conversion
#' to sp object for large numbers of objects
#'
#' @param key OSM key to search for. Useful keys include "building",
#' "waterway", "natural", "grass", "park", "amenity", "shop", "boundary", and
#' "highway". Others will be passed directly to the overpass API and may not
#' necessarily return results.
#' @param value OSM value to match to key. If NULL, all keys will be returned.
#' @param bbox the bounding box within which all key-value objects should be
#' downloaded.  Must be a vector of 4 elements (xmin, ymin, xmax, ymax).
#' @return Data frame of either spatial polygons or spatial lines

extract_osm_objects <- function (key="building", value=NULL, bbox=NULL)
{
    stopifnot (is.numeric (bbox))
    stopifnot (length (bbox) == 4)
    if (bbox [3] < bbox [1])
        bbox <- bbox [c (3, 2, 1, 4)]
    if (bbox [4] < bbox [2])
        bbox <- bbox [c (1, 4, 3, 2)]

    # make_osm_map passes empty values as "" rather than NULL:
    if (!is.null (value))
        if (nchar (value) == 0)
            value <- NULL

    # Construct the overpass query
    valold <- value
    if (!is.null (value))
        value <- paste ("'='", value, sep="")

    bbox <- paste ("(", bbox [2], ",", bbox [1], ",",
                   bbox[4], ",", bbox [3], ")", sep="")

    query <- paste ("(way['", key, value, "']", bbox, 
                    ";node['", key, value, "']", bbox, 
                    ";rel['", key, value, "']", bbox, ";", sep="")
    url_base <- 'http://overpass-api.de/api/interpreter?data='
    query <- paste (url_base, query, ");(._;>;);out;", sep="")
    value <- valold

    dat <- RCurl::getURL (query)
    dat <- XML::xmlParse (dat)

    k <- v <- NULL # supress "no visible binding" note from R CMD check
    dato <- osmar::as_osmar (dat)
    if (!is.null (value) & !is.null (key))
        pids <- osmar::find (dato, osmar::way (osmar::tags(
                                                k == key & v == value)))
    else if (!is.null (key))
        pids <- osmar::find (dato, osmar::way (osmar::tags(k == key)))
    else if (!is.null (value))
        pids <- osmar::find (dato, osmar::way (osmar::tags(v == value)))
    else 
        stop ("key-value missing")
    
    # spts converts to SpatialPoints, currently only for trees but easily
    # extended
    spts <- FALSE
    if (!is.null (value))
        if (value == "tree") # no pids needed
            spts <- TRUE

    if (spts)
        sp <- osmar::as_sp (dato, "points")
    else
    {
        pids <- osmar::find_down (dato, osmar::way (pids))
        nvalid <- sum (sapply (pids, length))
        if (nvalid <= 3) # (nodes, ways, relations)
        {
            warning ("No valid data for (", key, ", ", value, ")")
            sp <- NULL
        } else
        {
            sp <- subset (dato, ids = pids)
            # TODO: Extract names of objects (at least for streets, buildings)

            if (key=="boundary" | key == "highway" | key == "waterway") 
                sp <- osmar::as_sp (sp, "lines")
            else 
                sp <- osmar::as_sp (sp, "polygons")
        }
    }

    return (sp)
}
