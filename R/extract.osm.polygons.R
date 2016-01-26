#' extract.osm.polygons
#'
#' Extracts "sp" polygons from an OSM XML object. Requires conversion to osmar
#' object which can be quite slow, as can final conversion to sp object for
#' large numbers of polygons.
#'
#' @param dat = raw XML data returned from get.osm.data () for the given object
#' type (if NULL, raw data are downloaded here, for which bbox must be provided).
#' @param type: currently recognises "building", "waterway", "natural", "grass",
#' "park", "amenity", "shop", "boundary", and "highway". Others will be simply
#' passed to "way[type=*]", which may or may not work.
#' @return sp object containing all polygons within bbox of given type.

extract.osm.polygons <- function (dat=NULL, key="building", value=NULL, bbox=NULL)
{
    require (xml)

    if (is.null (dat) & is.null (bbox))
        stop ("bbox must be provided to download data")
    else if (is.null (dat))
        dat <- get.osm.data (key=key, value=value, bbox=bbox)

    dato <- as_osmar (dat)
    if (type=="grass") {
        pids <- find (dato, way(tags(k == "landuse" & v == key)))
    } else if (type == "park") {
        pids <- find (dato, way(tags(v == key)))
    } else {
        pids <- find (dato, way(tags(k == key)))
    }
    pids <- find_down (dato, way (pids))
    sp <- subset (dato, ids = pids)
    if (type=="boundary" | type == "highway") {
        sp <- as_sp (sp, "lines")
    } else {
        sp <- as_sp (sp, "polygons")
    }

    return (sp)
}

