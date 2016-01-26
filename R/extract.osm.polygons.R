#' extract.osm.polygons
#'
#' Extracts "sp" polygons from an OSM XML object. Requires conversion to osmar
#' object which can be quite slow, as can final conversion to sp object for
#' large numbers of polygons.
#'
#' @param dat = raw XML data returned from get.osm.data () for the given object
#' key (if NULL, raw data are downloaded here, for which bbox must be provided).
#' @param key: currently recognises "building", "waterway", "natural", "grass",
#' "park", "amenity", "shop", "boundary", and "highway". Others will be simply
#' passed to "way[key=value]", which may or may not work.
#' @return sp object containing all polygons within bbox of given key.

extract.osm.polygons <- function (dat=NULL, key="building", value=NULL, bbox=NULL)
{
    if (is.null (dat) & is.null (bbox))
        stop ("bbox must be provided to download data")
    else if (is.null (dat))
        dat <- get.osm.data (key=key, value=value, bbox=bbox)

    dato <- osmar::as_osmar (dat)
    if (key=="grass") {
        pids <- osmar::find (dato, osmar::way (osmar::tags(k == "landuse" & v == key)))
    } else if (key == "park") {
        pids <- osmar::find (dato, osmar::way (osmar::tags(v == key)))
    } else {
        pids <- osmar::find (dato, osmar::way (osmar::tags(k == key)))
    }
    pids <- osmar::find_down (dato, osmar::way (pids))
    sp <- subset (dato, ids = pids)
    if (key=="boundary" | key == "highway") {
        sp <- osmar::as_sp (sp, "lines")
    } else {
        sp <- osmar::as_sp (sp, "polygons")
    }

    return (sp)
}

