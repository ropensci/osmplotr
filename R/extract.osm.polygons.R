#' extract.osm.polygons
#'
#' For the given vector of structure types returns a list of unambiguous
#' prefixes to be appended to the objects returned by get.osm.data ()
#'
#' @param dat = raw XML data returned from get.osm.data () for the given object
#' type (if NULL, raw data are downloaded here, for which bbox must be provided).
#' @param type: currently recognises "building", "waterway", "natural", "grass",
#' "park", "amenity", "shop", "boundary", and "highway". Others will be simply
#' passed to "way[type=*]", which may or may not work.
#' @return sp object containing all polygons within bbox of given type.

extract.osm.polygons <- function (dat=NULL, type="building", bbox=NULL)
{
    require (xml)

    if (is.null (dat) & is.null (bbox))
        stop ("bbox must be provided to download data")
    else if (is.null (dat))
        dat <- get.osm.data (type=type, bbox=bbox)

    datp <- xmlParse (dat)
    dato <- as_osmar (datp)
    if (type=="grass") {
        pids <- find (dato, way(tags(k == "landuse" & v == type)))
    } else if (type == "park") {
        pids <- find (dato, way(tags(v == type)))
    } else {
        pids <- find (dato, way(tags(k == type)))
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

