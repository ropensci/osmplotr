#' make.query
#'
#' Formats a string to be passed to an overpass API query. Query is presumed to
#' apply to ways by default.
#'
#' @param bbox = a numeric vector (length=4) of (min lon, min lat, max lon, max
#' lat)
#' @param key = an OSM key to extract (NULL returns all data)
#' @param value = an OSM value matched to the given key (NULL returns all keys
#' regardless of value)
#' @param node (binary) match OSM nodes as well
#' @param rel (binary) match OSM relations as well
#' @return formatted text string that can be simply passed to RCurl::getURL

make.query <- function (bbox, key=NULL, value=NULL,
                        node=FALSE, rel=FALSE)
{
    require (RCurl)
    stopifnot (is.numeric (bbox))
    stopifnot (length (bbox) == 4)

    url.base <- 'http://overpass-api.de/api/interpreter?data='

    if (!is.null (value))
        value <- paste ("=", value)

    bbox <- paste ("(", bbox [2], ",", bbox [1], ",",
                   bbox[4], ",", bbox [3], ")", sep="")

    query <- paste ("(way['", key, value, "']", bbox, ";", sep="")
    if (node)
        query <- paste (query, "node['", key, value, "']", bbox, ";", sep="")
    if (rel)
        query <- paste (query, "rel['", key, value, "']", bbox, ";", sep="")
    paste (url.base, query, ");(._;>;);out;", sep="")
}
