#' extract_highway
#'
#' Extracts an OpenStreetMap highway by name, within the give bounding box.
#'
#' @param name = Name of highway. Lines components are return for *any* OSM way
#' with a partially-matched. Both wildcards and whitespace should be represented
#' by ".". 
#' @param bbox = the bounding box within which all key-value objects should be
#' downloaded. Default is a small part of central London.
#' @return SpatialLinesDataFrame containing the highway

extract_highway <- function (name="", bbox=NULL)
{
    stopifnot (nchar (name) > 0)

    if (is.null (bbox))
        stop ("bbox must be provided")
    stopifnot (is.numeric (bbox))
    stopifnot (length (bbox) == 4)
    if (bbox [3] < bbox [1])
        bbox <- bbox [c (3, 2, 1, 4)]
    if (bbox [4] < bbox [2])
        bbox <- bbox [c (1, 4, 3, 2)]
    bbox <- paste ("(", bbox [2], ",", bbox [1], ",",
                   bbox[4], ",", bbox [3], ")", sep="")

    query <- paste ("(way['name'~'", name, "']", bbox, sep="")
    query <- paste (query, ";>;);out;", sep="")
    url_base <- 'http://overpass-api.de/api/interpreter?data='
    query <- paste (url_base, query, sep="")
    dat <- RCurl::getURL (query)
    dat <- XML::xmlParse (dat)
    dato <- osmar::as_osmar (dat)
    key <- "highway"
    pids <- osmar::find (dato, osmar::way (osmar::tags(k == key)))
    pids <- osmar::find_down (dato, osmar::way (pids))
    nvalid <- sum (sapply (pids, length))
    if (nvalid <= 3) # (nodes, ways, relations)
    {
        warning ("No valid data for name=(", name, ")")
        return (NULL)
    } else
    {
        sp <- subset (dato, ids = pids)
        return (osmar::as_sp (sp, "lines"))
    }
}
