#' extract_highway
#'
#' Extracts an OpenStreetMap highway by name, within the given bounding box.
#'
#' @param name Name of highway. Lines components are return for *any* OSM way
#' with a partially-matched. Both wildcards and whitespace should be represented
#' by '.'. 
#' @param bbox the bounding box within which to look for highways.  Must be a
#' vector of 4 elements (xmin, ymin, xmax, ymax).  
#' @return SpatialLinesDataFrame containing the highway
#' @return SpatialLinesDataFrame containing the highway
#' @export

extract_highway <- function (name='', bbox=NULL)
{
    stopifnot (nchar (name) > 0)

    if (is.null (bbox))
        stop ('bbox must be provided')
    stopifnot (is.numeric (bbox))
    stopifnot (length (bbox) == 4)
    if (bbox [3] < bbox [1])
        bbox <- bbox [c (3, 2, 1, 4)]
    if (bbox [4] < bbox [2])
        bbox <- bbox [c (1, 4, 3, 2)]
    bbox <- paste0 ('(', bbox [2], ',', bbox [1], ',',
                   bbox[4], ',', bbox [3], ')')

    query <- paste0 ("way['name'~'", name, "']", bbox)
    query <- paste0 (query, ';(._;>;);out;')
    url_base <- 'http://overpass-api.de/api/interpreter?data='
    query <- paste0 (url_base, query)
    dat <- RCurl::getURL (query)
    dat <- XML::xmlParse (dat)
    dato <- osmar::as_osmar (dat)
    key <- 'highway'
    k <- NULL # supress 'no visible binding' note from R CMD check
    pids <- osmar::find (dato, osmar::way (osmar::tags(k == key)))
    pids <- osmar::find_down (dato, osmar::way (pids))
    nvalid <- sum (sapply (pids, length))
    if (nvalid <= 3) # (nodes, ways, relations)
    {
        warning ('No valid data for name=(', name, ')')
        return (NULL)
    } else
        return (osmar::as_sp (subset (dato, ids = pids), 'lines'))
}
