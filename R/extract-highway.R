#' extract_highway
#'
#' Extracts an OpenStreetMap highway by name, within the given bounding box.
#'
#' @param name Name of highway. Lines components are return for *any* OSM way
#' with a partially-matched. Both wildcards and whitespace should be represented
#' by '.'. 
#' @param bbox the bounding box for the map.  A 2-by-2 matrix of 4 elements with
#' columns of min and max values, and rows of x and y values.  
#' @return SpatialLinesDataFrame containing the highway
#' @return SpatialLinesDataFrame containing the highway

extract_highway <- function (name='', bbox)
{
    stopifnot (nchar (name) > 0)

    if (missing (bbox))
        stop ('bbox must be provided')
    stopifnot (is.numeric (bbox))
    stopifnot (length (bbox) == 4)
    bbox <- paste0 ('(', bbox [2,1], ',', bbox [1,1], ',',
                    bbox [2,2], ',', bbox [1,2], ')')

    obj <- warn <- NULL

    query <- paste0 ("way['name'~'", name, "']", bbox)
    query <- paste0 (query, ';(._;>;);out;')
    url_base <- 'http://overpass-api.de/api/interpreter?data='
    query <- paste0 (url_base, query)
    #dat <- RCurl::getURL (query)
    #dat <- XML::xmlParse (dat)
    dat <- httr::GET (query)
    if (dat$status_code != 200)
        message (httr::http_status (dat)$message)
    # Encoding must be supplied to suppress warning
    dat <- XML::xmlParse (httr::content (dat, "text", encoding='UTF-8'))
    dato <- osmar::as_osmar (dat)
    key <- 'highway'
    k <- NULL # supress 'no visible binding' note from R CMD check
    pids <- osmar::find (dato, osmar::way (osmar::tags(k == key)))
    pids <- osmar::find_down (dato, osmar::way (pids))
    nvalid <- sum (sapply (pids, length))
    obj <- NULL
    if (nvalid <= 3) # (nodes, ways, relations)
        warning (paste0 ('No valid data for name=(', name, ')'))
    else
        obj <- osmar::as_sp (subset (dato, ids = pids), 'lines')

    return (obj)
}
