#' extract_highway
#'
#' Extracts an OpenStreetMap highway by name, within the given bounding box.
#'
#' @param name Name of highway. Lines components are return for *any* OSM way
#' with a partially-matched. Both wildcards and whitespace should be represented
#' by `.'. 
#' @param bbox the bounding box for the map.  A 2-by-2 matrix of 4 elements with
#' columns of min and max values, and rows of x and y values.  
#' @return A \code{SpatialLinesDataFrame} containing the highway.
extract_highway <- function (name='', bbox)
{
    stopifnot (nchar (name) > 0)

    if (missing (bbox))
        stop ('bbox must be provided')
    stopifnot (is.numeric (bbox))
    stopifnot (length (bbox) == 4)
    bbox <- paste0 ('(', bbox [2,1], ',', bbox [1,1], ',',
                    bbox [2,2], ',', bbox [1,2], ')')

    obj <- NULL

    if (!curl::has_internet ())
        stop ('Error: No internet connection')

    query <- paste0 ("way['name'~'", name, "']", bbox)
    query <- paste0 (query, ';(._;>;);out;')
    url_base <- 'http://overpass-api.de/api/interpreter?data='
    query <- paste0 (url_base, query)

    #dat <- RCurl::getURL (query)
    #dat <- XML::xmlParse (dat)
    #dat <- httr::GET (query, timeout=1)
    # httr::GET sometimes errors with 'Error in curl::curl_fetch_memory (url,
    #       handle=handle) : Timeout was reached'. The current tryCatch catches
    #       this error only.
    dat <- tryCatch (
        httr::GET (query, timeout=60),
        error=function (err) {
            message ('error in httr::GET - most likely Timeout')
            return (list (status_code=504))
        })
    count <- 1
    # code#429 = "Too Many Requests (RFC 6585)"
    # code#504 = "Gateway Timeout"
    codes <- c (429, 504)
    while (dat$status_code %in% codes && count < 10)
    {
        dat <- tryCatch (
            httr::GET (query, timeout=60),
            error=function (err) {
                message ('error in httr::GET - most likely Timeout')
                return (list (status_code=504))
            })
        count <- count + 1
    }

    if (dat$status_code != 200)
    {
        warning (paste0 (httr::http_status (dat)$message, ' (status ',
                        dat$status_code, ')'))
        return (NULL)
    }

    # Encoding must be supplied to suppress warning
    dat <- XML::xmlParse (httr::content (dat, "text", encoding='UTF-8'))
    dato <- osmar::as_osmar (dat)
    key <- 'highway'
    k <- NULL # supress 'no visible binding' note from R CMD check
    pids <- osmar::find (dato, osmar::way (osmar::tags(k == key)))
    pids <- osmar::find_down (dato, osmar::way (pids))
    nvalid <- sum (sapply (pids, length))
    obj <- NULL
    if (nvalid > 3) # (nodes, ways, relations)
        obj <- osmar::as_sp (subset (dato, ids = pids), 'lines')

    return (obj)
}
