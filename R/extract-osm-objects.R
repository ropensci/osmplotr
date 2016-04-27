#' extract_osm_objects
#'
#' Downloads OSM XML objects and extracts 'sp' polygons or lines.  Requires
#' conversion to osmar object which can be quite slow, as can final conversion
#' to sp object for large numbers of objects
#'
#' @param key OSM key to search for. Useful keys include 'building',
#' 'waterway', 'natural', 'grass', 'park', 'amenity', 'shop', 'boundary', and
#' 'highway'. Others will be passed directly to the overpass API and may not
#' necessarily return results.
#' @param value OSM value to match to key. If NULL, all keys will be returned.
#' Negation is specified by '!value'.
#' @param extra_pairs A list of additional key-value pairs to be passed
#' to the overpass API.
#' @param bbox the bounding box within which all key-value objects should be
#' downloaded.  A 2-by-2 matrix of 4 elements with columns of min and
#' max values, and rows of x and y values.
#' @param verbose If TRUE, provides notification of progress
#'
#' @return A data frame of sp objects
#' @export
#'
#' @seealso \code{\link{add_osm_objects}}.
#'
#' @examples
#' \dontrun{
#' bbox <- get_bbox (c(-0.13,51.50,-0.11,51.52))
#' dat_B <- extract_osm_objects (key='building', bbox=bbox)
#' dat_H <- extract_osm_objects (key='highway', bbox=bbox)
#' dat_BR <- extract_osm_objects (key='building', value='residential', bbox=bbox)
#' dat_HP <- extract_osm_objects (key='highway', value='primary', bbox=bbox)
#' dat_HNP <- extract_osm_objects (key='highway', value='!primary', bbox=bbox)
#' extra_pairs <- c ('name', 'Royal.Festival.Hall')
#' dat <- extract_osm_objects (key='building', extra_pairs=extra_pairs, bbox=bbox)
#' }

extract_osm_objects <- function (key, value, extra_pairs, bbox, verbose=FALSE)
{
    if (missing (key))
        stop ('key must be provided')
    if (missing (bbox))
        stop ('bbox must be provided')
    stopifnot (is.numeric (bbox))
    stopifnot (length (bbox) == 4)

    if (key == 'park')
    {
        key <- 'leisure'
        value <- 'park'
    } else if (key == 'grass')
    {
        key <- 'landuse'
        value <- 'grass'
    } else if (key == 'tree')
    {
        key <- 'natural'
        value <- 'tree'
    }
    
    # Construct the overpass query, starting with main key-value pair and
    # possible negation
    keyold <- key
    if (!missing (value))
    {
        valold <- value
        if (substring (value, 1, 1) == '!')
            value <- paste0 ("['", key, "'!='", 
                            substring (value, 2, nchar (value)), "']")
        else if (key == 'name')
            value <- paste0 ("['", key, "'~'", value, "']")
        else
            value <- paste0 ("['", key, "'='", value, "']")
    } else
        value <- ''
    if (key == 'name')
        key <- ''
    else
        key <- paste0 ("['", key, "']")

    # Then any extra key-value pairs
    if (!missing (extra_pairs))
    {
        if (!is.list (extra_pairs))
            extra_pairs <- list (extra_pairs)
        ep <- NULL
        for (i in extra_pairs)
            ep <- paste0 (ep, "['", i [1], "'~'", i [2], "']")
        extra_pairs <- ep
    } else
        extra_pairs <- ''

    bbox <- paste0 ('(', bbox [2,1], ',', bbox [1,1], ',',
                    bbox [2,2], ',', bbox [1,2], ')')


    query <- paste0 ('(node', key, value, extra_pairs, bbox,
                    ';way', key, value, extra_pairs, bbox,
                    ';rel', key, value, extra_pairs, bbox, ';')
    url_base <- 'http://overpass-api.de/api/interpreter?data='
    query <- paste0 (url_base, query, ');(._;>;);out;')
    if (nchar (value) > 0)
        value <- valold
    else
        value <- NULL
    key <- keyold

    obj <- NULL

    if (verbose) message ('downloading OSM data ... ')
    dat <- httr::GET (query)
    if (dat$status_code != 200)
        warning (httr::http_status (dat)$message)
    # Encoding must be supplied in the following to suppress warning
    dat <- XML::xmlParse (httr::content (dat, 'text', encoding='UTF-8'))

    k <- v <- NULL # supress 'no visible binding' note from R CMD check
    if (verbose) message ('converting OSM data to omsar format')
    dato <- osmar::as_osmar (dat)
    # A very important NOTE: It can arise the OSM relations have IDs which
    # duplicate IDs in OSM ways, even through the two may bear no relationship
    # at all. This causes the attempt in `osmar::as_sp` to force them to an `sp`
    # object to crash because 
    # # Error in validObject(.Object) :
    # #   invalid class 'SpatialLines' object: non-unique Lines ID of slot values
    # The IDs are actually neither needed not used, so the next lines simply
    # modifies all relation IDs by pre-pending 'r' to avoid such problems:
    for (i in seq (dato$relations))
        if (nrow (dato$relations [[i]]) > 0)
            dato$relations [[i]]$id <- paste0 ('r', dato$relations [[i]]$id)
    if (nchar (key) > 0)
        pids <- osmar::find (dato, osmar::way (osmar::tags(k == key)))
    else if (!is.null (value))
        pids <- osmar::find (dato, osmar::way (osmar::tags(v == value)))
    
    # spts converts to SpatialPoints, currently only for trees but easily
    # extended
    spts <- FALSE
    if (!is.null (value))
        if (value == 'tree') # no pids needed
            spts <- TRUE

    if (verbose) message ('converting osmar data to sp format')
    if (spts)
        obj <- osmar::as_sp (dato, 'points')
    else
    {
        pids1 <- osmar::find_down (dato, osmar::way (pids))
        pids2 <- osmar::find_up (dato, osmar::way (pids))
        pids <- mapply (c, pids1, pids2, simplify=FALSE)
        pids <- lapply (pids, function (i) unique (i))
        nvalid <- sum (sapply (pids, length))
        if (nvalid <= 3) # (nodes, ways, relations)
            warning (paste0 ('No valid data for (', key, ', ', value, ')'))
        else
        {
            obj <- subset (dato, ids = pids)
            # TODO: Extract names of objects (at least for streets, buildings)

            if (key=='boundary' | key == 'highway' | key == 'waterway') 
                obj <- osmar::as_sp (obj, 'lines')
            else 
                obj <- osmar::as_sp (obj, 'polygons')
        }
    }

    return (obj)
}
