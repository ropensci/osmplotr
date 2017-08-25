#' extract_highways
#'
#' Extracts a list of named OpenStreetMap highways. OSM data are neither
#' structured nor ordered; this routine reduces data for each given highway to a
#' minimal number of discrete and sequentially ordered segments. These segments
#' may or may not connect, yet can be connected at their nearest points with
#' \code{get_highway_cycle}.
#'
#' @param highway_names A vector of highway names passed directly to the
#' Overpass API. Wildcards and whitespaces are `.'; for other options see
#' overpass help.
#' @param bbox the bounding box for the map.  A 2-by-2 matrix of 4 elements with
#' columns of min and max values, and rows of x and y values.  
#' @return A list of highways matching \code{highway_names} each element of
#' which is a list of distinct components for the given highway.
#' @return A \code{data.frame} of \code{sp} objects
#'
#' @noRd
extract_highways <- function (highway_names, bbox)
{
    if (missing (highway_names))
        stop ('A vector of highway names must be given')
    if (missing (bbox))
        stop ('A bounding box must be given')

    #----------Download OSM data for highways
    hw_abbrvs <- abbreviate_hwy_names (highway_names)
    dl_dat <- dl_hw_data (highway_names, hw_abbrvs, bbox)
    p4s <- dl_dat$p4s
    if (length (dl_dat$indx) < length (highway_names))
    {
        hw_abbrvs <- hw_abbrvs [dl_dat$indx]
        highway_names <- highway_names [dl_dat$indx]
    }


    #---------Extract coordinates
    ways <- list ()
    for (i in seq (highway_names))
    {
        wi <- lapply (get (hw_abbrvs [i])$geometry,
                      function (i) as.matrix (i))
        ways [[i]] <- order_lines (wi)
    }
    names (ways) <- hw_abbrvs

    attr (ways, "crs") <- p4s

    return (ways)
}

abbreviate_hwy_names <- function (highway_names, nletters = 2)
{
    hw_abbrvs <- sapply (highway_names, function (x)
                      tolower (substring (x, 1, nletters)))
    while (any (duplicated (hw_abbrvs)))
    {
        nletters <- nletters + 1
        hw_abbrvs <- sapply (highway_names, function (x)
                          tolower (substring (x, 1, nletters)))
    }

    return (hw_abbrvs)
}

dl_hw_data <- function (highway_names, hw_abbrvs, bbox)
{
    cat ('Downloading OSM data ...\n')
    p4s <- NULL
    lens_old <- length (highway_names)
    lens <- 0
    # in case download does not work, this will try again until same data are
    # returned twice in a row
    while (lens != lens_old)
    {
        indx <- NULL
        pb <- txtProgressBar (max = 1, style = 3)
        # style = 3 shows start and end positions
        for (i in seq (highway_names))
        {
            dat <- extract_highway (name = highway_names [i], bbox = bbox)
            if (!is.null (dat))
                if (nrow (dat) > 0)
                {
                    assign (hw_abbrvs [i], value = dat, envir = parent.frame ())
                    indx <- c (indx, i)
                }
            setTxtProgressBar(pb, i / length (highway_names))
        }
        lens <- rep (0, length (indx))
        for (i in seq (indx))
            lens [i] <- nrow (get (hw_abbrvs [indx] [i],
                                   envir = parent.frame ()))

        lens <- length (which (lens > 0)) # total number returning data
        if (lens > 0)
        {
            hw1 <- hw_abbrvs [indx] [which (lens > 0)] [1]
            p4s <- attr (get (hw1, envir = parent.frame ())$geometry,
                         'crs')$proj4string
        }
        rm (dat)
        close (pb)
        lens_old <- lens
    }
    if (lens == 0)
        stop ('No data able to be extracted')
    else if (lens < length (highway_names))
        message ('Unable to download all requested data.')

    list ('p4s' = p4s, 'indx' = indx)
}


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
#'
#' @noRd
extract_highway <- function (name = '', bbox)
{
    check_arg (name, 'name', 'character')
    bbox <- check_bbox_arg (bbox)

    qry <- osmdata::opq (bbox = bbox)
    qry <- osmdata::add_osm_feature (qry, key = 'highway')
    qry <- osmdata::add_osm_feature (qry, key = 'name', value = name,
                                 key_exact = FALSE, value_exact = FALSE,
                                 match_case = FALSE)

    osmdata::osmdata_sf (qry)$osm_lines
}
