#' get_suffixes
#'
#' For the given vector of structure types returns a list of unambiguous
#' suffices to be appended to the objects returned by get_osm_data (). This
#' function is useful only for automating map production, as illustrated in
#' make.osm.map().
#'
#' @param dat_types the vector of types of structures (defaults listed in
#' ?get_osm_data).
#' @return List of structures and corresponding prefixes.

get_suffixes <- function (dat_types = c ("building", "amenity", "waterway", 
                         "grass", "natural", "park", "highway", "boundary"))
{
    lettrs <- sapply (dat_types, function (x) toupper (substr (x, 1, 1)))
    # matches returns a list of matches for each letter, with the subsequent
    # indx marking any replications.
    matches <- sapply (lettrs, function (x) which (lettrs %in% x))
    indx <- which (sapply (matches, function (x) length (x) > 1))
    if (length (indx) > 0) 
    {
        for (i in 1:length (indx)) 
        {
            indx_i <- matches [[indx [i]]] # Only need to extract first element
            lettrs [indx_i] <- toupper (substr (dat_types [indx_i], 1, 2))
        }
    }
    dat <- data.frame (cbind (dat_types, lettrs))
    row.names (dat) <- NULL
    names (dat) <- c ("dat_types", "letters")
    return (dat)
}

