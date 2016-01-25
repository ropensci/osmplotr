#' get.suffixes
#'
#' For the given vector of structure types returns a list of unambiguous
#' suffices to be appended to the objects returned by get.osm.data ()
#'
#' @param dat.types the vector of types of structures (defaults listed in
#' ?get.osm.data).
#' @return List of structures and corresponding prefixes.

get.suffixes <- function (dat.types = c ("building", "amenity", "waterway", 
                         "grass", "natural", "park", "highway", "boundary"))
{
    letters <- sapply (dat.types, function (x) toupper (substr (x, 1, 1)))
    # matches returns a list of matches for each letter, with the subsequent
    # indx marking any replications.
    matches <- sapply (letters, function (x) which (letters %in% x))
    indx <- which (sapply (matches, function (x) length (x) > 1))
    if (length (indx) > 0) 
    {
        for (i in 1:length (indx)) 
        {
            indx.i <- matches [[indx [i]]] # Only need to extract first element
            letters [indx.i] <- toupper (substr (dat.types [indx.i], 1, 2))
        }
    }
    dat <- data.frame (cbind (dat.types, letters))
    row.names (dat) <- NULL
    names (dat) <- c ("dat.types", "letters")
    return (dat)
}

