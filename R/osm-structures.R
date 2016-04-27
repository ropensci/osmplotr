#' osm_structures
#'
#' For the given vector of structure types returns a 'data.frame' containing two
#' columns of corresponding OpenStreetMap key-value pairs, one column of
#' unambiguous suffices to be appended to the objects returned by
#' get_osm_data(), and one column specifying colours. This data frame may be
#' subsequently modified as desired, and ultimately passed to make_osm_map() to
#' automate map production.
#'
#' @param structures The vector of types of structures (defaults listed in
#' '?extract_osm_objects').  
#' @param col_scheme Colour scheme for the plot (current options include 'dark'
#' and 'light') 
#' @return 'data.frame' of structures, key-value pairs, corresponding prefixes,
#' and colours.
#'
#' @export
#'
#' @seealso \code{\link{make_osm_map}}.
#'
#' @examples
#' # Default structures:
#' osm_structures ()
#' # user-defined structures:
#' structures <- c ('highway', 'park', 'grass')
#' structs <- osm_structures (structures=structures, col_scheme='light')
#' # make_osm_map returns potentially modified list of data
#' dat <- make_osm_map (osm_data=london, structures=structs)
#' # map contains updated $osm_data and actual map in $map
#' print (dat$map)

osm_structures <- function (structures = c ('building', 'amenity', 'waterway',
                         'grass', 'natural', 'park', 'highway', 'boundary',
                         'tree'), col_scheme='dark')
{
    # Set up key-value pairs:
    keys <- structures
    values <- rep ('', length (keys))
    if (any (structures == 'grass'))
    {
        keys [structures == 'grass'] <- 'landuse'
        values [structures == 'grass'] <- 'grass'
    }
    if (any (structures == 'park'))
    {
        keys [structures == 'park'] <- 'leisure'
        values [structures == 'park'] <- 'park'
    }
    if (any (structures == 'tree'))
    {
        keys [structures == 'tree'] <- 'natural'
        values [structures == 'tree'] <- 'tree'
    }
    if (any (structures == 'water'))
    {
        keys [structures == 'water'] <- 'natural'
        values [structures == 'water'] <- 'water'
    }

    # Get suffixes for naming data objects, extending suffixes until
    # sufficiently many letters are included for entries to become unique.
    indx_in <- which (!duplicated (structures))
    indx_out <- which (duplicated (structures))
    lettrs <- sapply (structures [indx_in], function (x) 
                      toupper (substr (x, 1, 1)))
    matches <- sapply (lettrs, function (x) which (lettrs %in% x))
    # matches returns a list of matches for each letter
    nletts <- rep (2, length (matches))
    # This while loop will always stop because it is only applied to unique values
    while (max (sapply (matches, length)) > 1)
    {
        # The list of matches is then reduced to unique values. This is done
        # with a loop, because it enables the list of matches to be shortened to
        # only those containing unique values.
        matches_red <- list ()
        for (i in seq (matches))
            if (length (matches [[i]]) > 1 & 
                !all (matches [[i]] %in% unlist (matches_red)))
                matches_red [[length (matches_red) + 1]] <- matches [[i]]
        for (i in seq (matches_red))
        {
            repls <- structures [indx_in] [matches_red [[i]] ]
            lettrs [matches_red [[i]] ] <- toupper (substr (repls, 1, 
                                                nletts [matches_red [[i]] ]))
            nletts [matches_red [[i]] ] <- nletts [matches_red [[i]] ] + 1
        }
        matches <- sapply (lettrs, function (x) which (lettrs %in% x))
    }
    # lettrs then has unique suffixes for all unique (structures). These values
    # then have be extended to the full structures with duplicates. This is a
    # bit tricky, and is done by first creating an index of all duplicates:
    indx <- which (duplicated (structures) | 
                   duplicated (structures, fromLast=TRUE))
    # Then the values of that indx that are not in indx_out
    indx <- indx [!indx %in% indx_out]
    # And those two can be matched for the desired replacement
    suffixes <- rep (NULL, length (structures))
    suffixes [indx_in] <- lettrs
    for (i in indx)
    {
        ii <- which (structures == structures [i])
        suffixes [ii] <- suffixes [i]
    }

    # Color scheme:
    if (col_scheme == 'dark')
    {
        col_bg <- 'gray20'
        col_green <- rgb (100, 120, 100, 255, maxColorValue=255)
        col_green_bright <- rgb (100, 160, 100, 255, maxColorValue=255)
        col_blue <- rgb (100, 100, 120, 255, maxColorValue=255)
        col_gray1 <- rgb (100, 100, 100, 255, maxColorValue=255)
        col_gray2 <- rgb (120, 120, 120, 255, maxColorValue=255)
        col_white <- rgb (200, 200, 200, 255, maxColorValue=255)
        col_black <- rgb (0, 0, 0, 255, maxColorValue=255)
        cols <- rep (col_bg, length (structures))
        cols [structures == 'building'] <- col_gray1
        cols [structures == 'amenity'] <- col_gray2
        cols [structures == 'waterway'] <- col_blue
        cols [structures == 'natural'] <- col_green
        cols [structures == 'park'] <- col_green
        cols [structures == 'tree'] <- col_green_bright
        cols [structures == 'grass'] <- col_green_bright
        cols [structures == 'highway'] <- col_black
        cols [structures == 'boundary'] <- col_white
    } else if (col_scheme == 'light')
    {
        col_bg <- 'gray95'
        col_green <- rgb (200, 220, 200, 255, maxColorValue=255)
        col_green_bright <- rgb (200, 255, 200, 255, maxColorValue=255)
        col_blue <- rgb (200, 200, 220, 255, maxColorValue=255)
        col_gray1 <- rgb (200, 200, 200, 255, maxColorValue=255)
        col_gray2 <- rgb (220, 220, 220, 255, maxColorValue=255)
        col_white <- rgb (255, 255, 255, 255, maxColorValue=255)
        col_black <- rgb (150, 150, 150, 255, maxColorValue=255)
        cols <- rep (col_bg, length (structures))
        cols [structures == 'building'] <- col_gray1
        cols [structures == 'amenity'] <- col_gray2
        cols [structures == 'waterway'] <- col_blue
        cols [structures == 'natural'] <- col_green
        cols [structures == 'park'] <- col_green
        cols [structures == 'tree'] <- col_green_bright
        cols [structures == 'grass'] <- col_green_bright
        cols [structures == 'highway'] <- col_black
        cols [structures == 'boundary'] <- col_white
    }
    # Then add row to designate background colour (this has to be done prior to
    # data.frame construction, because cols are converted there to factors):
    structures <- c (structures, 'background')
    keys <- c (keys, '')
    values <- c (values, '')
    suffixes <- c (suffixes, '')
    cols <- c (cols, col_bg)
    
    dat <- data.frame (cbind (structures, keys, values, suffixes, cols),
                       stringsAsFactors=FALSE, row.names=seq (length (keys)))
    names (dat) <- c ('structure', 'key', 'value', 'suffix', 'cols')
    return (dat)
}

