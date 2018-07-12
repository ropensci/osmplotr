#' osm_structures
#'
#' For the given vector of structure types returns a \code{data.frame}
#' containing two columns of corresponding OpenStreetMap \code{key-value} pairs,
#' one column of unambiguous suffixes to be appended to the objects returned by
#' \code{\link{extract_osm_objects}}, and one column specifying colours. This
#' \code{data.frame} may be subsequently modified as desired, and ultimately
#' passed to \code{\link{make_osm_map}} to automate map production.
#'
#' @param structures The vector of types of structures (defaults listed in
#' \code{\link{extract_osm_objects}}).  
#' @param col_scheme Colour scheme for the plot (current options include
#' \code{dark} and \code{light}).
#' @return \code{data.frame} of structures, \code{key-value} pairs,
#' corresponding prefixes, and colours.
#'
#' @export
#'
#' @seealso \code{\link{make_osm_map}}.
#'
#' @examples
#' # Default structures:
#' osm_structures ()
#' # user-defined structures:
#' structures <- c ('highway', 'park', 'ameniiy', 'tree')
#' structs <- osm_structures (structures = structures, col_scheme = 'light')
#' # make_osm_map returns potentially modified list of data
#' \dontrun{
#' dat <- make_osm_map (osm_data = london, structures = structs)
#' # map contains updated $osm_data and actual map in $map
#' print_osm_map (dat$map)
#' }
osm_structures <- function (structures = c ('building', 'amenity', 'waterway',
                         'grass', 'natural', 'park', 'highway', 'boundary',
                         'tree'), col_scheme = 'dark')
{
    kv <- get_key_vals (structures) # key-val pairs

    # Get suffixes for naming data objects
    indx_in <- which (!duplicated (structures))
    indx_out <- which (duplicated (structures))
    lettrs <- sapply (structures [indx_in], function (x)
                      toupper (substr (x, 1, 1)))
    lettrs <- unique_suffixes (lettrs, structures, indx_in)
    suffixes <- extend_suffixes (lettrs, structures, indx_in, indx_out)

    scheme_cols <- NULL
    if (col_scheme == 'dark')
        scheme_cols <- get_dark_cols ()
    else if (col_scheme == 'light')
        scheme_cols <- get_light_cols ()

    if (!is.null (scheme_cols))
        cols <- set_cols (scheme_cols, structures)

    # Then add row to designate background colour (this has to be done prior to
    # data.frame construction, because cols are converted there to factors):
    structures <- c (structures, 'background')
    kv$keys <- c (kv$keys, '')
    kv$values <- c (kv$values, '')
    suffixes <- c (suffixes, '')
    cols <- c (cols, scheme_cols$col_bg)

    dat <- data.frame (cbind (structures, kv$keys, kv$values, suffixes, cols),
                       stringsAsFactors = FALSE,
                       row.names = seq (length (kv$keys)))
    names (dat) <- c ('structure', 'key', 'value', 'suffix', 'cols')
    return (dat)
}

get_key_vals <- function (structures)
{
    keys <- structures
    values <- rep ('', length (keys))
    val_list <- c ('grass', 'park', 'tree', 'water')
    key_list <- c ('landuse', 'leisure', 'natural', 'natural')

    for (i in seq (val_list))
        if (any (structures == val_list [i]))
        {
            keys [structures == val_list [i]] <- key_list [i]
            values [structures == val_list [i]] <- val_list [i]
        }

    return (list ('keys' = keys, 'values' = values))
}

get_dark_cols <- function ()
{
    list (
          col_bg = 'gray20',
          col_green = rgb (100, 120, 100, 255, maxColorValue = 255),
          col_green_bright = rgb (100, 160, 100, 255, maxColorValue = 255),
          col_blue = rgb (100, 100, 120, 255, maxColorValue = 255),
          col_gray1 = rgb (100, 100, 100, 255, maxColorValue = 255),
          col_gray2 = rgb (120, 120, 120, 255, maxColorValue = 255),
          col_white = rgb (200, 200, 200, 255, maxColorValue = 255),
          col_black = rgb (0, 0, 0, 255, maxColorValue = 255)
          )
}

get_light_cols <- function ()
{
    list (
          col_bg = 'gray95',
          col_green = rgb (200, 220, 200, 255, maxColorValue = 255),
          col_green_bright = rgb (200, 255, 200, 255, maxColorValue = 255),
          col_blue = rgb (200, 200, 220, 255, maxColorValue = 255),
          col_gray1 = rgb (200, 200, 200, 255, maxColorValue = 255),
          col_gray2 = rgb (220, 220, 220, 255, maxColorValue = 255),
          col_white = rgb (255, 255, 255, 255, maxColorValue = 255),
          col_black = rgb (150, 150, 150, 255, maxColorValue = 255)
          )
}

set_cols <- function (col_scheme, structures)
{
    cols <- rep (col_scheme$col_bg, length (structures))
    cols [structures == 'building'] <- col_scheme$col_gray1
    cols [structures == 'amenity'] <- col_scheme$col_gray2
    cols [structures == 'waterway'] <- col_scheme$col_blue
    cols [structures == 'natural'] <- col_scheme$col_green
    cols [structures == 'park'] <- col_scheme$col_green
    cols [structures == 'tree'] <- col_scheme$col_green_bright
    cols [structures == 'grass'] <- col_scheme$col_green_bright
    cols [structures == 'highway'] <- col_scheme$col_black
    cols [structures == 'boundary'] <- col_scheme$col_white

    return (cols)
}

# Extend suffixes until sufficiently many letters are included for entries to
# become unique.
unique_suffixes <- function (sfx, structures, indx_in)
{
    matches <- sapply (sfx, function (x) which (sfx %in% x))
    nletts <- rep (2, length (matches))
    # This while loop will always stop because it is only applied to unique
    # values
    while (max (sapply (matches, length)) > 1)
    {
        matches_red <- list ()
        for (i in seq (matches))
            if (length (matches [[i]]) > 1 &
                !all (matches [[i]] %in% unlist (matches_red)))
                matches_red [[length (matches_red) + 1]] <- matches [[i]]
        for (i in seq (matches_red))
        {
            repls <- structures [indx_in] [matches_red [[i]] ]
            sfx [matches_red [[i]] ] <- toupper (substr (repls, 1,
                                                nletts [matches_red [[i]] ]))
            nletts [matches_red [[i]] ] <- nletts [matches_red [[i]] ] + 1
        }
        matches <- sapply (sfx, function (x) which (sfx %in% x))
    }

    return (sfx)
}

# Extend list of unique suffixes to the full structures with duplicates. This
# is a bit tricky, and is done by first creating an index of all duplicates:
extend_suffixes <- function (sfx, structures, indx_in, indx_out)
{
    indx <- which (duplicated (structures) |
                   duplicated (structures, fromLast = TRUE))
    # Then the values of that indx that are not in indx_out
    indx <- indx [!indx %in% indx_out]
    # And those two can be matched for the desired replacement
    suffixes <- rep (NULL, length (structures))
    suffixes [indx_in] <- sfx
    for (i in indx)
    {
        ii <- which (structures == structures [i])
        suffixes [ii] <- suffixes [i]
    }

    return (suffixes)
}
