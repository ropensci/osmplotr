check_map_arg <- function (map)
{
    if (missing (map))
        stop ('a non-null map must be provided', call. = FALSE)
    if (!is (map, 'ggplot'))
        stop ('map must be a ggplot2 object', call. = FALSE)
}

#' get type of geometry object from either sf or sp objects
#'
#' @note \code{sf} objects return singular nouns ('polygon', 'point'), while
#' \code{sp} return plurals ('polygons', 'points')
#'
#' @noRd
get_obj_type <- function (obj)
{
    if (is (obj, 'sf'))
    {
        i <- which (grepl ('sfc_', class (obj$geometry)))
        obj_type <- tolower (strsplit (class (obj$geometry) [i],
                                       'sfc_') [[1]] [2])
    } else
    {
        obj_type <- tolower (strsplit (strsplit (class (obj),
                                                 'Spatial') [[1]] [2],
                                       'DataFrame') [[1]] [1])
    }

    return (obj_type)
}


#' capitalise first letter of word
#'
#' @note does same as stringi::stri_trans_totitle
#'
#' @noRd
cap_first <- function (x)
{
    paste0 (toupper (substring (x, 1, 1)), substring (x, 2, nchar (x)))
}

check_obj_arg <- function (obj)
{
    if (missing (obj))
        stop ('obj must be provided', call. = FALSE)
    if (!(is (obj, 'Spatial') | is (obj, 'sf')))
        stop ('obj must be a spatial object', call. = FALSE)
}

check_col_arg <- function (col)
{
    if (missing (col))
        stop ('a non-null col must be provided')

    # Note col2rbg (NA) = white
    tryCatch (
              col2rgb (col),
              error = function (e)
              {
                  e$message <-  paste0 ("Invalid colour: ", col)
                  stop (e)
              })
}

check_bbox_arg <- function (bbox)
{
    if (missing (bbox))
        stop ('bbox must be provided')
    if (is (bbox, 'sf')) # sf obj submitted to osm_basemap
    {
        if (is (bbox$geometry, "sfc_LINESTRING") |
            is (bbox$geometry, "sfc_POINT"))
            xy <- do.call (rbind, bbox$geometry)
        else if (is (bbox$geometry, "sfc_POLYGON"))
            xy <- do.call (rbind, lapply (bbox$geometry, function (i) i [[1]]))
        else if (is (bbox$geometry, "sfc_MULTIPOLYGON") |
                 is (bbox$geometry, "sfc_MULTILINESTRING"))
            xy <- do.call (rbind, lapply (bbox$geometry,
                                          function (i) i [[1]] [[1]]))
        bbox <- t (apply (xy, 2, range))
        rownames (bbox) <- c ("x", "y")
        colnames (bbox) <- c ("min", "max")
    }
    if (!is.numeric (bbox))
        stop ('bbox is not numeric')
    if (length (bbox) < 4)
        stop ('bbox must have length = 4')
    if (length (bbox) > 4)
    {
        warning ('bbox has length > 4; only first 4 elements will be used')
        bbox <- matrix (bbox [1:4], 2, 2)
    }

    return (bbox)
}

check_structures_arg <- function (structures)
{
    if (!missing (structures))
    {
        if (!is.data.frame (structures))
            stop ('structures must be a data frame')
        ns <- c ('structure', 'key', 'value', 'suffix', 'cols')
        if (!all (names (structures) == ns))
            stop ('structures not in recognised format')
    }
}

#' generic function to check argument conversion to given function type
#'
#' @noRd
check_arg <- function (arg, arg_name, fn_type, na_okay = FALSE)
{
    if (missing (arg))
        stop (paste (arg_name, 'must be provided'))
    else if (length (arg) == 0)
        stop (paste (arg_name, 'can not be NULL'))
    else if (!na_okay & is.na (arg))
        stop (paste (arg_name, 'can not be NA'))

    adj <- tryCatch (
                     do.call (paste0 ('as.', fn_type), list (arg)),
                     warning = function (w)
                     {
                         w$message <- paste (arg_name,
                                              'can not be coerced to',
                                              fn_type)
                     })

    invisible (adj)
}
