check_map_arg <- function (map)
{
    if (missing (map))
        stop ('a non-null map must be supplied', call. = FALSE)
    if (!is (map, 'ggplot'))
        stop ('map must be a ggplot2 object', call. = FALSE)
}

get_objtxt <- function (obj)
{
    if (class (obj) == 'SpatialPolygonsDataFrame')
        objtxt <- c ('polygons', 'Polygons')
    else if (class (obj) == 'SpatialLinesDataFrame')
        objtxt <- c ('lines', 'Lines')
    else if (class (obj) == 'SpatialPointsDataFrame')
        objtxt <- c ('points', '')

    return (objtxt)
}

check_obj_arg <- function (obj)
{
    if (missing (obj))
        stop ('obj must be supplied', call. = FALSE)
    if (!is (obj, 'Spatial'))
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

#' generic function to check argument conversion to given function type
#'
#' @noRd
check_arg <- function (arg, arg_name, fn_type, na_okay = FALSE)
{
    if (missing (arg))
        stop (paste (arg_name, 'must be given'))
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
