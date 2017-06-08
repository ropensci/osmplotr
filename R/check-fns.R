check_map_arg <- function (map)
{
    if (is.null (map))
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
    if (is.null (obj))
        stop ('obj must be supplied', call. = FALSE)
    if (!is (obj, 'Spatial'))
        stop ('obj must be a spatial object', call. = FALSE)
}

check_col_arg <- function (col, null_okay = FALSE)
{
    if (!null_okay & is.null (col))
        stop ('col is NULL')

    # Note col2rbg (NA) = white
    tryCatch (
              col2rgb (col),
              error = function (e)
              {
                  e$message <-  paste0 ("Invalid colour: ", col)
                  stop (e)
              })
}
