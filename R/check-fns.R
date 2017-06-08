check_map_arg <- function (map)
{
    if (is.null (map))
        stop ('map must be supplied', call. = FALSE)
    if (!is (map, 'ggplot'))
        stop ('map must be a ggplot2 object', call. = FALSE)
}

check_obj_arg <- function (obj)
{
    if (is.null (obj))
        stop ('obj must be supplied to add_osm_groups', call. = FALSE)
    if (!is (obj, 'Spatial'))
        stop ('obj must be a spatial object', call. = FALSE)
}
