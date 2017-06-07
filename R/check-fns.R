#' @noRd
check_map_arg <- function (map)
{
    if (!is (map, 'ggplot'))
        stop ('map must be a ggplot2 object', call. = FALSE)
}
