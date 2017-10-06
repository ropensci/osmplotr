#' osm_basemap
#'
#' Generates a base OSM plot ready for polygon, line, and point objects to be
#' overlain with \code{\link{add_osm_objects}}. 
#'
#' @param bbox bounding box (Latitude-longitude range) to be plotted.  A 2-by-2
#' matrix of 4 elements with columns of min and max values, and rows of x and y
#' values. Can also be an object of class \code{sf}, for example as returned
#' from \code{extract_osm_objects} or the \code{osmdata} package, in which case
#' the bounding box will be extracted from the object coordinates.
#' @param structures Data frame returned by \code{\link{osm_structures}} used
#' here to specify background colour of plot; if missing, the colour is
#' specified by \code{bg}.
#' @param bg Background colour of map (default = \code{gray20}) only if
#' \code{structs} not given).
#' @return A \code{ggplot2} object containing the base \code{map}.
#' @importFrom ggplot2 ggplot coord_map aes scale_x_continuous
#' scale_y_continuous theme_minimal element_rect element_blank margin unit
#' @importFrom mapproj mapproject
#' @export
#'
#' @seealso \code{\link{add_osm_objects}}, \code{\link{make_osm_map}}.
#'
#' @examples
#' bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
#' map <- osm_basemap (bbox = bbox, bg = 'gray20')
#' map <- add_osm_objects (map, london$dat_BNR, col = 'gray40') 
#' print_osm_map (map)
osm_basemap <- function (bbox, structures, bg = 'gray20')
{
    # ---------------  sanity checks and warnings  ---------------
    bbox <- check_bbox_arg (bbox)
    if (!missing (structures))
    {
        check_structures_arg (structures)
        bg <- structure$cols [which (structures$structure == 'background')]
    }
    check_col_arg (bg)
    if (length (bg) > 1)
    {
        warning ('bg has length > 1; only first element will be used')
        bg <- bg [1]
    }
    # ---------------  end sanity checks and warnings  ---------------

    map_theme <- set_map_theme (bg = bg)

    lon <- lat <- NA
    map <- ggplot2::ggplot () + map_theme +
                ggplot2::coord_map (xlim = range (bbox[1, ]),
                                    ylim = range (bbox[2, ])) +
                ggplot2::aes (x = lon, y = lat) +
                ggplot2::scale_x_continuous (expand = c(0, 0)) +
                ggplot2::scale_y_continuous (expand = c(0, 0))

    return (map)
}

set_map_theme <- function (bg)
{
    theme <- ggplot2::theme_minimal ()
    theme$panel.background <- ggplot2::element_rect (fill = bg, size = 0)
    theme$line <- ggplot2::element_blank ()
    theme$axis.text <- ggplot2::element_blank ()
    theme$axis.title <- ggplot2::element_blank ()
    theme$plot.margin <- ggplot2::margin (rep (ggplot2::unit (0, 'null'), 4))
    theme$plot.margin <- ggplot2::margin (rep (ggplot2::unit (-0.5, 'line'), 4))
    theme$legend.position <- 'none'
    theme$axis.ticks.length <- ggplot2::unit (0, 'null')

    return (theme)
}
