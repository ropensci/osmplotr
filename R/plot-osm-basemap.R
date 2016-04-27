#' plot_osm_basemap
#'
#' Generates a base OSM plot ready for polygon, line, and point objects to be
#' overlain with add_osm_objects(). 
#'
#' @param bbox bounding box (Latitude-longitude range) to be plotted.  A 2-by-2
#' matrix of 4 elements with columns of min and max values, and rows of x and y
#' values.
#' @param structures Data frame returned by osm_structures() used here to
#' specify background colour of plot; if missing, the colour is specified by
#' 'bg'
#' @param bg Background colour of map (default = 'gray20' only if structs not
#' given)
#' @return ggplot object containing base map
#' @export
#'
#' @seealso \code{\link{add_osm_objects}}, \code{\link{make_osm_map}}.
#'
#' @examples
#' bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
#' map <- plot_osm_basemap (bbox=bbox, bg='gray20')
#' map <- add_osm_objects (map, london$dat_BNR, col='gray40') 
#' print (map)

plot_osm_basemap <- function (bbox, structures, bg='gray20')
{
    # ---------------  sanity checks and warnings  ---------------
    if (missing (bbox))
        stop ('bbox must be supplied')
    if (!is.numeric (bbox))
        stop ('bbox is not numeric')
    if (length (bbox) < 4)
        stop ('bbox must have length = 4')

    if (!missing (structures))
        bg = structure$cols [which (structures$structure == 'background')]
    if (!(is.character (bg) | is.numeric (bg)))
    {
        warning ('bg will be coerced to character')
        bg <- as.character (bg)
    }
    # ---------------  end sanity checks and warnings  ---------------

    # Because the initial plot has no data, setting these elements suffices to
    # generate a blank plot area with no margins
    new_theme <- ggplot2::theme_minimal ()
    new_theme$panel.background <- ggplot2::element_rect (fill = bg, size=0)
    new_theme$line <- ggplot2::element_blank ()
    new_theme$axis.text <- ggplot2::element_blank ()
    new_theme$axis.title <- ggplot2::element_blank ()
    new_theme$plot.margin <- ggplot2::margin (rep (ggplot2::unit (0, 'null'), 4))
    new_theme$plot.margin <- ggplot2::margin (rep (ggplot2::unit (-0.5, 'line'), 4))
    new_theme$legend.position <- 'none'
    new_theme$axis.ticks.length <- ggplot2::unit(0,'null')

    lon <- lat <- NA
    map <- ggplot2::ggplot () + new_theme +
                ggplot2::coord_cartesian (xlim=range (bbox[1,]), 
                                          ylim=range (bbox[2,])) +
                ggplot2::aes (x=lon, y=lat) +
                ggplot2::scale_x_continuous (expand=c(0, 0)) +
                ggplot2::scale_y_continuous (expand=c(0, 0))

    return (map)
}

