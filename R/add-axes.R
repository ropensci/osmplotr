#' add_axes
#'
#' Adds axes to the internal region of an OSM plot.
#'
#' @param map A \code{ggplot2} object to which the axes are to be added.
#' @param colour Colour of axis (determines colour of all elements: lines,
#' ticks, and labels).
#' @param pos Positions of axes and labels relative to entire plot device.
#' @param alpha alpha value for semi-transparent background surrounding axes and
#' labels (lower values increase transparency).
#' @param fontsize Size of axis font (in \code{ggplot2} terms; default=3).
#' @param fontface Fontface for axis labels (1:4=plain,bold,italic,bold-italic).
#' @param fontfamily Family of axis font (for example, `\code{Times}').
#' @param ... Mechanism to allow many parameters to be passed with alternative
#' names (\code{color} for \code{colour} and \code{xyz} for \code{fontxyz}.
#' @return Modified version of \code{map} with axes added.
#' @importFrom ggplot2 aes geom_polygon geom_segment geom_label
#' @export
#'
#' @seealso \code{\link{osm_basemap}}.
#'
#' @examples
#' bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
#' map <- osm_basemap (bbox = bbox, bg = "gray20")
#' map <- add_osm_objects (map, london$dat_BNR, col = "gray40") 
#' map <- add_axes (map)
#' print (map)
#'
#' # Map items are added sequentially, so adding axes prior to objects will
#' # produce a different result.
#' map <- osm_basemap (bbox = bbox, bg = "gray20")
#' map <- add_axes (map)
#' map <- add_osm_objects (map, london$dat_BNR, col = "gray40") 
#' print_osm_map (map)

add_axes <- function (map, colour = "black", pos = c(0.02, 0.03),
                      alpha = 0.4, fontsize = 3, fontface, fontfamily, ...)
{
    args <- list (...)
    if (hasArg ("color")) colour <- args [['color']]
    if (hasArg ("position")) pos <- args [['position']]
    if (hasArg ("size")) fontsize <- args [['size']]
    if (hasArg ("face")) fontface <- args [['face']]
    if (hasArg ("family")) fontfamily <- args [['family']]

    # ---------------  sanity checks and warnings  ---------------
    # ---------- map
    if (missing (map))
        stop ('map must be supplied to add_axes')
    check_map_arg (map)
    # ---------- colour
    tryCatch (
              col2rgb (colour),
              error = function (e)
              {
                  e$message <-  paste0 ("Invalid colour: ", colour)
                  stop (e)
              })
    # ---------- pos
    pos <- test_len2 (pos, 'pos')
    pos <- test_numeric (pos, 'pos', c (0.02, 0.03))
    pos <- test_range (pos, 'pos', c (0, 1), c (0.02, 0.03))
    # ---------- alpha
    alpha <- test_len1 (alpha, 'alpha')
    alpha <- test_numeric (alpha, 'alpha', 0.4)
    alpha <- test_range (alpha, 'alpha', c (0, 1), 0.4)
    # ---------- fontsize
    fontsize <- test_len1 (fontsize, 'fontsize')
    fontsize <- test_numeric (fontsize, 'fontsize', 3)
    fontsize <- test_pos (fontsize, 'fontsize', 3)
    # ---------------  end sanity checks and warnings  ---------------

    # The following are defaults from ggplot2/geom-label.R
    if (missing (fontface)) fontface <- 1
    if (missing (fontfamily)) fontfamily <- ""

    xrange <- map$coordinates$limits$x
    yrange <- map$coordinates$limits$y

    xp <- pretty (xrange) [2:(length (pretty (xrange)) - 1)]
    yp <- pretty (yrange) [2:(length (pretty (yrange)) - 1)]
    # change notation so xaxs_pos are y-coordinates of the x-axis
    xaxs_pos <- min (yrange) + pos * diff (yrange)
    axis_ratio <- diff (yrange) / diff (xrange)
    yaxs_pos <- min (xrange) + pos * axis_ratio * diff (xrange)

    expand <- 1.0 # expansion for semi-transparent rectangles

    map <- add_axis_rectangle (map, xaxs_pos, yaxs_pos, xrange, yrange,
                               alpha = alpha, expand = expand)
    map <- add_tick_rectangles_h (map, xp, xaxs_pos, yaxs_pos,
                               alpha = alpha, expand = expand)
    map <- add_tick_rectangles_v (map, yp, xaxs_pos, yaxs_pos,
                               alpha = alpha, expand = expand)

    map <- add_horizontal_axis (map, xaxs_pos, yaxs_pos, xrange, xp, colour,
                                alpha, fontsize, fontface, fontfamily)
    map <- add_vertical_axis (map, xaxs_pos, yaxs_pos, yrange, yp, colour,
                              alpha, fontsize, fontface, fontfamily)

    return (map)
}

# add semi-transparent rectangle that underlies the axes
#
#  (x1,y3)---(x2,y3)
#     |         |
#     |         |
#     |         |
#  (x1,y2)---(x2,y2)----------------------------(x3,y2)
#     |         |                                  |
#  (x1,y1)---(x2,y1)----------------------------(x3,y1)
#
add_axis_rectangle <- function (map, xaxs_pos, yaxs_pos, xrange, yrange,
                                alpha = 0.4, expand = 1.0)
{
    expand <- 1.0
    xr <- c (yaxs_pos [1] + c(-1, 1) * expand * diff (yaxs_pos), xrange [2])
    yr <- c (xaxs_pos [1] + c(-1, 1) * expand * diff (xaxs_pos), yrange [2])
    rdat <- data.frame (cbind ("lon" = c(xr[1], xr[1], xr[3], xr[3],
                                         xr[2], xr[2], xr[1]),
                               "lat" = c(yr[3], yr[1], yr[1], yr[2],
                                         yr[2], yr[3], yr[3])))

    lon <- lat <- NULL # suppress 'no visible binding' error
    aes <- ggplot2::aes (x = lon, y = lat, size = 0)

    map + ggplot2::geom_polygon (data = rdat, mapping = aes,
                                 inherit.aes = FALSE,
                                 fill = rgb (1, 1, 1, alpha),
                                 colour = 'transparent')
}

# add rectangles around horizontal ticks
add_tick_rectangles_h <- function (map, xp, xaxs_pos, yaxs_pos,
                                   alpha = 0.4, expand = 1.0)
{
    lon <- lat <- id <- NULL # suppress 'no visible binding' error
    x0 <- xp - expand * diff (yaxs_pos)
    x1 <- xp + expand * diff (yaxs_pos)
    y <- rep (xaxs_pos [c (1, 2, 2, 1, 1)], length (xp))
    x <- as.numeric (rbind (x0, x0, x1, x1, x0))
    rdat <- data.frame (cbind ("id" = rep (seq (xp), each = 5),
                               "lon" = x, "lat" = y))

    aes <- ggplot2::aes (x = lon, y = lat, group = id, size = 0)

    map + ggplot2::geom_polygon (data = rdat, mapping = aes,
                                 inherit.aes = FALSE,
                                 fill = rgb (1, 1, 1, alpha),
                                 colour = 'transparent')
}

# add rectangles around vertical ticks
add_tick_rectangles_v <- function (map, yp, xaxs_pos, yaxs_pos,
                                   alpha = 0.4, expand = 1.0)
{
    lon <- lat <- id <- NULL # suppress 'no visible binding' error
    y0 <- yp - expand * diff (xaxs_pos)
    y1 <- yp + expand * diff (xaxs_pos)
    x <- rep (yaxs_pos [c (1, 2, 2, 1, 1)], length (yp))
    y <- as.numeric (rbind (y0, y0, y1, y1, y0))
    rdat <- data.frame (cbind ("id" = rep (seq (yp), each = 5),
                               "lon" = x, "lat" = y))
    aes <- ggplot2::aes (x = lon, y = lat, group = id, size = 0)
    map <- map + ggplot2::geom_polygon (data = rdat, mapping = aes,
                                        inherit.aes = FALSE,
                                        fill = rgb (1, 1, 1, alpha),
                                        colour = 'transparent')
}

add_horizontal_axis <- function (map, xaxs_pos, yaxs_pos, xrange, xp, colour,
                                 alpha, fontsize, fontface, fontfamily)
{
    segdat <- data.frame (x1 = yaxs_pos [1], x2 = max (xrange),
                          y1 = xaxs_pos [1], y2 = xaxs_pos [1])
    x <- y <- x1 <- y1 <- x2 <- y2 <- NULL
    aes <- ggplot2::aes (x = x1, y = y1, xend = x2, yend = y2)
    map <- map + ggplot2::geom_segment (data = segdat, colour = colour,
                                        mapping = aes)

    # ticks and labels
    segdat <- data.frame (x1 = xp, x2 = xp,
                          y1 = xaxs_pos [1], y2 = xaxs_pos [2])
    labdat <- data.frame (x = xp, y = xaxs_pos [2])

    map + ggplot2::geom_segment (data = segdat, colour = colour,
                                 mapping = ggplot2::aes (x = x1, y = y1,
                                                         xend = x2,
                                                         yend = y2)) +
            ggplot2::geom_label (data = labdat,
                                 mapping = ggplot2::aes (x = x, y = y,
                                                         label = x),
                                 alpha = alpha,
                                 size = fontsize,
                                 colour = colour,
                                 inherit.aes = FALSE,
                                 label.size = 0,
                                 vjust = "inward",
                                 fontface = fontface, family = fontfamily)
}

add_vertical_axis <- function (map, xaxs_pos, yaxs_pos, yrange, yp, colour,
                               alpha, fontsize, fontface, fontfamily)
{
    segdat <- data.frame (x1 = yaxs_pos [1], x2 = yaxs_pos [1],
                          y1 = xaxs_pos [1], y2 = max (yrange))
    x <- y <- x1 <- y1 <- x2 <- y2 <- NULL
    aes <- ggplot2::aes (x = x1, y = y1, xend = x2, yend = y2)
    map <- map + ggplot2::geom_segment (data = segdat, colour = colour,
                                        mapping = aes)

    # ticks and labels
    segdat <- data.frame (x1 = yaxs_pos [1], x2 = yaxs_pos [2],
                          y1 = yp, y2 = yp)
    labdat <- data.frame (x = yaxs_pos [2], y = yp)

    map + ggplot2::geom_segment (data = segdat, colour = colour,
                                 mapping = ggplot2::aes (x = x1, y = y1,
                                                xend = x2,
                                                yend = y2)) +
            ggplot2::geom_label (data = labdat,
                                 mapping = ggplot2::aes (x = x, y = y,
                                                         label = y),
                                 alpha = alpha,
                                 size = fontsize,
                                 colour = colour,
                                 inherit.aes = FALSE,
                                 label.size = 0,
                                 hjust = "inward",
                                 fontface = fontface,
                                 family = fontfamily)
}
