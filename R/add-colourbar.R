#' add_colorbar
#'
#' Adds a colourbar to an existing map. Intended to be used in combination with
#' \code{\link{add_osm_surface}}. At present, only plots on right side of map.
#'
#' @param map A \code{ggplot2} object to which the colourbar is to be added.
#' @param barwidth Relative width of the bar (perpendicular to its direction),
#' either a single number giving distance from right or upper margin, or two
#' numbers giving left/right or lower/upper limits.
#' @param barlength Relative length of the bar (parallel to its direction),
#' either a single number giving total length of centred bar, or two numbers
#' giving lower/upper or left/right limits.
#' @param zlims Vector of (min,max) values for scale of colourbar. These should
#' be the values returned from \code{\link{add_osm_surface}}.
#' @param cols Vector of colours.
#' @param vertical If \code{FALSE}, colourbar is aligned horizontally instead of
#' default vertical alignment.
#' @param alpha Transparency level of region immediately surrounding colourbar,
#' including behind text. Lower values are more transparent.
#' @param text_col Colour of text, tick marks, and lines on colourbar.
#' @param fontsize Size of text labels (in \code{ggplot2} terms; default=3).
#' @param fontface Fontface for colourbar labels
#' (1:4=plain,bold,italic,bold-italic).
#' @param fontfamily Family of colourbar font (for example, `\code{Times}').
#' @param ... Mechanism to allow many parameters to be passed with alternative
#' names (such as \code{xyz} for \code{fontxyz}).
#' @return Modified version of \code{map} with colourbar added.
#' @importFrom ggplot2 aes geom_path geom_tile geom_segment geom_label
#' @export
#'
#' @seealso \code{\link{osm_basemap}}, \code{\link{add_osm_surface}}.
#'
#' @examples
#' bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
#' map <- osm_basemap (bbox = bbox, bg = "gray20")
#' # Align volcano data to lat-lon range of bbox
#' dv <- dim (volcano)
#' x <- seq (bbox [1,1], bbox [1,2], length.out = dv [1])
#' y <- seq (bbox [2,1], bbox [2,2], length.out = dv [2])
#' dat <- data.frame (
#'                   x = rep (x, dv [2]),
#'                   y = rep (y, each = dv [1]),
#'                   z = as.numeric (volcano)
#'                   )
#' map <- add_osm_surface (map, obj = london$dat_BNR, dat = dat,
#'                         cols = heat.colors (30))
#' map <- add_axes (map)
#' # Note colours of colourbar can be artibrarily set, and need not equal those
#' # passed to 'add_osm_surface'
#' map <- add_colourbar (map, zlims = range (volcano), cols = heat.colors(100),
#'                       text_col = "black")
#' print_osm_map (map)
#'
#' # Horizontal colourbar shifted away from margins:
#' map <- osm_basemap (bbox = bbox, bg = "gray20")
#' map <- add_osm_surface (map, obj = london$dat_BNR, dat = dat,
#'                         cols = heat.colors (30))
#' map <- add_colourbar (map, zlims = range (volcano), cols = heat.colors(100),
#'                       barwidth = c(0.1,0.15), barlength = c(0.5, 0.9),
#'                       vertical = FALSE)
#' print_osm_map (map)

add_colourbar <- function (map, barwidth = 0.02, barlength = 0.7, zlims, cols,
                           vertical = TRUE, alpha = 0.4,
                           text_col = "black", fontsize = 3,
                           fontface, fontfamily, ...)
{
    args <- list (...)
    if (hasArg ("width")) barwidth <- args [['width']]
    if (hasArg ("length")) barlength <- args [['length']]
    if (hasArg ("colors")) cols <- args [['colors']]
    if (hasArg ("colours")) cols <- args [['colours']]
    if (hasArg ("color")) text_col <- args [['color']]
    if (hasArg ("colour")) text_col <- args [['colour']]
    if (hasArg ("size")) fontsize <- args [['size']]
    if (hasArg ("face")) fontface <- args [['face']]
    if (hasArg ("family")) fontfamily <- args [['family']]

    # ---------------  sanity checks and warnings  ---------------
    # ---------- map
    if (missing (map))
        stop ('map must be supplied to add_axes')
    if (missing (cols))
        stop ("cols must be specified in add_colourbar")
    check_map_arg (map)
    barwidth <- check_barwidth_arg (barwidth)
    barlength <- check_barlength_arg (barlength)
    fontsize <- check_fontsize_arg (fontsize)
    vertical <- check_vertical_arg (vertical)
    alpha <- check_alpha_arg (alpha)
    if (missing (fontface)) fontface <- 1
    if (missing (fontfamily)) fontfamily <- ""
    # ---------------  end sanity checks and warnings  ---------------

    # suppress 'no visible binding' warnings
    x <- y <- x1 <- y1 <- x2 <- y2 <- NULL

    # ---------- Initial data setup
    # expand is for semi-transparent underlay, done for direction parallel to
    # colourbar only; perpendicular expansion is handled by size = 5 below.
    expand <- 0.02

    cbxy <- get_colourbar_xy (map, cols, vertical, barwidth, barlength, expand)

    # ---------- LAYER#1: semi-transparent underlay
    cbu <- colourbar_underlay (cbxy, vertical, expand)
    aes <- ggplot2::aes (x = x, y = y, size = 0)
    pcol <- rgb (1, 1, 1, alpha)
    # geom_path has rounded corners, geom_poly does not, and size = 5 *should*
    # ensure it covers the inside of most bars
    map <- map + ggplot2::geom_path (data = cbu$rdat, mapping = aes,
                                     inherit.aes = FALSE,
                                     colour = pcol, size = 5)

    # ---------- LAYER#2: colourbar
    aes <- ggplot2::aes (x = x, y = y)
    args <- colourbar_tile_args (cbu$cbxy, vertical, cols, aes)
    map <- map + do.call (ggplot2::geom_tile, args)

    # ---------- LAYER#3: outline around colourbar
    rdat <- colourbar_outline (cbxy, vertical, expand)
    map <- map + ggplot2::geom_path (data = rdat, mapping = aes,
                                     colour = text_col)

    # ---------- LAYERS#4-5: ticks and labels
    tl <- colourbar_ticks_labels (map, cbu, zlims, vertical)
    z <- tl$z

    gs <- ggplot2::geom_segment
    glab <- ggplot2::geom_label

    x2 <- y2 <- NULL # suppress 'no visible binding' error
    map + gs (data = tl$segdat, colour = text_col,
                     mapping = ggplot2::aes (x = x1, y = y1,
                                             xend = x2, yend = y2)) +
        glab (data = tl$labdat, mapping = ggplot2::aes (x = x, y = y,
                                                        label = z),
              alpha = alpha, size = fontsize, colour = text_col,
              fontface = fontface, family = fontfamily, inherit.aes = FALSE,
              label.size = 0, nudge_x = tl$nudge_x, nudge_y = tl$nudge_y,
              vjust = tl$vjust, hjust = tl$hjust)
}



#' initial coordinates for colourbar
#'
#' @noRd
get_colourbar_xy <- function (map, cols, vertical, barwidth, barlength, expand)
{
    xrange <- map$coordinates$limits$x
    yrange <- map$coordinates$limits$y

    n <- length (cols)
    if (!vertical)
    {
        zr <- xrange
        xrange <- yrange
        yrange <- zr
    }

    if (length (barwidth) == 1)
    {
        # barwidth transformed to map scale
        barwidth <- diff (xrange) * barwidth
        # x0 is centre of bar, shifted by 1/2 barwidth + 2 * expand, so the
        # transparent underlay fits entirely within panel
        x0 <- xrange [2] - (1 / 2 + 2 * expand) * barwidth
    } else
    {
        # x0 not shifted by expand here, just by mean (barwidth)
        x0 <- xrange [2] - mean (barwidth) * diff (xrange)
        # with barwidth then converted to a single number
        barwidth <- diff (xrange) * diff (barwidth)
    }
    x <- rep (x0, n)
    if (length (barlength) == 1)
        y0 <- yrange [1] + (0.5 + c (-1 / 2, 1 / 2) * barlength) * diff (yrange)
    else
        y0 <- yrange [1] + barlength * diff (yrange)
    y <- seq (y0 [1], y0 [2], length.out = n)

    # Note that the actual limits of geom_tile are increased by 1/2 an
    # increment, so the final range of the bar must be expanded
    bary <- range (y) + c (-1, 1) * (y [2] - y [1]) / 2

    if (!vertical)
    {
        z <- x
        x <- y
        y <- z
    }

    #cb <- data.frame (x = x, y = y)
    list ('x' = x, 'y' = y, 'bary' = bary, 'barwidth' = barwidth)
}


#' coordinates for semi-transparent underlay of colourbar
#'
#' @noRd
colourbar_underlay <- function (cbxy, vertical, expand)
{
    # The colour bar is also moved to middle of the underlay, away from the edge
    # by expand/2
    if (vertical)
    {
        x1 <- cbxy$x [1] + cbxy$barwidth * c (-1, 1) * (1 + 1 * expand) / 2
        y1 <- cbxy$bary
        cbxy$x <- mean (x1)
    } else
    {
        x1 <- cbxy$bary
        y1 <- cbxy$y [1] + cbxy$barwidth * c (-1, 1) * (1 + 1 * expand) / 2
        cbxy$y <- mean (y1)
    }
    # The df needs the 6th point to connect up properly
    rdat <- data.frame (
                    "x" = c (x1 [1], x1 [1], x1 [2], x1 [2], x1 [1], x1 [1]),
                    "y" = c (y1 [1], y1 [2], y1 [2], y1 [1], y1 [1], y1 [2])
                        )

    list ('cbxy' = cbxy, 'rdat' = rdat, 'x1' = x1, 'y1' = y1)
}

#' Get 
#'
#' @noRd
colourbar_tile_args <- function (cbxy, vertical, cols, aes)
{
    args <- list (data = data.frame ('x' = cbxy$x, 'y' = cbxy$y),
                  mapping = aes, fill = cols)
    if (vertical)
        args <- c (args, width = cbxy$barwidth)
    else
        args <- c (args, height = cbxy$barwidth)

    return (args)
}

#' coordinates for outline box of colourbar
#'
#' @noRd
colourbar_outline <- function (cbxy, vertical, expand)
{
    makedat <- function (a, b, barwidth, expand)
    {
        bt <- max (b) + diff (b) [1] / 2
        bb <- min (b) - diff (b) [1] / 2
        data.frame (
                    x = mean (a) + c (-1, -1, 1, 1, -1) * barwidth / 2,
                    y = c (bb, bt, bt, bb, bb)
                    )
    }
    if (vertical)
        rdat <- makedat (cbxy$x, cbxy$y, cbxy$barwidth, expand)
    else
    {
        rdat <- makedat (cbxy$y, cbxy$x, cbxy$barwidth, expand)
        names (rdat) <- c ("y", "x")
    }

    return (rdat)
}

#' parameters for ticks and labels
#'
#' @noRd
colourbar_ticks_labels <- function (map, cbu, zlims, vertical)
{
    # Note that the actual limits of geom_tile are increased by 1/2 an
    # increment, so:
    zlabs <- pretty (zlims)
    zlabs <- zlabs [which (zlabs > zlims[1] & zlabs < zlims[2])]
    z <- cbu$cbxy$bary [1] + (zlabs - zlims [1]) *
        diff (cbu$cbxy$bary) / diff (zlims)

    if (vertical)
    {
        segdat <- data.frame (x1 = cbu$x1 [1], x2 = cbu$x1 [2], y1 = z, y2 = z)
        labdat <- data.frame (x = cbu$x1 [1], y = z, z = zlabs)
        nudge_x <- -0.005 * diff (map$coordinates$limits$x)
        if (length (nudge_x) == 0)
            nudge_x <- -0.005
        nudge_y <- 0
        vjust <- 0.5
        hjust <- 1
    } else
    {
        segdat <- data.frame (x1 = z, x2 = z, y1 = cbu$y1 [1], y2 = cbu$y1 [2])
        labdat <- data.frame (x = z, y = cbu$y1 [1], z = zlabs)
        nudge_x <- 0
        nudge_y <- -0.005 * diff (map$coordinates$limits$y)
        if (length (nudge_y) == 0)
            nudge_y <- -0.010
        vjust <- 1
        hjust <- 0.5
    }

    return (list ('z' = z, 'segdat' = segdat, 'labdat' = labdat,
                  'nudge_x' = nudge_x, 'nudge_y' = nudge_y,
                  'vjust' = vjust, 'hjust' = hjust))
}

check_barwidth_arg <- function (barwidth)
{
    barwidth <- test_len2 (barwidth, 'barwidth')
    barwidth <- test_numeric (barwidth, 'barwidth', 0.02)
    barwidth <- test_range (barwidth, 'barwidth', c (0, 1), 0.02)

    return (sort (barwidth))
}

check_barlength_arg <- function (barlength)
{
    barlength <- test_len2 (barlength, 'barlength')
    barlength <- test_numeric (barlength, 'barlength', 0.7)
    barlength <- test_range (barlength, 'barlength', c (0, 1), 0.7)

    return (sort (barlength))
}

check_fontsize_arg <- function (fontsize)
{
    fontsize <- test_len1 (fontsize, 'fontsize')
    fontsize <- test_numeric (fontsize, 'fontsize', 3)
    fontsize <- test_pos (fontsize, 'fontsize', 3)

    return (fontsize)
}

check_vertical_arg <- function (vertical)
{
    vertical <- test_len1 (vertical, 'vertical')
    vertical <- test_logical (vertical, 'vertical', TRUE)

    return (vertical)
}

check_alpha_arg <- function (alpha)
{
    alpha <- test_len1 (alpha, 'alpha')
    alpha <- test_numeric (alpha, 'alpha', 0.4)
    alpha <- test_range (alpha, 'alpha', c (0, 1), 0.4)

    return (alpha)
}
