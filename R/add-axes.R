#' add_axes
#'
#' Adds axes to the internal region of an OSM plot.
#'
#' @param map A ggplot2 object to which the axes are to be added
#' @param colour Colour of axis (determines colour of all elements: lines,
#' ticks, and labels)
#' @param pos Positions of axes and labels relative to entire plot device
#' @param alpha alpha value for semi-transparent background surrounding axes and
#' labels (lower values increase transparency)
#' @param fontsize Size of axis font 
#' @param fontface Fontface for axis labels (1:4=plain,bold,italic,bold-italic)
#' @param fontfamily Family of axis font (for example, 'Times')
#' @return Modified version of map with axes added
#' @export
#'
#' @seealso \code{\link{plot_osm_basemap}}.
#'
#' @examples
#' bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
#' map <- plot_osm_basemap (bbox=bbox, bg="gray20")
#' map <- add_osm_objects (map, london$dat_BNR, col="gray40") 
#' map <- add_axes (map)
#' print (map)
#'
#' # Map items are added sequentially, so adding axes prior to objects will
#' # produce a different result.
#' map <- plot_osm_basemap (bbox=bbox, bg="gray20")
#' map <- add_axes (map)
#' map <- add_osm_objects (map, london$dat_BNR, col="gray40") 
#' print (map)

add_axes <- function (map, colour="black", pos=c(0.02,0.03),
                      alpha=0.4, fontsize=3, fontface, fontfamily)
{
    # ---------------  sanity checks and warnings  ---------------
    # ---------- map
    if (missing (map)) stop ('map must be supplied to add_axes')
    if (!is (map, 'ggplot')) stop ('map must be a ggplot object')

    # ---------- alpha
    alpha <- test_len1 (alpha, 'alpha')
    alpha <- test_numeric (alpha, 'alpha', 0.4)
    alpha <- test_range (alpha, 'alpha', c (0, 1), 0.4)
    # ---------- pos
    pos <- test_len2 (pos, 'pos')
    pos <- test_numeric (pos, 'pos', c (0.02, 0.03))
    pos <- test_range (pos, 'pos', c (0, 1), c (0.02, 0.03))
    # ---------- fontsize
    fontsize <- test_len1 (fontsize, 'fontsize')
    fontsize <- test_numeric (fontsize, 'fontsize', 3)
    fontsize <- test_pos (fontsize, 'fontsize', 3)
    # ---------------  end sanity checks and warnings  ---------------

    # The following are defaults from ggplot2/geom-label.R
    if (missing (fontface)) fontface <- 1
    if (missing (fontfamily)) fontfamily <- ""

    gs <- ggplot2::geom_segment
    glab <- ggplot2::geom_label
    aes <- ggplot2::aes

    xrange <- map$coordinates$limits$x
    yrange <- map$coordinates$limits$y

    xp <- pretty (xrange) [2:(length (pretty (xrange))-1)]
    yp <- pretty (yrange) [2:(length (pretty (yrange))-1)]
    xlims <- c (xp [1] - (xp [1] - min (xrange)) / 2,
                max (xp) - (max (xp) - max (xrange)) / 2)
    ylims <- c (yp [1] - (yp [1] - min (yrange)) / 2,
                max (yp) - (max (yp) - max (yrange)) / 2)
    xaxs_pos <- min (yrange) + pos * diff (yrange)
    axis_ratio <- diff (yrange) / diff (xrange)
    yaxs_pos <- min (xrange) + pos * axis_ratio * diff (xrange)

    # Rectangle around axes
    expand <- 0.02
    x0 <- yaxs_pos [1] - expand * diff (yaxs_pos)
    x1 <- xrange [2]
    y0 <- xaxs_pos [1] - expand * diff (xaxs_pos)
    y1 <- yrange [2]
    rdat <- data.frame (cbind ("lon"=c (x0, x0, x1, x1, x0), 
                               "lat"=c (y0, y1, y1, y0, y0)))
    lon <- lat <- id <- NULL # suppress 'no visible binding' error
    aes2 <- ggplot2::aes (x=lon, y=lat, size=0)
    map <- map + ggplot2::geom_path (data=rdat, mapping=aes2, inherit.aes=FALSE,
                                     colour=rgb (1, 1, 1, alpha))

    # And rectangles around tick marks, starting with horiztonal
    x0 <- xp - expand * diff (yaxs_pos)
    x1 <- xp + expand * diff (yaxs_pos)
    y <- rep (xaxs_pos [c (1, 2, 2, 1, 1)], length (xp))
    x <- as.numeric (rbind (x0, x0, x1, x1, x0))
    rdat <- data.frame (cbind ("id"=rep (seq (xp), each=5), "lon"=x, "lat"=y))
    aes2 <- ggplot2::aes (x=lon, y=lat, group=id, size=0)
    map <- map + ggplot2::geom_path (data=rdat, mapping=aes2, inherit.aes=FALSE,
                                     colour=rgb (1, 1, 1, alpha))
    # Then rectangles around vertical tick marks
    y0 <- yp - expand * diff (xaxs_pos)
    y1 <- yp + expand * diff (xaxs_pos)
    x <- rep (yaxs_pos [c (1, 2, 2, 1, 1)], length (yp))
    y <- as.numeric (rbind (y0, y0, y1, y1, y0))
    rdat <- data.frame (cbind ("id"=rep (seq (yp), each=5), "lon"=x, "lat"=y))
    aes2 <- ggplot2::aes (x=lon, y=lat, group=id, size=0)
    map <- map + ggplot2::geom_path (data=rdat, mapping=aes2, inherit.aes=FALSE,
                                     colour=rgb (1, 1, 1, alpha))

    # ------------- horiztonal axis
    # main line 
    segdat <- data.frame (x1=yaxs_pos [1], x2=max (xrange),
                          y1=xaxs_pos [1], y2=xaxs_pos [1])
    x2 <- y2 <- NULL
    map <- map + gs (data=segdat, colour=colour,
                            mapping=aes (x=x1, y=y1, xend=x2, yend=y2))

    # ticks and labels
    segdat <- data.frame (x1=xp, x2=xp,
                          y1=xaxs_pos [1], y2=xaxs_pos [2])
    labdat <- data.frame (x=xp, y=xaxs_pos [2])
    map <- map + gs (data=segdat, colour=colour,
                     mapping=ggplot2::aes (x=x1, y=y1, xend=x2, yend=y2)) +
                glab (data=labdat, mapping=ggplot2::aes (x=x, y=y, label=x),
                      alpha=alpha, size=fontsize, colour=colour,
                      inherit.aes=FALSE, label.size=0, vjust="inward",
                      fontface=fontface, family=fontfamily)

    # ------------- vertical axis
    # main line 
    segdat <- data.frame (x1=yaxs_pos [1], x2=yaxs_pos [1],
                          y1=xaxs_pos [1], y2=max (yrange))
    map <- map + gs (data=segdat, colour=colour,
                            mapping=aes (x=x1, y=y1, xend=x2, yend=y2))
    # ticks and labels
    segdat <- data.frame (x1=yaxs_pos [1], x2=yaxs_pos [2],
                          y1=yp, y2=yp)
    labdat <- data.frame (x=yaxs_pos [2], y=yp)
    map <- map + gs (data=segdat, colour=colour,
                     mapping=aes (x=x1, y=y1, xend=x2, yend=y2)) +
                glab (data=labdat, mapping=aes (x=x, y=y, label=y),
                      alpha=alpha, size=fontsize, colour=colour,
                      inherit.aes=FALSE, label.size=0, hjust="inward",
                      fontface=fontface, family=fontfamily)

    return (map)
}

