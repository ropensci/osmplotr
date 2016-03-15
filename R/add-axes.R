#' add_axes
#'
#' Adds axes to the internal region of an OSM plot.
#'
#' @param axis_col Colour of axis (determines colour of all elements: lines,
#' ticks, and labels)
#' @param axis_ps Point size of axis font (default 8)
#' @param axis_pos Relative positions of axes and labels (default=c(0.01,0.03))
#' @export
#'
#' @examples
#' plot_osm_basemap (bbox=get_bbox (c (-0.15, 51.5, -0.1, 51.52)), col="gray20")
#' add_osm_objects (london$dat_BNR, col="gray40") # non-residential buildings
#' add_axes ()

add_axes <- function (axis_col="white", axis_ps=8, axis_pos=c(0.01,0.03))
{
    usr <- par ('usr')
    xrange <- usr [1:2]
    yrange <- usr [3:4]
    # NOTE: this is much longer here than using axis(), but is allows greater
    # control, and particularly enables implicit scaling for different point
    # sizes.
    ps0 <- par ("ps")
    par (ps=axis_ps)
    xp <- pretty (xrange) [2:(length (pretty (xrange))-1)]
    yp <- pretty (yrange) [2:(length (pretty (yrange))-1)]
    xlims <- c (xp [2] - (xp [1] - min (xrange)) / 2,
                max (xp) - (max (xp) - max (xrange)) / 2)
    ylims <- c (yp [1] - (yp [1] - min (yrange)) / 2,
                max (yp) - (max (yp) - max (yrange)) / 2)
    xaxs_pos <- min (yrange) + axis_pos * diff (yrange)
    axis_ratio <- diff (yrange) / diff (xrange)
    yaxs_pos <- min (xrange) + axis_pos * axis_ratio * diff (xrange)

    lines (c (yaxs_pos [1], max (xrange)), rep (xaxs_pos [1], 2), col=axis_col)
    junk <- sapply (xp, function (i)
                    {
                        lines (rep (i, 2), xaxs_pos [1:2], col=axis_col)
                        text (i, xaxs_pos [2], col=axis_col, pos=3,
                              labels=i)
                    })
    lines (rep (yaxs_pos [1], 2), c (xaxs_pos [1], max (yrange)), 
           col=axis_col)
    junk <- sapply (yp, function (i)
                    {
                        lines (yaxs_pos [1:2], rep (i, 2), col=axis_col)
                        text (yaxs_pos [2], i, col=axis_col, pos=4, labels=i)
                    })
}

