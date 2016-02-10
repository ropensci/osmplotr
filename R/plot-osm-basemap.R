#' plot_osm_basemap
#'
#' Generates a base OSM plot ready for polygon and line objects to be overlain
#' with add_osm_polygons () and add_osm_lines (). NOTE: Graphics files must be
#' closed after finishing map with dev.off() or graphics.off(). Files size may
#' be adjusted by width only: height is automatically calculated from the aspect
#' ratio of the bounding box.
#'
#' @param xylims = Latitude-longitude range to be plotted as returned from
#' get_xylims () 
#' @param filename = name of plot file; default=NULL plots to
#' screen device (low
#' quality and likely slow)
#' @param width = width of graphics file (in px; default 480).
#' @param bg = background colour of map (default = "gray20")
#' @param graphic.device = "png" (default), "jpeg", "png", or "tiff"
#' @param ... other parameters to be passed to graphic device (such as width and
#' height; see ?png, for example, for details)
#' @return nothing (generates file of specified type).

plot_osm_basemap <- function (xylims=xylims, filename=NULL, width=640,
                              bg="gray20", graphic.device="png", ...)
{
    if (!is.null (filename))
        if (nchar (filename) == 0)
            filename <- NULL

    if (is.null (filename) & width == 640)
        width <- 7
    height <- width * diff (xylims$y) / diff (xylims$x)
    if (!is.null (filename))
        png (filename=filename, width=width, height=height,
             type="cairo-png", bg="white", ...)
    else 
        dev.new (width=width, height=height)

    par (mar=c(0,0,0,0))
    plot (NULL, NULL, xlim=xylims$x, ylim=xylims$y, xaxs="i", yaxs="i",
          xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
    usr <- par ("usr")
    rect (usr [1], usr [3], usr [2], usr [4], border=NA, col=bg)
}

