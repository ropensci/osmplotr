#' plot.osm.basemap
#'
#' Generates a base OSM plot ready for polygon and line objects to be overlain
#' with add.osm.polygons () and add.osm.lines ()
#'
#' @param xylims = range to be plotted as returned from get.xylims ()
#' @param filename = name of plot file; default=NULL plots to screen device (low
#' quality and likely slow)
#' @param bg = background colour of map (default = "gray20")
#' @param graphic.device = "png" (default), "jpeg", "png", or "tiff"
#' @param ... other parameters to be passed to graphic device (such as width and
#' height; see ?png, for example, for details)
#' @return nothing (generates file of specified type).

plot.osm.basemap <- function (xylims=xylims, filename=NULL, bg="gray20",
                               graphic.device="png", ...)
{
    if (!is.null (filname))
        png (filename=filename, type="cairo-png", bg="white", ...)
    else
        x11 ()

    par (mar=c(0,0,0,0))
    plot (NULL, NULL, xlim=xylims$x, ylim=xylims$y, xaxs="i", yaxs="i",
          xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
    usr <- par ("usr")
    rect (usr [1], usr [3], usr [2], usr [4], border=NA, col=bg)
}

