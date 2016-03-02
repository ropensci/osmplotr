#' plot_osm_basemap
#'
#' Generates a base OSM plot ready for polygon, line, and point objects to be
#' overlain with add_osm_objects(). NOTE: Graphics files must be closed after
#' finishing map with dev.off() or graphics.off(). Unless specified, height of
#' graphics device is automatically calculated in proportion to the given width
#' according to the aspect ratio of the bounding box.
#'
#' @param xylims Latitude-longitude range to be plotted as returned from
#' get_xylims()
#' @param filename Name of plot file; default=NULL plots to screen device (low
#' quality and likely slow)
#' @param width Width of graphics file (in px; default 480).
#' @param structures Data frame returned by osm_structures() used here to
#' specify background colour of plot; if 'structs=NULL', the colour is specified
#' by 'bg'
#' @param bg Background colour of map (default = 'gray20' only if structs not
#' given)
#' @param graphic.device Type of graphic device to print to. For example, 'png'
#' (default), 'jpeg', 'png', or 'tiff' 
#' @param ... Other parameters to be passed to graphic device (such as width and
#' height; see ?png, for example, for details)
#' @return nothing (generates file of specified type)

plot_osm_basemap <- function (xylims=xylims, filename=NULL, width=640,
                              structures=NULL, bg='gray20',
                              graphic.device='png', ...)
{
    if (!is.null (structures))
        bg = structure$cols [which (structures$structure == 'background')]

    if (!is.null (filename))
        if (nchar (filename) == 0)
            filename <- NULL

    if (is.null (filename) & width == 640)
        width <- 7
    height <- width * diff (xylims$y) / diff (xylims$x)
    if (!is.null (filename))
        png (filename=filename, width=width, height=height,
             type='cairo-png', bg='white', ...)
    else 
        dev.new (width=width, height=height)

    par (mar=c(0,0,0,0))
    plot (NULL, NULL, xlim=xylims$x, ylim=xylims$y, xaxs='i', yaxs='i',
          xaxt='n', yaxt='n', xlab='', ylab='', bty='n')
    usr <- par ('usr')
    rect (usr [1], usr [3], usr [2], usr [4], border=NA, col=bg)
}

