#' plot_osm_basemap
#'
#' Generates a base OSM plot ready for polygon, line, and point objects to be
#' overlain with add_osm_objects(). NOTE: Graphics files must be closed after
#' finishing map with dev.off() or graphics.off(). Unless specified, height of
#' graphics device is automatically calculated in proportion to the given width
#' according to the aspect ratio of the bounding box.
#'
#' @param bbox bounding box (Latitude-longitude range) to be plotted.  A 2-by-2
#' matrix of 4 elements with columns of min and max values, and rows of x and y
#' values.
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
#' @export
#'
#' @examples
#' plot_osm_basemap (bbox=get_bbox (c (-0.15, 51.5, -0.1, 51.52)), col="gray20")
#' add_osm_objects (london$dat_BNR, col="gray40") # non-residential buildings

plot_osm_basemap <- function (bbox=bbox, filename=NULL, width=640,
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
    height <- width * diff (bbox [2,]) / diff (bbox [1,])
    if (!is.null (filename))
        png (filename=filename, width=width, height=height,
             type='cairo-png', bg='white', ...)
    else 
        dev.new (width=width, height=height)

    par (mar=rep (0, 4))
    plot (NULL, NULL, xlim=bbox [1,], ylim=bbox [2,], xaxs='i', yaxs='i',
          xaxt='n', yaxt='n', xlab='', ylab='', bty='n')
    usr <- par ('usr')
    rect (usr [1], usr [3], usr [2], usr [4], border=NA, col=bg)
}

