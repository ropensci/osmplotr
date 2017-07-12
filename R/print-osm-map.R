#' print_osm_map
#'
#' Prints an OSM map produced with \code{osmplotr} to a specified graphics device.
#'
#' @param map The map to be printed; a \code{ggplot2} object produced by
#' \code{osmplotr}.
#' @param width Desired width of graphics device.
#' @param height Desired height of graphics device. Ignored if width specified.
#' @param filename Name of file to which map is to be printed.
#' @param device Type of graphics device (extracted from filename extension if
#' not explicitly provided).
#' @param units Units for height and width of graphics device.
#' @param dpi Resolution of graphics device (dots-per-inch).
#' @export
#'
#' @seealso \code{\link{osm_basemap}}, \code{\link{add_osm_objects}},
#' \code{\link{make_osm_map}}.
#'
#' @examples
#' bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
#' map <- osm_basemap (bbox = bbox, bg = 'gray20')
#' map <- add_osm_objects (map, london$dat_BNR, col = 'gray40') 
#' print_osm_map (map, width = 7) # prints to screen device
#' \dontrun{
#' print_osm_map (map, file = 'map.png', width = 500, units = 'px')
#' }
print_osm_map <- function (map, width, height, filename,
                           device, units = c('in', 'cm', 'mm', 'px'), dpi = 300)
{
    if (missing (map))
        stop ('map must be supplied')
    if (missing (width) & missing (height))
        width <- 7

    xlims <- map$coordinates$limits$x
    ylims <- map$coordinates$limits$y
    if (!missing (width))
        height <- width * diff (ylims) / diff (xlims)
    else
        width <- height * diff (xlims) / diff (ylims)

    units <- match.arg (units)
    if (missing (device) & missing (filename))
    {
        dev.new (width = width, height = height)
        print (map)
    } else
    {
        dev <- get_graphics_device (device, filename, units, dpi = dpi)
        dev (file = filename, width = width, height = height)
        print (map)
        on.exit (utils::capture.output (grDevices::dev.off (which =
                                                            dev.cur ())))
    }
    invisible (map)
}

# code from hadley/ggplot2::save
get_graphics_device <- function (device, filename, units, dpi = 300)
{
    devices <- list (
                     eps =  function (...)
                         grDevices::postscript (..., onefile = FALSE,
                                                horizontal = FALSE,
                                                paper = 'special'),
                     ps =  function (...)
                         grDevices::postscript (..., onefile = FALSE,
                                                horizontal = FALSE,
                                                paper = 'special'),
                     tex =  function (...) grDevices::pictex (...),
                     pdf =  function (..., version = '1.4')
                         grDevices::pdf (..., version = version),
                     svg =  function (...) grDevices::svg (...),
                     png =  function (...)
                         grDevices::png (..., res = dpi, units = units),
                     jpg =  function (...)
                         grDevices::jpeg (..., res = dpi, units = units),
                     jpeg = function (...)
                         grDevices::jpeg (..., res = dpi, units = units),
                     bmp =  function (...)
                         grDevices::bmp (..., res = dpi, units = units),
                     tiff = function (...)
                         grDevices::tiff (..., res = dpi, units = units)
                     )
    if (missing (device))
        device <- tolower (tools::file_ext (filename))
    if (!is.character(device) || length(device) != 1)
        stop("`device` must be NULL, a string or a function.", call. = FALSE)

    dev <- devices [[device]]
    if (is.null(dev))
        stop("Unknown graphics device '", device, "'", call. = FALSE)
    return (dev)
}
