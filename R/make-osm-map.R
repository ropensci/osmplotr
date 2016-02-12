#' make_osm_map
#'
#' Makes an entire OSM map for the given bbox by downloading all data.  This
#' function can take quite some time (tens of minutes) to execute the two
#' primary steps of downloading and processing the raw OSM data.  For this
#' reason, this function is not intended to be actually run; rather it serves to
#' demonstrate how all functions may be combined to generate a map.
#'
#' Progress is dumped to screen, including time taken.
#' 
#' @param filename Name of plot file; default=NULL plots to screen device (low
#' quality and likely slow)
#' @param bbox The bounding box for the map.  Must be a vector of 4 elements
#' (xmin, ymin, xmax, ymax). If NULL, bbox is taken from the largest extent of
#' OSM objects in osm_data.
#' @param osm_data A list of OSM objects as returned from
#' \code{extract_osm_objects}.  These objects may be included in the plot
#' without downloading. These should all be named with the stated
#' \code{dat_prefix} and have suffixes as given in \code{structures}.
#' @param structures A data.frame specifying types of OSM structures as returned
#' from osm_structures, and potentially modified to alter lists of structures to
#' be plotted, and their associated colours. The order of structs determines the
#' plot order of objects.
#' @param dat_prefix Prefix for data structures (default 'dat_'). Final data
#' structures are created by appending the suffixes from \code{osm_structures}.
#' @return list of OSM structures each as Spatial(Polygon/List)DataFrame, and
#' appended to `osm_data` (which is NULL by default).


make_osm_map <- function (filename=NULL, bbox=NULL, osm_data=NULL,
                          structures=osm_structures (), 
                          dat_prefix="dat_")
{
    if (is.null (bbox) & is.null (osm_data))
        stop ("Either bounding box or osm_data must be given")
    if (!is.null (osm_data))
        attach (osm_data)
    if (is.null (bbox)) # get it from osm_data
    {
        xylims <- list (xrange=c (Inf, -Inf), yrange=c(Inf, -Inf))
        for (i in osm_data)
        {
            lims <- get_xylims (i)
            if (lims$xrange [1] < xylims$xrange [1])
                xylims$xrange [1] <- lims$xrange [1]
            if (lims$xrange [2] > xylims$xrange [2])
                xylims$xrange [2] <- lims$xrange [2]
            if (lims$yrange [1] < xylims$yrange [1])
                xylims$yrange [1] <- lims$yrange [1]
            if (lims$yrange [2] > xylims$yrange [2])
                xylims$yrange [2] <- lims$yrange [2]
        }
        bbox <- c (lims$xrange [1], lims$yrange [1],
                   lims$xrange [2], lims$yrange [2])
    }

    sfx <- structures$suffixes [1:(nrow (structures) - 1)]
    structs_new <- which (!sapply (sfx, function (i) 
                                     exists (paste0 (dat_prefix, i))))
    if (length (structs_new) > 0)
    {
        structs_full <- structures
        structures <- structures [structs_new,]
    }
    ns <- nrow (structures) - 1 # last row is background

    cat ("Downloading and extracting OSM data for", ns, "structures ...\n")
    pb <- txtProgressBar (max=1, style = 3) # shows start and end positions
    t0 <- proc.time ()
    for (i in 1:ns) {
        dat <- extract_osm_objects (key=structures$key [i],
                                    value=structures$value [i], bbox=bbox)
        fname <- paste0 (dat_prefix, structures$suffixes [i])
        assign (fname, dat)
        osm_data <- c (osm_data, list (fname=get (fname)))
        setTxtProgressBar(pb, i / ns)
    }
    close (pb)
    cat ("That took ", (proc.time () - t0)[3], "s\n", sep="")

    plot_osm_basemap (xylims=xylims, filename=filename)
    for (i in seq (nrow (structs)))
    {
        fname <- paste ("dat_", structs$letters [i], sep="")
        load (get (fname))
        add_osm_objects (get (fname), col=as.character (structs$cols [i]))
        rm (list=c(fname))
    }

    if (!is.null (filename)) 
        junk <- dev.off (which=dev.cur())
}
