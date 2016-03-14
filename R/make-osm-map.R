#' make_osm_map
#'
#' Makes an entire OSM map for the given bbox using the submitted data, or by
#' downloading data if none submitted. This is a convenience function enabling
#' an entire map to be produced according to the graphical format specified with
#' the `structures` argument.  
#' 
#' @param filename Name of plot file; default=NULL plots to screen device (low
#' quality and likely slow)
#' @param bbox the bounding box for the map.  A 2-by-2 matrix of 4 elements with
#' columns of min and max values, and rows of x and y values.  If NULL, bbox is
#' taken from the largest extent of OSM objects in osm_data.
#' @param osm_data A list of OSM objects as returned from extract_osm_objects().
#' These objects may be included in the plot without downloading. These should
#' all be named with the stated 'dat_prefix' and have suffixes as given in
#' 'structures'.
#' @param structures A data.frame specifying types of OSM structures as returned
#' from osm_structures(), and potentially modified to alter lists of structures
#' to be plotted, and their associated colours. Objects are overlaid on plot
#' according to the order given in 'structures'.
#' @param width Width of graphics device, with height calculated according to
#' latitudinal and longitudinal proportions of 'bbox'.
#' @param dat_prefix Prefix for data structures (default 'dat_'). Final data
#' structures are created by appending the suffixes from osm_structures().
#' @return list of OSM structures each as Spatial(Polygon/List)DataFrame, and
#' appended to `osm_data` (which is NULL by default).
#' @export
#'
#' @section Note:
#' If 'osm_data=NULL', then data will be downloaded, which can take some time.
#' Progress is dumped to screen.


make_osm_map <- function (filename=NULL, bbox=NULL, osm_data=NULL,
                          structures=osm_structures (), width=640,
                          dat_prefix='dat_')
{
    if (is.null (bbox)) # get it from osm_data
    {
        if (is.null (osm_data))
            stop ('Either bounding box or osm_data must be given')
        bbox <- matrix (c (Inf, Inf, -Inf, -Inf), nrow=2, ncol=2)
        rownames (bbox) <- c ("x", "y")
        classes <- c ('SpatialLinesDataFrame', 'SpatialPolygonsDataFrame',
                      'SpatialPointsDataFrame')
        for (i in osm_data)
        {
            if (class (i) %in% classes)
            {
                bbi <- slot (i, "bbox")
                if (bbi [1,1] < bbox [1,1]) bbox [1,1] <- bbi [1,1]
                if (bbi [2,1] < bbox [2,1]) bbox [2,1] <- bbi [2,1]
                if (bbi [1,2] > bbox [1,2]) bbox [1,2] <- bbi [1,2]
                if (bbi [2,2] > bbox [2,2]) bbox [2,2] <- bbi [2,2]
            }
        }
    }

    sfx <- structures$suffix [1:(nrow (structures) - 1)]
    structs_new <- which (!sapply (sfx, function (i) 
                           any (paste0 (dat_prefix, i) %in% names (osm_data))))
    if (length (structs_new) > 0)
    {
        structs_full <- structures
        structures <- structures [structs_new,]

        cat ('Downloading and extracting OSM data for', ns, 'structures ...\n')
        pb <- txtProgressBar (max=1, style = 3) # shows start and end positions
        t0 <- proc.time ()
        for (i in 1:nrow (structures)) {
            dat <- extract_osm_objects (key=structures$key [i],
                                        value=structures$value [i], bbox=bbox)
            if (!is.null (dat$warn))
                warning (paste0 (structures$structure [i], dat$warn))
            fname <- paste0 (dat_prefix, structures$suffix [i])
            assign (fname, dat$obj)
            osm_data [[fname]] <- get (fname)
            setTxtProgressBar(pb, i / nrow (structures))
        }
        close (pb)
        cat ('That took ', (proc.time () - t0)[3], 's\n', sep='')

        structures <- structs_full
    }
    ns <- nrow (structures) - 1 # last row is background

    bg <- structures$col [structures$structure == 'background']
    plot_osm_basemap (bbox=bbox, filename=filename, bg=bg, width=width)
    for (i in seq (nrow (structures) - 1))
    {
        obji <- paste0 ('dat_', structures$suffix [i])
        add_osm_objects (osm_data [[obji]], col=structures$cols [i])
    }

    if (!is.null (filename)) 
        junk <- dev.off (which=dev.cur())

    return (osm_data)
}
