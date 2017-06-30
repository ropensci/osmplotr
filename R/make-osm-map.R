#' make_osm_map
#'
#' Makes an entire OSM map for the given bbox using the submitted data, or by
#' downloading data if none submitted. This is a convenience function enabling
#' an entire map to be produced according to the graphical format specified with
#' the \code{structures} argument.  
#' 
#' @param bbox The bounding box for the map.  A 2-by-2 matrix of 4 elements with
#' columns of min and max values, and rows of x and y values.  If \code{NULL},
#' \code{bbox} is taken from the largest extent of OSM objects in
#' \code{osm_data}.
#' @param osm_data A list of OSM objects as returned from
#' \code{\link{extract_osm_objects}}.  These objects may be included in the plot
#' without downloading. These should all be named with the stated
#' \code{dat_prefix} and have suffixes as given in \code{structures}.
#' @param structures A \code{data.frame} specifying types of OSM structures as
#' returned from \code{\link{osm_structures}}, and potentially modified to alter
#' lists of structures to be plotted, and their associated colours. Objects are
#' overlaid on plot according to the order given in \code{structures}.
#' @param dat_prefix Prefix for data structures (default \code{dat_}). Final data
#' structures are created by appending the suffixes from
#' \code{\link{osm_structures}}.
#' @return List of two components: 
#' \enumerate{
#'   \item List of OSM structures each as
#'      \code{Spatial(Points/Lines/Polygons)DataFrame} and appended to
#'      \code{osm_data} (which is \code{NULL} by default), and 
#'   \item The \code{map} as a \code{ggplot2} object
#' }
#' @export
#'
#' @section Note:
#' If \code{osm_data} is not given, then data will be downloaded, which can take
#' some time.  Progress is dumped to screen.
#'
#' @seealso \code{\link{osm_basemap}}, \code{\link{add_osm_objects}}.
#'
#' @examples
#' structures <- c ('highway', 'park')
#' structs <- osm_structures (structures = structures, col_scheme = 'light')
#' # make_osm_map returns potentially modified list of data using the provided
#' # 'london' data for highways and parks.
#' dat <- make_osm_map (osm_data = london, structures = structs)
#' # or download data automatically using a defined bounding boox
#' bbox <- get_bbox (c(-0.15,51.5,-0.10,51.52))
#' \dontrun{
#' dat <- make_osm_map (bbox = bbox, structures = structs)
#' print_osm_map (dat$map)
#' }
make_osm_map <- function (bbox, osm_data,
                          structures = osm_structures (), dat_prefix = 'dat_')
{
    if (missing (bbox))
        bbox <- get_bbox_from_data (osm_data)

    sfx <- structures$suffix [1:(nrow (structures) - 1)]
    if (missing (osm_data))
    {
        structs_new <- seq (nrow (structures) - 1)
        osm_data <- list ()
    } else
        structs_new <- which (!sapply (sfx, function (i)
                           any (paste0 (dat_prefix, i) %in% names (osm_data))))

    if (length (structs_new) > 0)
    {
        md <- get_missing_osm_data (osm_data, structures [structs_new, ],
                                    bbox, dat_prefix)

        indx <- c (md$indx, nrow (structures))
        structures <- structures [indx, ]
        osm_data <- c (osm_data, md$osm_data)
    }
    ns <- nrow (structures) - 1 # last row is background
    if (ns == 0)
        stop ('Downloads contain no data')

    bg <- structures$col [structures$structure == 'background']
    map <- osm_basemap (bbox = bbox, bg = bg)
    for (i in seq (ns))
    {
        obji <- paste0 ('dat_', structures$suffix [i])
        map <- add_osm_objects (map, osm_data [[obji]],
                                col = structures$cols [i])
    }

    list (osm_data = osm_data, map = map)
}

get_bbox_from_data <- function (osm_data)
{
    if (missing (osm_data))
        stop ('Either bounding box or osm_data must be given')
    bbox <- matrix (c (Inf, Inf, -Inf, -Inf), nrow = 2, ncol = 2)
    rownames (bbox) <- c ('x', 'y')
    sp_classes <- c ('SpatialLinesDataFrame', 'SpatialPolygonsDataFrame',
                  'SpatialPointsDataFrame')
    for (i in osm_data)
    {
        if (is (i, 'sf'))
        {
            bbi <- attr (i$geometry, 'bbox')
            if (bbi [1] < bbox [1, 1]) bbox [1, 1] <- bbi [1]
            if (bbi [2] < bbox [2, 1]) bbox [2, 1] <- bbi [2]
            if (bbi [1] > bbox [1, 2]) bbox [1, 2] <- bbi [1]
            if (bbi [2] > bbox [2, 2]) bbox [2, 2] <- bbi [2]
        } else if (any (class (i) %in% sp_classes))
        {
            bbi <- slot (i, 'bbox')
            if (bbi [1, 1] < bbox [1, 1]) bbox [1, 1] <- bbi [1, 1]
            if (bbi [2, 1] < bbox [2, 1]) bbox [2, 1] <- bbi [2, 1]
            if (bbi [1, 2] > bbox [1, 2]) bbox [1, 2] <- bbi [1, 2]
            if (bbi [2, 2] > bbox [2, 2]) bbox [2, 2] <- bbi [2, 2]
        }
    }

    return (bbox)
}

get_missing_osm_data <- function (osm_data, structures, bbox, dat_prefix)
{
    cat ('Downloading and extracting OSM data for',
         nrow (structures), 'structures ...\n')
    pb <- txtProgressBar (max = 1, style = 3)
    t0 <- proc.time ()
    indx <- NULL
    for (i in seq (nrow (structures)))
    {
        if (structures$value [i] == "")
            dat <- extract_osm_objects (key = structures$key [i],
                                        bbox = bbox)
        else
            dat <- extract_osm_objects (key = structures$key [i],
                                        value = structures$value [i],
                                        bbox = bbox)

        if (nrow (dat) > 0)
        {
            fname <- paste0 (dat_prefix, structures$suffix [i])
            assign (fname, dat)
            osm_data [[fname]] <- get (fname)
            indx <- c (indx, i)
        }
        setTxtProgressBar(pb, i / nrow (structures))
    }
    close (pb)
    cat ('That took ', (proc.time () - t0)[3], 's\n', sep = '')

    list ('indx' = indx, 'osm_data' = osm_data)
}
