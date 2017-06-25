#' connect_highways
#'
#' Takes a list of highways names which must enclose an internal area, and returns
#' a \code{SpatialLines} object containing a sequence of OSM nodes which
#' cyclically connect all highways. Will fail if the streets do not form a
#' cycle.
#'
#' @param highways A vector of highway names passed directly to the Overpass
#' API. Wildcards and whitespaces are `.'; for other options see online help for
#' the overpass API.
#' @param bbox the bounding box for the map.  A 2-by-2 matrix of 4 elements with
#' columns of min and max values, and rows of x and y values.  
#' @param plot If \code{TRUE}, then all OSM data for each highway is plotted and
#' the final cycle overlaid.
#' @return A single set of \code{SpatialPoints} containing the lat-lon
#' coordinates of the cyclic line connecting all given streets.
#' @export
#'
#' @note \enumerate{
#' \item \code{connect_highways} is primarily intended to provide a means to
#' define boundaries of groups which can then be highlighted using
#' \code{\link{add_osm_groups}}.
#' \item This function can not be guaranteed failsafe owing both to the
#' inherently unpredictable nature of OpenStreetMap, as well as to the unknown
#' relationships between named highways. The \code{plot} option enables
#' problematic cases to be examined and hopefully resolved.  The function is
#' still experimental, so please help further improvements by reporting any
#' problems!
#' }
#'
#' @seealso \code{\link{add_osm_groups}}.
#'
#' @examples
#' bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
#' \dontrun{
#' highways <- c ('Monmouth.St', 'Short.?s.Gardens', 'Endell.St', 'Long.Acre',
#'                'Upper.Saint.Martin')
#' # Note that dots signify "anything", including whitespace and apostrophes, and
#' # that '?' denotes optional previous character and so here matches both 
#' # "Shorts Gardens" and "Short's Gardens"
#' highways1 <- connect_highways (highways = highways, bbox = bbox, plot = TRUE)
#' highways <- c ('Endell.St', 'High.Holborn', 'Drury.Lane', 'Long.Acre')
#' highways2 <- connect_highways (highways = highways, bbox = bbox, plot = TRUE)
#' }
#' # These are also part of the 'london' data provided with 'osmplotr':
#' highways1 <- london$highways1
#' highways2 <- london$highways2
#'
#' # Use of 'connect_highways' to highlight a region on a map
#' map <- osm_basemap (bbox = bbox, bg = 'gray20')
#' # dat_B <- extract_osm_data (key = 'building', value = '!residential', bbox = bbox)
#' # Those data are part of 'osmplotr':
#' dat_BNR <- london$dat_BNR # Non-residential buildings
#' groups <- list (london$highways1, london$highways2)
#' map <- add_osm_groups (map, obj = dat_BNR, groups = groups,
#'                        cols = c('red', 'blue'), bg = 'gray40')
#' print_osm_map (map)
connect_highways <- function (highways, bbox, plot = FALSE)
{
    if (missing (highways))
        stop ('A vector of highway names must be given')
    if (missing (bbox))
        stop ('A bounding box must be given')

    # Uses extract_highways to generate a list of highways, each component of
    # which is a spatially ordered list of distinct segments. Then uses
    # connect_highways to generate a fundamental cycle connecting all listed
    # highways (through forcing connections between highways if necessary).
    # This latter list simply means that components of each highway connect
    # cyclically with components of other highways, yet each highway itself may
    # still not necessarily internally connect. Thus the last remaining task of
    # this routine is to ensure that the internal components of each highway are
    # actually all connected.

    # Start by getting the list of highway components which have been
    # sequentially joined and ordered into a minimal set
    ways <- extract_highways (highway_names = highways, bbox = bbox)
    i0 <- which (sapply (ways, length) == 0)
    if (any (i0))
        for (i in i0)
            warning (highways [i], ' contains no data')
    while (any (sapply (ways, length) == 0))
    {
        i0 <- which (sapply (ways, length) == 0)
        ways [[i0 [1] ]] <- NULL
    }
    p4s <- attr (ways, "crs")

    ways <- flatten_highways (ways)
    ways <- get_highway_cycle (ways)

    if (plot)
        plot_highways (ways)

    conmat <- get_conmat (ways)
    cycles <- try (ggm::fundCycles (conmat), TRUE)
    res <- NULL
    if (is (attr (cycles, "condition"), "simpleError"))
        warning ('There are no cycles in the listed highways')
    else
    {
        cyc <- cycles [[which.max (sapply (cycles, nrow))]]

        # ***** Then calculate shortest path through the entire cycle
        paths <- sps_through_cycle (cyc, ways)
        nd_first <- rownames (paths [[1]]) [1]
        nd_last <- tail (rownames (tail (paths, n = 1) [[1]]), n = 1)
        if (nd_first != nd_last)
            warning ('No cycle able to be formed')
        else
        {
            path <- do.call (rbind, paths)

            if (plot)
                lines (path [, 1], path [, 2], lwd = 3, col = 'black', lty = 2)

            indx <- which (!duplicated (rownames (path)))
            res <- sp::SpatialPoints (path [indx, ])
            sp::proj4string (res) <- p4s
        }
    }

    return (res)
}

#' flatten_highways
#'
#' Connect any distinct components of a single highway into one single connected
#' component, inverting components where necessary
#'
#' @param ways List of highways returned from \code{extract_highways}, each
#' component of which will have more than one component only where distinct
#' components exist or for branches.
#'
#' @return Flattened List of highways, each component of which is a single
#' matrix.
#'
#' @noRd
flatten_highways <- function (ways)
{
    for (i in seq (ways))
    {
        while (length (ways [[i]]) > 1)
        {
            dmat <- ways_dist (ways [[i]])
            i12 <- which (dmat == min (dmat), arr.ind = TRUE)
            ways [[i]] [[ i12 [1] ]] <- join_ways (ways [[i]] [[i12 [1] ]],
                                                   ways [[i]] [[i12 [2] ]])
            ways [[i]] [[ i12 [2] ]] <- NULL
        }
        if (is.list (ways [[i]]))
            ways [[i]] <- ways [[i]] [[1]]
    }

    return (ways)
}

# Distances between a single list of ways
ways_dist <- function (ways)
{
    dmat <- array (Inf, dim = rep (length (ways), 2))
    combs <- combn (length (ways), 2)
    for (j in seq (ncol (combs)))
    {
        dmat [combs [1, j], combs [2, j]] <-
            haversine (ways [[combs [1, j] ]],
                       ways [[combs [2, j] ]]) [3]
    }

    return (dmat)
}

# connect way2 to way1 at closest nodes, flipping where needed and including the
# longest components
join_ways <- function (way1, way2)
{
    hs <- haversine (way1, way2)
    way1 <- flip_joining_way (way1, hs [1],
                              flip = (hs [1] < (nrow (way1) / 2)))
    way2 <- flip_joining_way (way2, hs [2],
                              flip = (hs [2] > (nrow (way2) / 2)))

    way <- rbind (way1, way2)
    way [!duplicated (way), ]
}

# extract longest part of way from join point i
flip_joining_way <- function (way, i, flip = FALSE)
{
    if (i < (nrow (way) / 2))
        way <- way [i:nrow (way), ]
    else
        way <- way [1:i, ]

    if (flip)
        way <- apply (way, 2, rev)

    return (way)
}


#' haversine
#'
#' Returns the minimal haversine distance between 2 ways, along with the
#' element numbers in each way corresponding to that minimal distance
#'
#' @param way1 A matrix or data frame of spatial coordinates
#' @param way2 A matrix or data frame of spatial coordinates
#' @return Vector of 3 elements: numbers of elements in (way1, way2)
#' corresponding to minimal distance, and the distance itself.
#'
#' @noRd
haversine <- function (way1, way2)
{
    x1 <- array (way1 [, 1], dim = c(nrow (way1), nrow (way2)))
    y1 <- array (way1 [, 2], dim = c(nrow (way1), nrow (way2)))
    x2 <- t (array (way2 [, 1], dim = c(nrow (way2), nrow (way1))))
    y2 <- t (array (way2 [, 2], dim = c(nrow (way2), nrow (way1))))
    # haversine distances:
    xd <- (x2 - x1) * pi / 180
    yd <- (y2 - y1) * pi / 180
    d <- sin (yd / 2) * sin (yd / 2) + cos (y2 * pi / 180) *
        cos (y1 * pi / 180) * sin (xd / 2) * sin (xd / 2)
    d <- 2.0 * atan2 (sqrt (d), sqrt (1.0 - d))
    d <- 6371 * d
    i1 <- which.min (apply (d, 1, min))
    i2 <- which.min (apply (d, 2, min))
    c (i1, i2, min (d))
}


#' one_shortest_path
#'
#' shortest path along one part of highway cycle
#'
#' @noRd
one_shortest_path <- function (ways, w0, wf, wt)
{
    # Find the components of [[w0]] which connect to [[wf]] & [[wt]]
    w0f <- which (rownames (ways [[w0]]) %in% rownames (ways [[wf]]))
    w0t <- which (rownames (ways [[w0]]) %in% rownames (ways [[wt]]))
    res <- NULL
    if (!is.null (w0f) & !is.null (w0t))
    {
        # w0f & w0t may have > 1 element
        if (min (w0f) > max (w0t))
        {
            wtmp <- w0f
            w0f <- w0t
            w0t <- wtmp
        }
        w0f <- max (w0f)
        w0t <- min (w0t)

        res <- ways [[w0]] [w0f:w0t, ]
        # invert so it can be rbind'ed directly onto next (wt)
        if (!tail (rownames (res), 1) %in% rownames (ways [[wt]]))
            res <- apply (res, 2, rev)
    }

    return (res)
}


plot_highways <- function (ways)
{
    xy <- do.call (rbind, ways)

    par (mar = rep (0, 4))
    plot (NULL, NULL, xlim = range (xy [, 1]), ylim = range (xy [, 2]),
          xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', frame = FALSE)

    cols <- rainbow (length (ways))

    for (i in seq (ways))
    {
        x <- ways [[i]] [, 1]
        y <- ways [[i]] [, 2]
        lines (x, y, col = cols [i])
        text (mean (x), mean (y), labels = i, col = cols [i])
    }
}

#' get_conmat
#'
#' Get connection matrix between a list of ways
#'
#' @param ways List of ways to be connected
#'
#' @return Binary connectivity matrix between all ways
#'
#' @noRd
get_conmat <- function (ways)
{
    conmat <- array (FALSE, dim = rep (length (ways), 2))

    for (i in seq (ways))
    {
        ref <- ways
        ref [[i]] <- NULL
        indx <- (seq (ways)) [!(seq (ways)) %in% i]

        # Then find which lines from ref intersect with ways [[i]]:
        ni <- unlist (lapply (ref, function (x)
                              intersect_ref_test (x, ways [[i]])))
        indx2 <- indx [which (!is.na (ni))]
        conmat [i, indx2] <- conmat [indx2, i] <- TRUE
    }

    return (conmat)
}


#' get intersection between a single way and the flat list of all ways
#'
#' @param x a single component of a single way
#' @param test The flat list of all components of all ways (excluding x)
#'
#' @note If there is more than one common vertex, only the first is taken.
#' TODO: Implement an alternative approach?
#'
#' @noRd
intersect_ref_test <- function (x, test)
{
    n <- array (x %in% test, dim = dim (x))
    n <- which (rowSums (n) == 2) [1]
    rownames (x) [n]
}

#' shortest paths along each component of cycle
#'
#' @param cyc Cycle extracted from \code{extract_cycle}
#' @param ways List of ways to be connected
#'
#' @return List of directly-connected shortest-paths along each part of the
#' cycle.
#'
#' @noRd
sps_through_cycle <- function (cyc, ways)
{
    cyc <- rbind (cyc, cyc [1, ])
    paths <- list ()
    for (i in seq (nrow (cyc) - 1))
    {
        w0 <- cyc [i, 2] # the current way
        wf <- cyc [i, 1] # the 'from' way
        wt <- cyc [i + 1, 2] # the 'to' way

        paths [[i]] <- one_shortest_path (ways, w0, wf, wt)
    } # end for i over nrow (cyc)

    return (paths)
}
