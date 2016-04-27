#' connect_highways
#'
#' Takes a list of highways names which must enclose an internal area, and returns
#' a SpatialLines object containing a sequence of OSM nodes which cyclically
#' connect all highways. Will fail if the streets do not form a cycle.
#'
#' @param highways A vector of highway names passed directly to the Overpass
#' API. Wildcards and whitespaces are '.'; for other options see online help for
#' the overpass API.
#' @param bbox the bounding box for the map.  A 2-by-2 matrix of 4 elements with
#' columns of min and max values, and rows of x and y values.  
#' @param plot If TRUE, then all OSM data for each highway is plotted and the
#' final cycle overlaid.
#' @return A single set of 'SpatialPoints' containing the lat-lon coordinates of
#' the cyclic line connecting all given streets.
#' @export
#'
#' @note \enumerate{
#' \item 'connect_highways' is primarily intended to provide a means to define
#' boundaries of groups which can then be highlighted using 'add_osm_groups'.
#' \item This function can not be guaranteed failsafe owing both to the
#' inherently unpredictable nature of OpenStreetMap, as well as to the unknown
#' relationships between named highways. The `plot` option enables problematic
#' cases to be examined and hopefully resolved.  The function is still
#' experimental, so please help further improvements by reporting any problems!
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
#' highways1 <- connect_highways (highways=highways, bbox=bbox, plot=TRUE)
#' highways <- c ('Endell.St', 'High.Holborn', 'Drury.Lane', 'Long.Acre')
#' highways2 <- connect_highways (highways=highways, bbox=bbox, plot=TRUE)
#' }
#' # These are also part of the 'london' data provided with 'osmplotr':
#' highways1 <- london$highways1
#' highways2 <- london$highways2
#'
#' # Use of 'connect_highways' to highlight a region on a map
#' map <- plot_osm_basemap (bbox=bbox, bg='gray20')
#' # dat_B <- extract_osm_data (key='building', value='!residential', bbox=bbox)
#' # Those data are part of 'osmplotr':
#' dat_BNR <- london$dat_BNR # Non-residential buildings
#' groups <- list (london$highways1, london$highways2)
#' map <- add_osm_groups (map, obj=dat_BNR, groups=groups,
#'                        cols=c('red', 'blue'), bg='gray40')
#' print (map)


connect_highways <- function (highways, bbox, plot=FALSE)
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

    # Start by getting the sequentially ordered list of highways, exluding any
    # components, and connecting them:
    ways <- extract_highways (highway_names=highways, bbox=bbox)
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
    #if (!is.null (exclude))
    #{
    #    exclude <- sort (exclude, decreasing=TRUE)
    #    i <- floor (exclude)
    #    j <- 10 * (exclude - i)
    #    for (k in seq (length (i)))
    #        ways [[i [k] ]] [[j [k] ]] <- NULL
    #}
    ways <- get_highway_cycle (ways)

    if (plot)
    {
        xlims <- range (sapply (ways, function (i) 
                            range (sapply (i, function (j) range (j [,1])))))
        ylims <- range (sapply (ways, function (i) 
                            range (sapply (i, function (j) range (j [,2])))))
        #plot.new ()
        par (mar=rep (0, 4))
        plot (NULL, NULL, xlim=xlims, ylim=ylims, xaxt='n', yaxt='n',
              xlab='', ylab='', frame=FALSE)
        cols <- rainbow (length (ways))
        for (i in seq (ways))
            for (j in seq (ways [[i]]))
            {
                x <- ways [[i]] [[j]] [,1]
                y <- ways [[i]] [[j]] [,2]
                n <- length (x)
                lines (x, y, col=cols [i])
                text (x [1], y [1], labels=paste0 (i, '.', j), col=cols [i])
                text (x [n], y [n], labels=paste0 (i, '.', j), col=cols [i])
            }
    }

    # Extract the cycle as established in get_highway_cycle
    conmat <- array (FALSE, dim=rep (length (ways), 2))
    for (i in seq (ways))
    {
        test <- do.call (rbind, ways [[i]])
        ref <- ways
        ref [[i]] <- NULL
        indx <- (1:length (ways)) [!(1:length (ways)) %in% i]
        # Then find which lines from ref intersect with test:
        ni <- unlist (lapply (ref, function (x) {
                      xflat <- do.call (rbind, x)
                      n <- array (xflat %in% test, dim=dim (xflat))
                      # NOTE: If there is more than one common vertex, only the
                      # first is taken. TODO: Check alternatives!
                      n <- which (rowSums (n) == 2) [1]
                      rownames (xflat) [n]
                     }))
        indx2 <- indx [which (!is.na (ni))]
        conmat [i, indx2] <- conmat [indx2, i] <- TRUE
    }
    cycles <- try (ggm::fundCycles (conmat), TRUE)
    if (is (attr (cycles, "condition"), "simpleError"))
        stop ('There are no cycles in the listed highways')
    cyc <- cycles [[which.max (sapply (cycles, nrow))]]

    # ***** Then calculate shortest paths along each part of the cycle
    cyc <- rbind (cyc, cyc [1,])
    paths <- list ()
    for (i in seq (nrow (cyc) - 1))
    {
        w0 <- cyc [i,2] # the current way
        wf <- cyc [i,1] # the 'from' way
        wt <- cyc [i+1,2] # the 'to' way

        # Find the components of [[w0]] which connect to [[wf]] & [[wt]], along
        # with the node names
        wf_flat <- do.call (rbind, ways [[wf]])
        w0f <- sapply (ways [[w0]], function (x) 
                       max (rowSums (array (x %in% wf_flat, dim=dim(x)))))
        if (max (w0f) < 2)
            stop ('Error: way [[', w0, ']] does not join any others')
        w0f_names <- sapply (ways [[w0]], function (x) rownames (x)
                [which (rowSums (array (x %in% wf_flat, dim=dim (x))) == 2)])
        w0f_names <- unique (unlist (w0f_names)) 
        w0f <- which (w0f == 2) 
        # w0f_names holds names of nodes which join [[wf]], while w0f indexes
        # bit(s) of [[w0]] which connect to [[wf]], with w0f == 2 for parts of
        # [[w0]] that are in wf.
        # TODO: Deal with multiple w0f / w0f_names

        wt_flat <- do.call (rbind, ways [[wt]])
        w0t <- sapply (ways [[w0]], function (x) 
                       max (rowSums (array (x %in% wt_flat, dim=dim(x)))))
        if (max (w0t) < 2)
            stop ('Error: way [[', w0, ']] does not join any others')
        w0t_names <- sapply (ways [[w0]], function (x) rownames (x)
                [which (rowSums (array (x %in% wt_flat, dim=dim (x))) == 2)])
        w0t_names <- unique (unlist (w0t_names))
        w0t <- which (w0t == 2)

        sp <- shortest_way (ways [[w0]], w0f_names, w0t_names)
        # If there is no connection between the components containing w0f_names
        # and w0t_names, then sp is NULL. Components are then sequentially
        # connected by joining the two at the shortest distance and
        # re-calculating until a shortest path is possible.
        if (is.null (sp))
        {
            # Start by making a connection matrix between the components of w0,
            # in this case holding distances between components, with default of
            # Inf so shortest non-zero distances can be found. nodes holds
            # indices (from, to) subsequently connected segments.
            conmat_way <- nodes <- array (Inf, dim=rep (length (ways [[w0]]), 2))
            combs <- combn (length (ways [[w0]]), 2)
            for (j in seq (ncol (combs)))
            {
                way1 <- ways [[w0]] [[combs [1, j] ]]
                way2 <- ways [[w0]] [[combs [2, j] ]]
                shared_nodes <- array (way1 %in% way2, dim=dim (way1))
                shared_nodes <- which (rowSums (shared_nodes) == 2)
                if (length (shared_nodes) == 0)
                {
                    hs <- haversand (way1, way2)
                    conmat_way [combs [1, j], combs [2, j]] <- 
                        conmat_way [combs [2, j], combs [1, j]] <- hs [3]
                    nodes [combs [1, j], combs [2, j]] <- hs [1]
                    nodes [combs [2, j], combs [1, j]] <- hs [2]
                }
            } # end for j over combs
        }
        # Having established conmat_way, the successively connect the closest
        # segments until a shortest path is found. 
        while (is.null (sp))
        {
            i1 <- which.min (apply (conmat_way, 1, min))
            i2 <- which.min (conmat_way [i1, ])
            way1 <- ways [[w0]] [[i1]]
            way2 <- ways [[w0]] [[i2]]
            hs <- haversand (way1, way2)
            xy1 <- way1 [hs [1],]
            xy2 <- way2 [hs [2],]
            # Only connect components at terminal nodes:
            if (hs [1] == 1)
            {
                rnames <- c (rownames (way2) [hs [2]], rownames (way1))
                way1 <- rbind (xy2, way1)
                rownames (way1) <- rnames
            } else if (hs [1] == nrow (way1))
            {
                rnames <- c (rownames (way1), rownames (way2) [hs [2]])
                way1 <- rbind (way1, xy2)
                rownames (way1) <- rnames
            } else if (hs [2] == 1)
            {
                rnames <- c (rownames (way1) [hs [1]], rownames (way2))
                way2 <- rbind (xy1, way2)
                rownames (way2) <- rnames
            } else if (hs [2] == nrow (way2))
            {
                rnames <- c (rownames (way2), rownames (way1) [hs [1]])
                way2 <- rbind (way2, xy1)
                rownames (way2) <- rnames
            }
            ways [[w0]] [[i1]] <- way1
            ways [[w0]] [[i2]] <- way2
            # conmat_way for that pair of components is set to Inf whether or
            # not connecting nodes were terminal. (Non-terminal cases will arise
            # for example for parallel lanes which do not cross, yet have very
            # low distances between them.)
            #conmat_way [i1, i2] <- conmat_way [i2, i1] <- Inf
            # This stop should never happen:
            if (all (!is.finite (conmat_way)))
                stop (paste0 ('Segments of way#', i, ' cannot be joined'))
            sp <- shortest_way (ways [[w0]], w0f_names, w0t_names)
        } # end if (maxlen == 0)
        paths [[i]] <- sp
    } # end for i over nrow (cyc)

    # Finally, connect paths together to form desired cyclic boundary.  This
    # involves first checking whether any of the paths need to be flipped 
    for (i in seq (length (paths) - 1))
    {
        n <- which (rowSums (array (paths [[i]] %in% paths [[i+1]], 
                                    dim=dim (paths [[i]]))) == 2)
        # n is the index into paths [[i]] of nodes occuring in paths [[i+1]].
        # This obviously should be nrow (paths [[i]]) (although there can also
        # be multiple values) so:
        if (min (n) == 1)
            paths [[i]] <- apply (t (paths [[i]]), 1, rev) # flip
    }
    path <- do.call (rbind, paths)

    if (plot)
        lines (path [,1], path [,2], lwd=3, col='black', lty=2)

    indx <- which (!duplicated (rownames (path)))
    res <- sp::SpatialPoints (path [indx,])
    proj4string (res) <- p4s

    return (res)
}


#' haversand
#'
#' Returns the minimal Haversand distance between 2 ways, along with the
#' element numbers in each way corresponding to that minimal distance
#'
#' @param way1 A matrix or data frame of spatial coordinates
#' @param way2 A matrix or data frame of spatial coordinates
#' @return Vector of 3 elements: numbers of elements in (way1, way2)
#' corresponding to minimal distance, and the distance itself.
haversand <- function (way1, way2)
{
    x1 <- array (way1 [,1], dim=c(nrow (way1), nrow (way2)))
    y1 <- array (way1 [,2], dim=c(nrow (way1), nrow (way2)))
    x2 <- t (array (way2 [,1], dim=c(nrow (way2), nrow (way1))))
    y2 <- t (array (way2 [,2], dim=c(nrow (way2), nrow (way1))))
    # Haversand distances:
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

#' shortest_way
#'
#' returns the shortest path between node_from and node_to (both as names
#' of nodes), or NULL if node_from and node_to are not connected.
#'
#' @param way A single highway (as a list of OSM components)
#' @param node_from The ID of a node in way from which to calculate the
#' shortest_path
#' @param node_to The ID of a node in way towards which to calculate the
#' shortest_path
#' @return Shortest path as list of nodal IDs (or NULL if no shortest path)
shortest_way <- function (way, node_from, node_to)
{
    nf <- which (sapply (way, function (x) node_from %in% rownames (x)))
    nt <- which (sapply (way, function (x) node_to %in% rownames (x)))
    # make igraph of entire way
    from <- unlist (lapply (way, function (x) 
                            rownames (x) [1:(nrow (x) - 1)]))
    to <- unlist (lapply (way, function (x) rownames (x) [2:nrow (x)]))
    # graph_from_edgelist is @importFrom
    #g <- igraph::graph_from_edgelist (cbind (from, to), directed=FALSE)
    g <- graph_from_edgelist (cbind (from, to), directed=FALSE)

    from_node_list <- rep (node_from, length (node_to))
    to_node_list <- rep (node_to, each=length (node_from))
    maxlen <- 0
    fromi <- toi <- NA
    the_path <- NULL
    for (j in seq (from_node_list))
    {
        # shortest_paths is @importFrom
        #sp <- suppressWarnings (igraph::shortest_paths (g,
        #                        from_node_list [j], to_node_list [j]))
        sp <- suppressWarnings (shortest_paths (g,
                                from_node_list [j], to_node_list [j]))
        sp <- sp$vpath [[1]]
        if (length (sp) > maxlen)
        {
            maxlen <- length (sp)
            the_path <- names (sp)
        }
    }
    if (!is.null (the_path))
    {
        way_flat <- do.call (rbind, way)
        indx <- match (the_path, rownames (way_flat))
        stopifnot (all (!is.na (indx)))
        the_path <- way_flat [indx,]
    }
    return (the_path)
}

