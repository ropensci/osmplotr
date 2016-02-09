#' highways2polygon
#'
#' takes a list of highways names which must enclose an internal area, and returns
#' a SpatialLines object containing a sequence of OSM nodes which cyclically
#' connect all highways. Will fail if the streets do not form a cycle.
#'
#' @param highways = A vector of highway names passed directly to the Overpass
#' API. Wildcards and whitespaces are '.'; for other options see overpass help.
#' @param bbox = the bounding box within which to look for highways. 
#' Must be a vector of 4 elements (xmin, ymin, xmax, ymax).
#' Default is a small part of central London.
#' @param plot if TRUE, then all OSM data for each highway is plotted (with
#' lwds[1], cols[1]), with the final cycle overlaid (with lwds[2], cols[2]).
#' @param lwds = line widths for (all highways, final cycle)
#' @param cols = line colours for (all highways, final cycle)
#' @return A single data.frame containing the lat-lon coordinates of the cyclic
#' line connecting all given streets.

highways2polygon <- function (highways=NULL, bbox=NULL, 
                             plot=FALSE, lwds=c(1,3), cols=c("black","red"))
{
    if (is.null (highways))
        stop ("A vector of highway names must be given")
    if (is.null (bbox))
        stop ("A bounding box must be given")

    # Uses extract_highways to generate a list of highways, each component of
    # which is a spatially ordered list of distinct segments. Then uses
    # connect_highways to generate a fundamental cycle connecting all listed
    # highways (through forcing connections between highways if necessary).
    # This latter list simply means that components of each highway connect
    # cyclically with components of other highways, yet each highway itself may
    # still not necessarily internally connect. Thus the last remaining task of
    # this routine is to ensure that the internal components of each highway are
    # actually all connected.

    # Start by getting the sequentially ordered list of highways and connecting
    # them:
    ways <- extract_highways (highway_names=highways, bbox=bbox)
    ways <- connect_highways (ways)

    if (plot)
    {
        xlims <- range (sapply (ways, function (i) 
                            range (sapply (i, function (j) range (j [,1])))))
        ylims <- range (sapply (ways, function (i) 
                            range (sapply (i, function (j) range (j [,2])))))
        plot.new ()
        par (mar=rep (0, 4))
        plot (NULL, NULL, xlim=xlims, ylim=ylims, xaxt="n", yaxt="n",
              xlab="", ylab="", frame=FALSE)
        for (i in seq (ways))
            for (j in ways [[i]])
                lines (j [,1], j [,2], col=cols [1], lwd=lwds [1])
    }

    # Extract the cycle as established in connect_highways
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
    cycles <- ggm::fundCycles (conmat)
    if (is.null (cycles))
        stop ("There are no cycles in the listed highways")
    i <- which.max (sapply (cycles, nrow))
    cyc <- cycles [[i]]

    # ***** Then calculate shortest paths along each part of the cycle
    cyc_len <- nrow (cyc)
    cyc <- rbind (cyc, cyc [1,])
    paths <- list ()
    for (i in seq (cyc_len))
    {
        w0 <- cyc [i,2] # the current way
        wf <- cyc [i,1] # the "from" way
        wt <- cyc [i+1,2] # the "to" way

        # Find the components of [[w0]] which connect to [[wf]] & [[wt]], along
        # with the node names
        wf_flat <- do.call (rbind, ways [[wf]])
        w0f <- sapply (ways [[w0]], function (x) 
                       max (rowSums (array (x %in% wf_flat, dim=dim(x)))))
        if (max (w0f) < 2)
            stop ("Error: way [[", w0, "]] does not join any others")
        w0f_names <- sapply (ways [[w0]], function (x) rownames (x)
                [which (rowSums (array (x %in% wf_flat, dim=dim (x))) == 2)])
        w0f_names <- unique (unlist (w0f_names)) # names of nodes which join [[wf]]
        w0f <- which (w0f == 2) # bit(s) of [[w0]] which connect to [[wf]]
        # w0f == 2 for parts of [[w0]] that are in wf
        wt_flat <- do.call (rbind, ways [[wt]])
        w0t <- sapply (ways [[w0]], function (x) 
                       max (rowSums (array (x %in% wt_flat, dim=dim(x)))))
        if (max (w0t) < 2)
            stop ("Error: way [[", w0, "]] does not join any others")
        w0t_names <- sapply (ways [[w0]], function (x) rownames (x)
                [which (rowSums (array (x %in% wt_flat, dim=dim (x))) == 2)])
        w0t_names <- unique (unlist (w0t_names))
        w0t <- which (w0t == 2)

        # Then find *longest* shortest path in [[w0]] between w0f_names and
        # w0t_names. Start by storing ways [[w0]] [[w0t]] & [[w0]]
        # [[w0f]]---that is, the bits of [[w0]] which connect with [[wt]] and
        # [[wf]]---as an igraph
        indx <- c (w0f, w0t)
        from <- unlist (lapply (indx, function (x) rownames (ways [[w0]] [[x]])
                                    [1:(nrow (ways [[w0]] [[x]]) - 1)]))
        to <- unlist (lapply (indx, function (x) rownames (ways [[w0]] [[x]])
                                    [2:nrow (ways [[w0]] [[x]])]))
        g <- igraph::graph_from_edgelist (cbind (from, to), directed=FALSE)
        
        from_node_list <- rep (w0f_names, length (w0t_names))
        to_node_list <- rep (w0t_names, each=length (w0f_names))
        maxlen <- 0
        fromi <- toi <- NA
        for (j in seq (from_node_list))
        {
            sp <- suppressWarnings (igraph::shortest_paths (g,
                                    from_node_list [j], to_node_list [j]))
            sp <- sp$vpath [[1]]
            if (length (sp) > maxlen)
            {
                maxlen <- length (sp)
                fromi <- from_node_list [j]
                toi <- to_node_list [j]
            }
        }
        # If the two segments of [[w0]] do not join, then force a join. Do this
        # by finding the shortest *geographic* distance between [from] and [to]
        # segments
        if (maxlen == 0)
        {
            xfrom <- sapply (w0f, function (x) ways [[w0]] [x])
            xfrom <- do.call (rbind, xfrom)
            xto <- sapply (w0t, function (x) ways [[w0]] [x])
            xto <- do.call (rbind, xto)
            x1 <- array (xfrom [,1], dim=c(nrow (xfrom), nrow (xto)))
            y1 <- array (xfrom [,2], dim=c(nrow (xfrom), nrow (xto)))
            x2 <- t (array (xto [,1], dim=c(nrow (xto), nrow (xfrom))))
            y2 <- t (array (xto [,2], dim=c(nrow (xto), nrow (xfrom))))
            # Haversand distances:
            xd <- (x2 - x1) * pi / 180;
            yd <- (y2 - y1) * pi / 180;
            d <- sin (yd / 2) * sin (yd / 2) + cos (y2 * pi / 180) *
                cos (y1 * pi / 180) * sin (xd / 2) * sin (xd / 2);
            d <- 2.0 * atan2 (sqrt (d), sqrt (1.0 - d));
            d <- d * 6371
            # Find names and (lat,lon) of closest points:
            ifrom <- which.min (apply (d, 1, min))
            ito <- which.min (apply (d, 2, min))
            name_from <- rownames (xfrom) [ifrom]
            xy_from <- xfrom [ifrom,]
            name_to <- rownames (xto) [ito]
            xy_to <- xto [ito,]

            # Extract the elements of ways [[w0]] that start or end in name_from
            # (which may have length>1), and plug name_to on to ends or starts:
            ifrom <- unlist (sapply (ways [[w0]], function (x) 
                            max (c (-1, which (rownames (x) == name_from)))))
            # Check that ifrom match end points, rather than intermediate points
            nodefrom <- ifrom [which (ifrom > 0)]
            ifrom <- which (ifrom > 0)
            nrows <- sapply (ifrom, function (x) nrow (ways [[w0]] [[x]]))
            # And only keep cases where name_from is a terminal node
            indx <- which (nodefrom == 1 | nodefrom == nrows)
            nodefrom <- nodefrom [indx]
            ifrom <- ifrom [indx]

            for (j in 1:length (nodefrom))
            {
                if (nodefrom [j] == 1)
                {
                    rnames <- c (name_to, rownames (ways [[w0]] [[ifrom [j] ]]))
                    ways [[w0]] [[ifrom [j] ]] <- 
                        rbind (xy_to, ways [[w0]] [[ifrom [j] ]])
                    rownames (ways [[w0]] [[ifrom [j] ]]) <- rnames
                } else
                {
                    rnames <- c (rownames (ways [[w0]] [[ifrom [j] ]]), name_to)
                    ways [[w0]] [[ifrom [j] ]] <- 
                        rbind (ways [[w0]] [[ifrom [j] ]], xy_to)
                    rownames (ways [[w0]] [[ifrom [j] ]]) <- rnames
                }
            } # end for j over nodefrom

            ito <- unlist (sapply (ways [[w0]], function (x) 
                            max (c (-1, which (rownames (x) == name_to)))))
            nodeto <- ito [which (ito > 0)]
            ito <- which (ito > 0)
            nrows <- sapply (ito, function (x) nrow (ways [[w0]] [[x]]))
            # And only keep cases where name_from is a terminal node
            indx <- which (nodeto == 1 | nodeto == nrows)
            nodeto <- nodeto [indx]
            ito <- ito [indx]

            for (j in 1:length (nodeto))
            {
                if (nodeto [j] == 1)
                {
                    rnames <- c (name_from, rownames (ways [[w0]] [[ito [j] ]]))
                    ways [[w0]] [[ito [j] ]] <- 
                        rbind (xy_to, ways [[w0]] [[ito [j] ]])
                    rowname (ways [[w0]] [[ito [j] ]]) <- rnames
                } else
                {
                    rnames <- c (rownames (ways [[w0]] [[ito [j] ]]), name_from)
                    ways [[w0]] [[ito [j] ]] <- 
                        rbind (ways [[w0]] [[ito [j] ]], xy_to)
                    rownames (ways [[w0]] [[ito [j] ]]) <- rnames
                }
                # It is possible adding _from duplicates _to from above, so
                ways [[w0]] [[ito [j] ]] <- unique (ways [[w0]] [[ito [j] ]])
            } # end for j over nodeto
        } # end if (maxlen == 0)

        # Store objs [[w0]] as an igraph
        from <- unlist (lapply (ways [[w0]], function (x) 
                                           rownames (x)[1:(nrow(x)-1)]))
        to <- unlist (lapply (ways [[w0]], function (x) 
                                           rownames (x)[2:nrow(x)]))
        g <- igraph::graph_from_edgelist (cbind (from, to), directed=FALSE)
        if (is.na (fromi) | is.na (toi))
        {
            # This will happen if there was no path possible above,
            # necessitating the (maxlen == 0) clause.
            for (j in seq (from_node_list))
            {
                sp <- suppressWarnings (igraph::shortest_paths (g,
                                        from_node_list [j], to_node_list [j]))
                sp <- sp$vpath [[1]]
                if (length (sp) > maxlen)
                {
                    maxlen <- length (sp)
                    fromi <- from_node_list [j]
                    toi <- to_node_list [j]
                }
            }
        }

        # Then finally the desired shortest path extraction
        sp <- suppressWarnings (igraph::shortest_paths (g, fromi, toi))
        sp <- sp$vpath [[1]]
        # Then get coordinates of the shortest-longest path.
        # Note that junctions may have duplicates of same node, but match only
        # returns the first match, so that's okay.
        street <- do.call (rbind, ways [[w0]])
        indx <- match (names (sp), rownames (street))
        stopifnot (all (!is.na (indx)))
        path <- street [indx,]

        paths [[i]] <- path
    } # end for i over cyc_len

    # Finally, connect paths together to form desired cyclic boundary.  This
    # involves first checking whether any of the paths need to be flipped 
    for (i in 1:(length (paths) - 1))
    {
        n <- which (rowSums (array (paths [[i]] %in% paths [[i+1]], 
                                    dim=dim (paths [[i]]))) == 2)
        # n is the index into paths [[i]] of nodes occuring in paths [[i+1]].
        # This obviously should be nrow (paths [[i]]), so:
        if (n == 1)
            paths [[i]] <- apply (t (paths [[i]]), 1, rev) # flip
    }
    path <- do.call (rbind, paths)

    if (plot)
        lines (path [,1], path [,2], lwd=lwds [2], col=cols [2])

    return (unique (path))
}
