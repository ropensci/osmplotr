#' streets2polygon
#'
#' takes a list of street names which must enclose an internal area, and returns
#' a SpatialLines object containing a sequence of OSM nodes which cyclically
#' connect all streets. Will fail if the streets do not form a cycle.
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

streets2polygon <- function (highways=NULL, bbox=NULL,
                             plot=FALSE, lwds=c(1,3), cols=c("black","red"))
{
    if (is.null (highways))
        stop ("A vector of highway names must be given")
    if (is.null (bbox))
        stop ("A bounding box must be given")

    # Proceeds through five stages:
    # (1) Download OSM data for highways 
    # (2) Order the individual OSM objects into a minimal number of discrete
    # sequences
    # (3) If they don't exist, add juction points to lines which geographically
    # cross. 
    # (4) Fill a connectivity matrix between all highways and extract the
    # *longest* cycle connecting them all
    # (5) Calculate shortest paths along each part of the cycle
    # (6) Connect paths together to form desired cyclic boundary

    # **** (1) Download OSM data for highways 
    #
    # Start by getting 2-letter abbreviations for each highway
    nletters <- 2
    waynames <- sapply (highways, function (x) 
                      tolower (substring (x, 1, nletters)))
    while (any (duplicated (waynames)))
    {
        nletters <- nletters + 1
        waynames <- sapply (highways, function (x) 
                          tolower (substring (x, 1, nletters)))
    }

    cat ("Downloading OSM data ...\n")
    pb <- txtProgressBar (max=1, style = 3) # shows start and end positions
    for (i in seq (highways))
    {
        dat <- extract_highway (name = highways [i], bbox=bbox)
        assign (waynames [i], dat)
        setTxtProgressBar(pb, i / length (highways))
    }
    rm (dat)
    close (pb)


    # ***** (2) Order the individual OSM objects into a minimal number of
    # *****     discrete sequences
    i0 <- 0 # Nodes in ordered lines are numbered sequentially from (i0+1)
    for (i in seq (highways))
    {
        dat <- order_lines (get (waynames [i]), i0=i0)
        assign (paste (waynames [i], "o", sep=""), dat)
        i0 <- max (unlist (lapply (dat, function (x) as.numeric (rownames (x)))))
    }

    # ***** (3) If they don't exist, add juction points to lines which
    # *****     geographically cross. 
    #
    # Start by constructing list of all street objects, and also get the maximum
    # vertex number, so new junction vertices can be numbered above that.
    objs <- NULL
    maxvert <- 0
    for (i in waynames)
    {
        objs [[i]] <- get (paste (i, "o", sep=""))
        maxvert <- max (maxvert, unlist (lapply (objs [[i]], function (x)
                                    max (as.numeric (rownames (x))))))
    }
    maxvert <- maxvert + 1

    # Each obj contains a list of all OSM objects corresponding to the given
    # street. These OSM objects then need to be modified through the addition of
    # junction points, requiring a double loop.
    for (i in seq (objs))
    {
        obji <- objs [[i]]
        test <- objs
        test [[i]] <- NULL
        test_flat <- do.call (c, test)
        # Check whether any of obji cross any of test_flat *and* don't already
        # exist as vertices
        for (j in seq (obji))
        {
            li <- sp::Line (obji [[j]])
            li <- sp::SpatialLines (list (Lines (list (li), ID="a"))) 
            # The following function returns default of -1 for no geometric
            # intersection; 0 where intersection exists but is *NOT* a vertex of
            # li, and 2 where intersection is a vertex of li.
            intersections <- sapply (test_flat, function (x) {
                        lj <- sp::Line (x)
                        lj <- sp::SpatialLines (list (Lines (list (lj), ID="a"))) 
                        int <- rgeos::gIntersection (li, lj)
                        if (!is.null (int))
                            sum (coordinates (int) %in% x)
                        else
                            -1
                        })
            if (any (intersections == 0))
            {
                # Then they have to be added to objs [[i]] [[j]]. 
                stopifnot (length (which (intersections == 0)) == 1)
                x <- test_flat [which (intersections == 0)] [[1]]
                lj <- sp::Line (x)
                lj <- sp::SpatialLines (list (Lines (list (lj), ID="a"))) 
                xy <- coordinates (rgeos::gIntersection (li, lj))
                d <- sqrt ((xy [1] - obji [[j]] [,1]) ^ 2 + 
                           (xy [2] - obji [[j]] [,2]) ^ 2)
                di <- which.min (d)
                n <- nrow (obji [[j]])
                rnames <- rownames (obji [[j]])
                # TODO: di ==1 could be 1,xy,2:n
                if (di == 1)
                    indx <- list (NULL, 1:n)
                else if (di == n)
                    indx <- list (1:n, NULL)
                else if (d [di - 1] < d [di + 1])
                    indx <- list (1:(di-1), di:n)
                else
                    indx <- list (1:di, (di+1):n)
                objs [[i]] [[j]] <- rbind (obji [[j]] [indx [[1]], ], xy,
                                           obji [[j]] [indx [[2]], ])
                rownames (objs [[i]] [[j]]) <- c (rname [indx [[1]]],
                                                  maxvert,
                                                  rnames [indx [[2]]])

                # Then add same vertex into the other element, which requires
                # first making an index into the list of lists that is objs
                n <- which (intersections == 0)
                lens <- cumsum (sapply (test, length))
                ni <- max (which (lens < n)) + 1
                nj <- n - lens [ni - 1]
                # Then ni needs to point into the full objs instead of test
                ni <- seq (objs) [!seq (objs) %in% i] [ni]
                temp <- objs [[ni]] [[nj]] 
                # Then insert xy into temp
                d <- sqrt ((xy [1] - temp [,1]) ^ 2 + (xy [2] - temp [,2]) ^ 2)
                di <- which.min (d)
                n <- nrow (temp)
                rnames <- rownames (temp)

                if (d [di-1] < d [di+1])
                {
                    temp <- rbind (temp [1:(di-1),], xy, temp [di:n,])
                    rownames (temp) <- c (rnames [1:(di-1)], maxvert,
                                                rnames [di:n])
                } else
                {
                    temp <- rbind (temp [1:di,], xy, temp [(di+1):n,])
                    rownames (temp) <- c (rnames [1:di], maxvert,
                                                rnames [(di+1):n])
                }
                objs [[ni]] [[nj]] <- temp
            } # end if any (intersections) == 0
        } # end for j over obj [[i]]
    } # end for i over all objs


    # ***** (4) Fill a connectivity matrix between all highways and extract the
    # *****     *longest* cycle connecting them all

    conmat <- array (FALSE, dim=rep (length (waynames), 2))
    for (i in seq (objs))
    {
        test <- do.call (rbind, objs [[i]])
        ref <- objs
        ref [[i]] <- NULL
        indx <- (1:length (objs)) [!(1:length (objs)) %in% i]
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
    if (sum (rowSums (conmat)) < 3)
        stop ("Only ", length (which (rowSums (conmat) > 0)), " / ", 
              nrow (conmat), " streets actually connect; no cycle is possible")
    # Extract longest cycle:
    cycles <- ggm::fundCycles (conmat)
    if (is.null (cycles))
        stop ("Streets do not form a cycle")
    ci <- which.max (sapply (cycles, nrow))
    cyc <- cycles [[ci]]


    # ***** (5) Calculate shortest paths along each part of the cycle
    cyc.len <- nrow (cyc)
    cyc <- rbind (cyc, cyc [1,])
    paths <- list ()
    for (i in seq (cyc.len))
    {
        w0 <- cyc [i,2] # the current way
        wf <- cyc [i,1] # the "from" way
        wt <- cyc [i+1,2] # the "to" way

        # Store objs [[w0]] as an igraph
        from <- unlist (lapply (objs [[w0]], function (x) 
                                           rownames (x)[1:(nrow(x)-1)]))
        to <- unlist (lapply (objs [[w0]], function (x) 
                                           rownames (x)[2:nrow(x)]))
        g <- igraph::graph_from_edgelist (cbind (from, to), directed=FALSE)

        # Then find nodes in w0 which join to wf and wt
        street <- do.call (rbind, objs [[w0]])
        ends <- c (objs [[wf]], objs [[wt]])
        ends <- do.call (rbind, ends)
        n <- which (rowSums (array (street %in% ends, dim=dim (street))) == 2)
        # n is index into street of lat-lon pairs duplicated in ends
        xy <- street [n,] # coordinates of nn
        n <- rownames (street) [n] # names of nn within the igraph
        stopifnot (length (n) > 1)

        # If there are more than 2 nodes present in the ends, then extract the
        # *longest* of these shortest paths:
        if (length (n) > 2)
        {
            # Then reduce to the *longest* of the shortest paths
            maxlen <- 0
            ij <- NULL
            nc <- combn (n, 2)
            for (j in seq (n))
            {
                sp <- suppressWarnings (igraph::shortest_paths 
                                         (g, nc [1,j], nc [2,j])$vpath [[1]])
                if (length (sp) > maxlen)
                {
                    maxlen <- length (sp)
                    ij <- combn (1:3, 2) [,j]
                }
            }
            n <- n [ij]
        }
        # Then length (n) == 2, and so the shortest path can be extracted:
        sp <- suppressWarnings (igraph::shortest_paths (g, n [1], n [2])$vpath [[1]])

        # Then get xy of sp from "street", which is the flat version of objs [[w0]].
        # Note that junctions will have duplicates of same node, but match only
        # returns the first match, so that's okay.
        indx <- match (names (sp), rownames (street))
        stopifnot (all (!is.na (indx)))
        path <- street [indx,]

        paths [[i]] <- path
    }

    # (6) Connect paths together to form desired cyclic boundary
    #
    # This involves first checking whether any of the paths need to be flipped 
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
    {
        xlims <- range (lapply (waynames, function (i)
                                do.call (get.xylims, list (get (i)))$xrange))
        ylims <- range (lapply (waynames, function (i)
                                do.call (get.xylims, list (get (i)))$yrange))

        plot.new ()
        par (mar=c(0,0,0,0))
        plot (NULL, NULL, xlim=xlims, ylim=ylims, xaxt="n", yaxt="n",
              xlab="", ylab="", frame=FALSE)
        for (i in waynames)
        {
            ni <- paste (i, "o", sep="")
            for (j in get (ni))
                lines (j [,1], j [,2], lwd=lwds [1], col=cols [1])
        }
        lines (path [,1], path [,2], lwd=lwds [2], col=cols [2])
    }

    return (unique (path))
}
