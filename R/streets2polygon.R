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
#' @param force_join forces streets to form a cycle by sucessively joining
#' closest segments of adjacent streets
#' @param plot if TRUE, then all OSM data for each highway is plotted (with
#' lwds[1], cols[1]), with the final cycle overlaid (with lwds[2], cols[2]).
#' @param lwds = line widths for (all highways, final cycle)
#' @param cols = line colours for (all highways, final cycle)
#' @return A single data.frame containing the lat-lon coordinates of the cyclic
#' line connecting all given streets.

streets2polygon <- function (highways=NULL, bbox=NULL, force_join=TRUE,
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

    # **** (1) Get sequentially ordered list of highways
    ways <- extract_highways (highway_names=highways, bbox=bbox)


    # ***** (2) Fill a connectivity matrix between all highways and extract the
    # *****     *longest* cycle connecting them all

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
    # The following condition will be fulfilled when <= 2 street connect
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
