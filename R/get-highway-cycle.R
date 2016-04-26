#' get_highway_cycle
#'
#' Takes a list of OpenStreetMap highways returned by extract_highways()
#' and sequentially connects closest nodes of adjacent highways until the set of
#' highways connects to form a cycle.
#'
#' @param highways A list of highways as returned by extract_highways(), each
#' element of which is a list of distinct segments for a particular OSM highway.
#' @return A modified version of highways, extended by the addition of
#' connecting nodes.

get_highway_cycle <- function (highways)
{
    # Proceeds through the following 3 steps:
    # ***** (1) Add intersection nodes to junctions of ways where these don't
    # *****     already exist
    # ***** (2) Fill a connectivity matrix between all highways and extract the
    # *****     *longest* cycle connecting them all
    # ***** (3) Insert extra connections between highways until the longest
    # *****     cycle == length (highways). 
    if (missing (highways))
        stop ('highways must be given')
    if (class (highways) != 'list')
        stop ('highways must be a list')
    # TODO: Make class so that can be properly checked

    # ***** (1) Add intersection nodes to junctions of ways where these don't
    # *****     already exist
    for (i in seq (highways))
    {
        obji <- highways [[i]]
        test <- highways
        test [[i]] <- NULL
        test_flat <- do.call (c, test)
        # Check whether any of obji cross any of test_flat *and* don't already
        # exist as vertices
        for (j in seq (obji))
        {
            li <- sp::Line (obji [[j]])
            li <- sp::SpatialLines (list (Lines (list (li), ID='a'))) 
            # The following function returns default of -1 for no geometric
            # intersection; 0 where intersections exists but area *NOT* vertices
            # of li, and 2 where intersections are vertices of li.
            intersections <- sapply (test_flat, function (x) {
                        lj <- sp::Line (x)
                        lj <- sp::SpatialLines (list (Lines (list (lj), ID='a'))) 
                        int <- rgeos::gIntersection (li, lj)
                        if (!is.null (int))
                            sum (sp::coordinates (int) %in% x)
                        else
                            -1
                        })
            if (any (intersections == 0))
                for (k in which (intersections == 0))
                {
                    # Then they have to be added to highways [[i]] [[j]]. 
                    x <- test_flat [k] [[1]]
                    lj <- sp::Line (x)
                    lj <- sp::SpatialLines (list (Lines (list (lj), ID='a'))) 
                    xy <- sp::coordinates (rgeos::gIntersection (li, lj))
                    d <- sqrt ((xy [1] - obji [[j]] [,1]) ^ 2 + 
                               (xy [2] - obji [[j]] [,2]) ^ 2)
                    di <- which.min (d)
                    n <- nrow (obji [[j]])
                    rnames <- rownames (obji [[j]])
                    # xy can be closest to d1, but still either
                    # A. -------d1---xy--------------d2, or
                    # B. xy-----d1-------------------d2
                    # A. implies that |xy,d2|<|d1,d2|, and B vice-versa
                    if (di == 1)
                    {
                        d12 <- sqrt (diff (obji [[j]] [1:2,1]) ^ 2 +
                                     diff (obji [[j]] [1:2,2]) ^ 2)
                        if (d12 < d [2])
                            indx <- list (NULL, 1:n)
                        else
                            indx <- list (1, 2:n)
                    } else if (di == n)
                    {
                        d12 <- sqrt (diff (obji [[j]] [(n-1:n),1]) ^ 2 +
                                     diff (obji [[j]] [(n-1:n),2]) ^ 2)
                        if (d12 < d [n-1])
                            indx <- list (1:n, NULL)
                        else
                            indx <- list (1:(n-1), n)
                    } else if (d [di - 1] < d [di + 1])
                        indx <- list (1:(di-1), di:n)
                    else
                        indx <- list (1:di, (di+1):n)
                    highways [[i]] [[j]] <- rbind (obji [[j]] [indx [[1]], ], xy,
                                               obji [[j]] [indx [[2]], ])
                    rownames (highways [[i]] [[j]]) <- c (rnames [indx [[1]]],
                                                      maxvert,
                                                      rnames [indx [[2]]])

                    # Then add same vertex into the other elements, which
                    # requires first making an index into the list of lists that
                    # is highways
                    lens <- cumsum (sapply (test, length))
                    if (k < lens [1])
                    {
                        ni <- 1
                        nj <- k
                    } else
                    {
                        ni <- max (which (lens < k)) + 1
                        nj <- k - lens [ni - 1]
                    }
                    # Then ni needs to point into the full highways instead of
                    # test
                    ni <- seq (highways) [!seq (highways) %in% i] [ni]
                    temp <- highways [[ni]] [[nj]] 
                    # Then insert xy into temp
                    d <- sqrt ((xy [1] - temp [,1]) ^ 2 + (xy [2] - temp [,2]) ^ 2)
                    di <- which.min (d)
                    n <- nrow (temp)
                    rnames <- rownames (temp)

                    if (di == 1)
                    {
                        d12 <- sqrt (diff (obji [[j]] [1:2,1]) ^ 2 +
                                     diff (obji [[j]] [1:2,2]) ^ 2)
                        if (d12 < d [2])
                            indx <- list (NULL, 1:n)
                        else
                            indx <- list (1, 2:n)
                    } else if (di == n)
                    {
                        d12 <- sqrt (diff (obji [[j]] [(n-1:n),1]) ^ 2 +
                                     diff (obji [[j]] [(n-1:n),2]) ^ 2)
                        if (d12 < d [n-1])
                            indx <- list (1:n, NULL)
                        else
                            indx <- list (1:(n-1), n)
                    } else if (d [di - 1] < d [di + 1])
                        indx <- list (1:(di-1), di:n)
                    else
                        indx <- list (1:di, (di+1):n)
                    temp <- rbind (temp [indx [[1]],], xy, temp [indx [[2]],])
                    rownames (temp) <- c (rnames [indx [[1]]], maxvert,
                                                rnames [indx [[2]]])
                    highways [[ni]] [[nj]] <- unique (temp)
                } # end for k over which (intersections == 0)
        } # end for j over obj [[i]]
    } # end for i over all highways

    # Get maximum vertext number of highways
    maxvert <- 0
    for (i in seq (highways))
        maxvert <- max (maxvert, unlist (lapply (highways [[i]], function (x)
                                    max (as.numeric (rownames (x))))))
    maxvert <- maxvert + 1

    # ***** (2) Fill a connectivity matrix between all highways and extract the
    # *****     *longest* cycle connecting them all
    conmat <- array (FALSE, dim=rep (length (highways), 2))
    for (i in seq (highways))
    {
        test <- do.call (rbind, highways [[i]])
        ref <- highways
        ref [[i]] <- NULL
        indx <- (1:length (highways)) [!(1:length (highways)) %in% i]
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
        cycles <- NULL

    # ***** (3) Insert extra connections between highways until the longest
    # *****     cycle == length (highways). 
    conmat_adj <- conmat
    cyc_len <- 0
    if (!is.null (cycles))
        cyc_len <- max (sapply (cycles, nrow))
    n <- length (highways)
    i1_ref <- array (1:n, dim=c(n, n))
    i2_ref <- t (i1_ref)
    i1_ref <- i1_ref [upper.tri (i1_ref)]
    i2_ref <- i2_ref [upper.tri (i2_ref)]
    while (cyc_len < length (highways))
    {
        # At each step, find which connection results in the *largest* cycle.
        # There is a break-clause below to ensure this while loop will always
        # terminate.  Start by getting indices into pairs of streets that are
        # not connected
        conmat_tri <- conmat_adj [upper.tri (conmat_adj)]
        indx <- which (!conmat_tri)
        conmat_tri <- conmat_tri [indx]
        i1 <- i1_ref [indx]
        i2 <- i2_ref [indx]
        # Then connect each in turn and get length of cycle if formed
        cyc_len_i <- rep (NA, length (i1))
        for (i in seq (i1))
        {
            conmat_adj [i1 [i], i2 [i]] <- TRUE
            conmat_adj [i2 [i], i1 [i]] <- TRUE
            cycles <- try (ggm::fundCycles (conmat_adj), TRUE)
            if (is (attr (cycles, "condition"), "simpleError"))
                cycles <- NULL
            if (!is.null (cycles))
                cyc_len_i [i] <- max (sapply (cycles, nrow))
        }
        if (all (is.na (cyc_len_i)))
        {
            warning ('No cycles can be found or made')
            break
        } else if (max (cyc_len_i, na.rm=TRUE) <= cyc_len)
        {
            warning ('Cycle unable to be extended through all ways')
            break
        } else
            cyc_len <- max (cyc_len_i, na.rm=TRUE)

        # Then connect the actual highways corresponding to the longest cycle
        i1 <- i1 [which.max (cyc_len_i)]
        i2 <- i2 [which.max (cyc_len_i)]
        h1 <- do.call (rbind, highways [[i1]])
        h2 <- do.call (rbind, highways [[i2]])
        h1x <- array (h1 [,1], dim=c(nrow (h1), nrow (h2)))
        h1y <- array (h1 [,2], dim=c(nrow (h1), nrow (h2)))
        h2x <- t (array (h2 [,1], dim=c(nrow (h2), nrow (h1))))
        h2y <- t (array (h2 [,2], dim=c(nrow (h2), nrow (h1))))
        d <- sqrt ((h1x - h2x) ^ 2 + (h1y - h2y) ^ 2)
        di1 <- which.min (apply (d, 1, min))
        di2 <- which.min (apply (d, 2, min))
        node1 <- rownames (h1) [di1]
        xy1 <- h1 [di1,]
        node2 <- rownames (h2) [di2]
        xy2 <- h2 [di2,]
        # Then insert these nodes in the respective highways, but only in cases
        # where the joining node is terminal.  Joining nodes can't be
        # non-terminal in both ways, because then they would cross and already
        # have had a crossing-node inserted above. This insertion thus add to
        # the terminal node of one way an additional node that will generally
        # lie in the middle of some other way, thereby connecting the two. 
        ni1 <- sapply (highways [[i1]], function (x)
                       max (c (-1, which (rownames (x) == node1))))
        # ni1 = -1 where node1 not found, otherwise it's the position of
        # node1 in highways [[i1]] 
        ni2 <- which.max (ni1)
        if (max (ni1) == 1)
        {
            hnames <- c (node2, rownames (highways [[i1]] [[ni2]]))
            highways [[i1]] [[ni2]] <- rbind (xy2, highways [[i1]] [[ni2]])
            rownames (highways [[i1]] [[ni2]]) <- hnames
        } else if (max (ni1) == nrow (highways [[i1]] [[ni2]]))
        {
            hnames <- c (rownames (highways [[i1]] [[ni2]]), node2)
            highways [[i1]] [[ni2]] <- rbind (highways [[i1]] [[ni2]], xy2)
            rownames (highways [[i1]] [[ni2]]) <- hnames
        }

        # Then join node1 onto highways [[i2]]:
        ni1 <- sapply (highways [[i2]], function (x)
                       max (c (-1, which (rownames (x) == node2))))
        ni2 <- which.max (ni1)
        if (max (ni1) == 1)
        {
            hnames <- rbind (node1, names (highways [[i2]] [[ni2]]))
            highways [[i2]] [[ni2]] <- rbind (xy1, highways [[i2]] [[ni2]])
            names (highways [[i2]] [[ni2]]) <- hnames
        } else if (max (ni1) == nrow (highways [[i2]] [[ni2]]))
        {
            hnames <- rbind (names (highways [[i2]] [[ni2]]), node1)
            highways [[i2]] [[ni2]] <- rbind (highways [[i2]] [[ni2]], xy1)
            names (highways [[i2]] [[ni2]]) <- hnames
        }
    } # end while cyc_len < length (highways)

    return (highways)
}
