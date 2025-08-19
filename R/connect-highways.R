#' connect_highways
#'
#' Takes a list of highways names which must enclose an internal area, and
#' returns a \code{SpatialLines} object containing a sequence of OSM nodes which
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
#' highways <- c (
#'     "Monmouth.St", "Short.?s.Gardens", "Endell.St", "Long.Acre",
#'     "Upper.Saint.Martin"
#' )
#' # Note that dots signify "anything", including whitespace and apostrophes,
#' # and that '?' denotes optional previous character and so here matches
#' # both "Shorts Gardens" and "Short's Gardens"
#' \dontrun{
#' highways1 <- connect_highways (highways = highways, bbox = bbox, plot = TRUE)
#' highways <- c ("Endell.St", "High.Holborn", "Drury.Lane", "Long.Acre")
#' highways2 <- connect_highways (highways = highways, bbox = bbox, plot = TRUE)
#' }
#'
#' # Use of 'connect_highways' to highlight a region on a map
#' map <- osm_basemap (bbox = bbox, bg = "gray20")
#' # dat_B <- extract_osm_data (key = "building",
#' #                            value = "!residential",
#' #                            bbox = bbox)
#' # Those data are part of 'osmplotr':
#' dat_BNR <- london$dat_BNR # Non-residential buildings
#' \dontrun{
#' groups <- list (highways1, highways2)
#' map <- add_osm_groups (map,
#'     obj = dat_BNR, groups = groups,
#'     cols = c ("red", "blue"), bg = "gray40"
#' )
#' print_osm_map (map)
#' }
#' @family data-extraction
#' @export
connect_highways <- function (highways, bbox, plot = FALSE) {

    if (missing (highways)) {
        stop ("A vector of highway names must be given")
    }
    if (missing (bbox)) {
        stop ("A bounding box must be given")
    }

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
    if (any (i0)) {
        for (i in i0) {
            warning (highways [i], " contains no data")
        }
    }
    while (any (sapply (ways, length) == 0)) {

        i0 <- which (sapply (ways, length) == 0)
        ways [[i0 [1]]] <- NULL # nolint
    }

    if (plot) {
        plot_highways (ways)
    }

    # connect individual componenets of each way:
    ways <- connect_single_ways (ways)
    # insert any intersection nodes where necessary
    ways <- insert_intersections (ways)
    # connect any unconnected ways to form longest cycle through them:
    ways <- get_highway_cycle (ways)

    conmat <- get_conmat (ways)
    cycles <- try (ggm::fundCycles (conmat), TRUE)

    path <- NULL
    if (is.null (cycles) || is (attr (cycles, "condition"), "simpleError")) {
        warning ("There are no cycles in the listed highways")
    } else {

        cyc <- cycles [[which.max (sapply (cycles, nrow))]]
        if (nrow (cyc) < length (ways)) {
            warning ("Cycle unable to be extended through all ways",
                call. = FALSE
            )
        }

        # shortest path through the entire cycle:
        path <- sps_through_cycle (ways, cyc)
        if (plot) {
            lines (path [, 1], path [, 2], lwd = 2, lty = 2)
        }
    }

    return (path)
}

#' insert_intersections
#'
#' When one way crosses another over a bridge or overpass, they will not
#' actually share a node and so intersection nodes must be inserted.
#'
#' @noRd
insert_intersections <- function (ways) {

    # first find which ways don't have common nodes and seem to cross
    dmin <- 0.05 # minimal distance to consider possible intersection
    nw <- length (ways)
    ilist <- jlist <- NULL
    rownum <- 0
    for (i in seq (nw - 1)) {
        for (j in (i + 1):nw) {

            wi <- do.call (rbind, ways [[i]])
            wj <- do.call (rbind, ways [[j]])
            # only include ways if there's no actual intersection and if lines
            # are close enough (< 50m)
            d <- haversine (wi, wj) [3] # in km
            common_node <- any (rownames (wi) %in% rownames (wj))
            if (!common_node && d < dmin) {

                ilist <- c (ilist, i)
                jlist <- c (jlist, j)
            }
        }
    }

    # then insert any nodes where needed
    for (i in seq (ilist)) {

        # first find which components might cross
        plist <- qlist <- NULL
        for (p in seq (ways [[ilist [i]]])) { # nolint
            for (q in seq (ways [[jlist [i]]])) { # nolint

                d <- haversine (
                    ways [[ilist [i]]] [[p]], # nolint
                    ways [[jlist [i]]] [[q]]
                ) [3] # nolint
                if (d < dmin) {

                    plist <- c (plist, p)
                    qlist <- c (qlist, q)
                }
            }
        }

        # then insert intersections
        for (j in seq (plist)) {

            wp <- ways [[ilist [i]]] [[plist [j]]] # nolint
            wq <- ways [[jlist [i]]] [[qlist [j]]] # nolint
            newint <- insert_one_intersection (wp, wq,
                prefix = "a",
                num = rownum
            )
            ways [[ilist [i]]] [[plist [j]]] <- newint$way1 # nolint
            ways [[jlist [i]]] [[qlist [j]]] <- newint$way2 # nolint
            rownum <- rownum + 1
        } # end for j
    } # end for i

    return (ways)
}

# insert a common intersection into both way1 and way2
insert_one_intersection <- function (way1, way2, prefix = "a", num = 0) {

    class (way1) <- "matrix"
    class (way2) <- "matrix"
    lp <- sf::st_sfc (sf::st_linestring (way1), crs = 4326)
    lq <- sf::st_sfc (sf::st_linestring (way2), crs = 4326)
    int <- sf::st_coordinates (sf::st_intersection (lp, lq))
    if (length (int) > 0) {

        i <- which.min ((int [1, 1] - way1 [, 1])^2 +
            (int [1, 2] - way1 [, 2])^2)
        rnames <- rownames (way1)
        rnames <- c (
            rnames [1:(i - 1)],
            paste0 (prefix, num),
            rnames [i:length (rnames)]
        )
        way1 <- rbind (
            way1 [1:(i - 1), ], int [1, ],
            way1 [i:nrow (way1), ]
        )
        rownames (way1) <- rnames

        j <- which.min ((int [1, 1] - way2 [, 1])^2 +
            (int [1, 2] - way2 [, 2])^2)
        rnames <- rownames (way2)
        rnames <- c (
            rnames [1:(j - 1)],
            paste0 (prefix, num),
            rnames [j:length (rnames)]
        )
        way2 <- rbind (
            way2 [1:(j - 1), ], int [1, ],
            way2 [j:nrow (way2), ]
        )
        rownames (way2) <- rnames
    }

    list (way1 = way1, way2 = way2)
}


#' connect_single_ways
#'
#' inserts connection nodes into each indvidual way so that all components
#' actually connect
#'
#' @noRd
connect_single_ways <- function (ways) {

    for (i in seq (ways)) {

        wi <- ways [[i]]
        if (length (wi) > 1) {

            conmat <- get_conmat (wi)
            indx <- which (!apply (conmat, 1, any))
            for (j in indx) {

                wij <- wi [[j]]
                wj <- wi
                wj [[j]] <- NULL
                con <- which.min (unlist (lapply (wj, function (k) {
                    haversine (wij, k) [3]
                })))
                ways [[i]] [[j]] <- connect_at_closest (wij, wj [[con]])
            }
        }
    }

    return (ways)
}

# connect two components of ways by inserting closest element of way2 into way1
connect_at_closest <- function (way1, way2) {

    if (!any (rownames (way1) %in% rownames (way2))) {

        h <- haversine (way1, way2)

        way1 <- rbind (
            way1 [1:(h [1] - 1), , drop = FALSE], # nolint
            way2 [h [2], , drop = FALSE], # nolint
            way1 [h [1]:nrow (way1), , drop = FALSE]
        ) # nolint
    }
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
haversine <- function (way1, way2) {

    x1 <- array (way1 [, 1], dim = c (nrow (way1), nrow (way2)))
    y1 <- array (way1 [, 2], dim = c (nrow (way1), nrow (way2)))
    x2 <- t (array (way2 [, 1], dim = c (nrow (way2), nrow (way1))))
    y2 <- t (array (way2 [, 2], dim = c (nrow (way2), nrow (way1))))
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




plot_highways <- function (ways) {

    xy <- do.call (rbind, do.call (c, ways))

    par (mar = rep (0, 4))
    plot (NULL, NULL,
        xlim = range (xy [, 1]), ylim = range (xy [, 2]),
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame = FALSE
    )

    cols <- rainbow (length (ways))

    for (i in seq (ways)) {
        for (j in seq (ways [[i]])) {

            x <- ways [[i]] [[j]] [, 1]
            y <- ways [[i]] [[j]] [, 2]
            lines (x, y, col = cols [i])
            if (length (ways [[i]]) == 1) {
                lab <- paste0 (i)
            } else {
                lab <- paste0 (i, ".", j)
            }
            text (mean (x), mean (y), col = cols [i], labels = lab)
        }
    }
}

#' get_conmat
#'
#' Get connection matrix between a list of ways
#'
#' @param ways Either a full list of list of ways to be connected, or a single
#' list of ways.
#'
#' @return Binary connectivity matrix between all ways
#'
#' @noRd
get_conmat <- function (ways) {

    conmat <- array (FALSE, dim = rep (length (ways), 2))

    for (i in seq (ways)) {

        if (is.list (ways [[i]])) {

            wi <- do.call (rbind, ways [[i]])
            ref <- ways
        } else {

            wi <- ways [[i]]
            ref <- ways
        }
        ref [[i]] <- NULL
        if (is.list (ref [[1]])) {
            ref <- lapply (ref, function (i) do.call (rbind, i))
        }

        convec <- vapply (
            ref, function (i) {
                any (rownames (i) %in% rownames (wi))
            },
            logical (1)
        )

        indx <- seq (ways) [!(seq (ways)) %in% i]
        conmat [i, indx] <- conmat [indx, i] <- convec
    }

    return (conmat)
}


#' shortest path through entire cycle of ways
#'
#' @param ways List of ways to be connected
#' @param cyc Cycle extracted from \code{extract_cycle}
#'
#' @return Matrix of cyclically connected coordinates encircling all ways
#'
#' @noRd
sps_through_cycle <- function (ways, cyc) {

    cyc <- rbind (cyc, cyc [1, ])
    xy0 <- apply (do.call (rbind, do.call (c, ways)), 2, mean)

    # First reduce lists of ways doen to only those which connect with ways in
    # other groups:
    for (i in seq_len (nrow (cyc)) [-1]) {

        c0 <- cyc [i - 1, 2] # the current way
        cf <- cyc [i - 1, 1] # the 'from' way
        ct <- cyc [i, 2] # the 'to' way

        nf <- rownames (do.call (rbind, ways [[cf]]))
        nt <- rownames (do.call (rbind, ways [[ct]]))
        w0 <- ways [[c0]]
        index_f <- which (vapply (
            w0,
            function (i) any (rownames (i) %in% nf),
            logical (1L)
        ))
        index_t <- which (vapply (
            w0,
            function (i) any (rownames (i) %in% nt),
            logical (1L)
        ))
        combs <- expand.grid (index_f, index_t)
        cm <- osmplotr:::get_conmat (ways [[c0]])
        asp <- e1071::allShortestPaths (cm)
        paths <- apply (combs, 1, function (i) {
            unique (e1071::extractPath (asp, i [1], i [2]))
        }, simplify = FALSE)
        path_lens <- vapply (paths, length, integer (1L))
        paths <- paths [which (path_lens == min (path_lens))]
        if (length (paths) > 1L) {
            # Choose paths with outermost elements:
            dmax <- vapply (paths, function (p) {
                these_ways <- do.call (rbind, w0 [p])
                max (geodist::geodist (xy0, these_ways))
            }, numeric (1L))
            paths <- paths [[which.max (dmax)]]
        }
        ways [[c0]] <- ways [[c0]] [unlist (paths)]
    }

    thepath <- NULL
    for (i in seq_len (nrow (cyc)) [-1]) {

        c0 <- cyc [i - 1, 2] # the current way
        cf <- cyc [i - 1, 1] # the 'from' way
        ct <- cyc [i, 2] # the 'to' way

        w0f <- do.call (rbind, ways [[c0]])
        if (is.null (thepath)) {

            wff <- do.call (rbind, ways [[cf]])
        } else {

            wff <- thepath
        }
        wtf <- do.call (rbind, ways [[ct]])
        # start and end nodes that join to wf and wt:
        nst <- rownames (wff) [which (rownames (wff) %in% rownames (w0f))]
        nend <- rownames (wtf) [which (rownames (wtf) %in% rownames (w0f))]

        w0f <- w0f [!duplicated (w0f), ]
        adjmat <- array (NA, dim = rep (nrow (w0f), 2))
        nms <- rownames (w0f)
        for (j in seq_along (ways [[c0]])) {

            wj <- ways [[c0]] [[j]]
            indx <- match (rownames (wj), nms)
            ifr <- indx [1:(length (indx) - 1)]
            ito <- indx [-1]
            indx <- (ito - 1) * nrow (adjmat) + ifr
            adjmat [indx] <- 1
            indx <- (ifr - 1) * nrow (adjmat) + ito
            adjmat [indx] <- 1
        }
        asp <- e1071::allShortestPaths (adjmat)
        ifr <- which (nms %in% nst)
        ito <- which (nms %in% nend)

        combs <- expand.grid (ifr, ito)
        paths <- apply (combs, 1, function (i) {
            unique (e1071::extractPath (asp, i [1], i [2]))
        }, simplify = FALSE)
        path_lens <- vapply (paths, length, integer (1L))
        this_path <- paths [[which.min (path_lens)]]
        this_way <- w0f [this_path, ]

        ways [[c0]] <- list (this_way)
        thepath <- rbind (thepath, this_way)
    }

    return (thepath)
}
