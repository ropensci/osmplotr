#' get_highway_cycle
#'
#' Takes a list of OpenStreetMap highways returned by
#' \code{\link{extract_highways}} and sequentially connects closest nodes of
#' adjacent highways until the set of highways connects to form a cycle.
#'
#' Proceeds through the following 3 steps:
#' (1) Add intersection nodes to junctions of ways where these don't already
#'      exist
#' (2) Fill a connectivity matrix between all highways and extract the *longest*
#'      cycle connecting them all
#' (3) Insert extra connections between highways until the longest cycle ==
#'      length (highways).
#'
#' @param ways A list of highways as returned by
#' \code{\link{extract_highways}}, each element of which is a list of distinct
#' segments for a particular OSM highway.
#'
#' @return A modified version of ways, extended by the addition of
#' connecting nodes.
#'
#' @noRd
get_highway_cycle <- function (ways)
{
    conmat <- get_conmat (ways)
    cycles <- try (ggm::fundCycles (conmat), TRUE)
    if (is (attr (cycles, "condition"), "simpleError"))
        cycles <- NULL

    cyc_len <- 0
    if (!is.null (cycles))
        cyc_len <- max (sapply (cycles, nrow))
    n <- length (ways)
    # i1_ref and i2_ref are index matricex of [from, to]
    i1_ref <- array (1:n, dim = c(n, n))
    i2_ref <- t (i1_ref)
    i1_ref <- i1_ref [upper.tri (i1_ref)]
    i2_ref <- i2_ref [upper.tri (i2_ref)]
    while (cyc_len < length (ways))
    {
        es <- extend_cycles (conmat, i1_ref, i2_ref)

        if (all (is.na (es$cyc_len)))
        {
            warning ('No cycles can be found or made')
            break
        } else if (max (es$cyc_len, na.rm = TRUE) <= cyc_len)
        {
            warning ('Cycle unable to be extended through all ways',
                     call. = FALSE)
            break
        } else
        {
            if (length (es$i1) > 1)
                es <- closest_cycle_connection (ways, es)
            cyc_len <- es$cyc_len
        }

        ways <- connect_two_ways (ways, es)
        conmat <- get_conmat (ways)
        cycles <- try (ggm::fundCycles (conmat), TRUE)
        if (!is.null (cycles))
            cyc_len <- max (sapply (cycles, nrow))
    } # end while cyc_len < length (ways)

    return (ways)
}


#' extend_cycles
#'
#' Find lengths of cycles formed by adding all possible single individual links
#' in a conmat and return the connection that gives the longest conmat.
#'
#' @note There is no check for the proximity of potentially connected lines
#' here, so it may theoretically occur that this routine will suggest shortcuts
#' across large distances.
#'
#' @noRd
extend_cycles <- function (conmat, i1, i2)
{
    conmat_tri <- conmat [upper.tri (conmat)]
    indx <- which (!conmat_tri)
    conmat_tri <- conmat_tri [indx]
    i1 <- i1 [indx]
    i2 <- i2 [indx]
    # Then connect each in turn and get length of cycle if formed
    cyc_len <- rep (NA, length (i1))
    conmat_ref <- conmat
    for (i in seq (i1))
    {
        conmat <- conmat_ref
        conmat [i1 [i], i2 [i]] <- TRUE
        conmat [i2 [i], i1 [i]] <- TRUE
        cycles <- try (ggm::fundCycles (conmat), TRUE)
        if (is (attr (cycles, "condition"), "simpleError"))
            cycles <- NULL
        if (!is.null (cycles))
            cyc_len [i] <- max (sapply (cycles, nrow))
    }

    indx <- which (cyc_len == max (cyc_len))

    return (list ('cyc_len' = max (cyc_len), 'i1' = i1 [indx],
                  'i2' = i2 [indx]))
}

# when extend_cycle returns multiple options, this returns the single option
# as the two ways that are the closest together.
closest_cycle_connection <- function (ways, es)
{
    d <- rep (NA, length (es$i1))
    for (i in seq (es$i1))
    {
        way1 <- do.call (rbind, ways [[es$i1 [i] ]])
        way2 <- do.call (rbind, ways [[es$i2 [i] ]])
        d [i] <- haversine (way1, way2) [3]
    }
    es$i1 <- es$i1 [which.min (d)]
    es$i2 <- es$i2 [which.min (d)]

    return (es)
}

#' connect_two_ways
#'
#' @param ways A list of highways as returned by \code{\link{extract_highways}},
#' each element of which is a list of distinct segments for a particular OSM
#' highway.
#' @param es Result of call to \code{extend_cycles}
#' @return Modified version of ways with a new connection that maximises the
#' length of the cycle given in \code{es}
#'
#' @noRd
connect_two_ways <- function (ways, es)
{
    wi1 <- ways [[es$i1]]
    wi2 <- ways [[es$i2]]
    dmat <- array (NA, dim = c (length (wi1), length (wi2)))
    for (i in seq (length (wi1)))
        for (j in seq (length (wi2)))
            dmat [i, j] <- haversine (wi1 [[i]], wi2 [[j]]) [3]
    indx <- which (dmat == min (dmat), arr.ind = TRUE) [1, ] # there may be > 1

    hs <- haversine (wi1 [[indx [1] ]], wi2 [[indx [2] ]])

    # Then insert one node in ways [[i1]]
    new_node <- wi2 [[indx [2] ]] [hs [2], , drop = FALSE] #nolint
    wi11 <- wi1 [[indx [1] ]]
    wi11 <- rbind (wi11 [1:(hs [1] - 1), , drop = FALSE], #nolint
                   new_node,
                   wi11 [hs [1]:nrow (wi11), , drop = FALSE]) #nolint

    ways [[es$i1]] [[indx [1] ]] <- wi11

    return (ways)
}
