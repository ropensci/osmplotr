#' order_lines
#'
#' Accepts a \code{SpatialLinesDataFrame} representing an OpenStreetMap line
#' object such as a highway. The list items of these objects are arbitrarily
#' organised within OpenStreetMap. This function orders the components,
#' returning a list of components each of which is ordered sequentially along
#' the line.  Points of intersection between components are also inserted where
#' these are not explicitly present in OpenStreetMap. Nodes are also
#' sequentially renumbered, starting at \code{(i0+1)}.
#'
#' @param sp_lines A \code{SpatialLinesDataFrame} returned from
#' \code{extract_osm_objects}.
#' @param i0 The first node is numbered \code{(i0+1)}, with other nodes (except
#' junction nodes) numbered sequentially .
#' @return A list of ordered line segments.
#'
#' @section Note:
#' This function is primarily used in \code{extract_highways}.
#'
#' @noRd
order_lines <- function (sp_lines, i0 = 0)
{
    stopifnot (class (sp_lines) == 'SpatialLinesDataFrame')
    # Extract all coords from the SLDF and store as simple list:
    xy <- sp::coordinates (sp_lines)
    xy <- lapply (xy, function (x) array (unlist (x), dim = dim(x[[1]]) ))
    # Start the ordered data.frame with the first item of xy
    xy_ord <- xy [[1]]
    # If a way contains discrete segments, these are stored in xy_ord_list, and
    # xy_ord itself is reset
    xy_ord_list <- NULL
    xy [[1]] <- NULL
    while (length (xy) > 0)
    {
        ex <- extend_ord_list (xy, xy_ord, xy_ord_list)
        xy <- ex$xy
        xy_ord <- ex$xy_ord
        xy_ord_list <- ex$xy_ord_list
    }

    if (!is.null (xy_ord_list))
    {
        xy_ord_list [[length (xy_ord_list) + 1]] <- unique (xy_ord)
        xy_ord <- xy_ord_list
    } else
        xy_ord <- list (unique (xy_ord))

    # Then insert intersections where these do not explicitly exist:
    if (length (xy_ord) > 1)
        xy_ord <- insert_junctions (xy_ord)

    xy_ord <- renumber_nodes (xy_ord, i0)

    # TODO: This is not going to work where there are multiple matches: FIX!
    xtop <- list (xy_ord [[1]])
    xbot <- xy_ord
    xbot [[1]] <- NULL
    while (length (xbot) > 0)
    {
        xbot <- match_xbot_xtop (xbot, xtop)
        xtop [[length (xtop) + 1]] <- xbot [[1]]
        xbot [[1]] <- NULL
    }

    return (xtop)
}

extend_ord_list <- function (xy, xy_ord, xy_ord_list)
{
    head <- NULL
    n <- sapply (xy, function (x) all (head (xy_ord, 1) %in% x))
    if (length (which (n)) > 0)
        head <- TRUE
    else if (length (which (n)) == 0)
    {
        n <- sapply (xy, function (x) all (tail (xy_ord, 1) %in% x))
        if (length (which (n)) > 0)
            head <- FALSE
    }
    if (length (which (n)) > 1)
    {
        # check one step ahead: (TODO: improve this)
        nn <- which (n)
        for (i in seq (nn))
        {
            nt <- sapply (xy, function (x)
                          all (tail (xy [[nn [i] ]], 1) %in% x))
            nh <- sapply (xy, function (x)
                          all (head (xy [[nn [i] ]], 1) %in% x))
            if (length (which (nt)) < 2 & length (which (nh)) < 2)
                n [nn [i]] <- FALSE # these don't connect further
        }
    }

    if (is.null (head))
    {
        xy_ord_list [[length (xy_ord_list) + 1]] <- unique (xy_ord)
        xy_ord <- xy [[1]]
        xy [[1]] <- NULL
    } else
    {
        n <- which (n) [1]
        # [1] because the above clause may still return multiple values
        xy_ord <- flip_xyn (xy, xy_ord, n, head)
        xy [[n]] <- NULL
    }

    return (list ('xy' = xy, 'xy_ord' = xy_ord, 'xy_ord_list' = xy_ord_list))
}

#' flip_xyn
#'
#' Check whether xy [[n]] should be flipped before rbind-ing it
#'
#' @noRd
flip_xyn <- function (xy, xy_ord, n, head = TRUE)
{
    if (head)
    {
        temp <- rbind (head (xy_ord, 1), xy [[n]])
        pos <- 2
    } else
    {
        temp <- rbind (tail (xy_ord, 1), xy [[n]])
        pos <- nrow (temp)
    }
    dup <- which (duplicated (temp))
    if (dup == pos) # then flip
        xy [[n]] <- apply (t (xy [[n]]), 1, rev)
    if (head)
        xy_ord <- rbind (xy [[n]], xy_ord)
    else
        xy_ord <- rbind (xy_ord, xy [[n]])

    return (xy_ord)
}

#' insert_junction_j
#'
#' This functio is necessary because there may be more than one intersection,
#' requiring looping over xy.
#'
#' @noRd
insert_junction_j <- function (xy_ord, li, i, j)
{
    lj <- sp::Line (xy_ord [[j]])
    lj <- sp::SpatialLines (list (sp::Lines (list (lj), ID = 'a')))
    xy <- sp::coordinates (rgeos::gIntersection (li, lj))
    if (is.null (nrow (xy)))
        xy <- matrix (xy, nrow = 1, ncol = 2)
    for (k in seq (nrow (xy))) # usually only one
    {
        indx <- get_join_index (xy [k, ], xy_ord [[i]])
        xy_ord [[i]] <- rbind (xy_ord [[i]] [indx [[1]], ],
                               xy [k, ],
                               xy_ord [[i]] [indx [[2]], ])
    }

    return (xy_ord)
}

insert_junctions <- function (xy_ord)
{
    for (i in seq (xy_ord))
    {
        ref <- xy_ord
        ref [[i]] <- NULL
        li <- sp::Line (xy_ord [[i]])
        li <- sp::SpatialLines (list (sp::Lines (list (li), ID = 'a')))
        # ID is filler
        intersections <- flag_intersections (ref, li)
        inref <- xy_in_ref (ref, xy_ord [[i]])
        inum <- (seq (xy_ord)) [!seq (xy_ord) %in% i]
        inum <- inum [which (intersections & !inref)]
        # inum the indexes elements of xy_ord which cross xy_ord [[i]] yet
        # do not have actual junction points. These points are now inserted:
        for (j in inum)
            xy_ord <- insert_junction_j (xy_ord, li, i, j)
    } # end for i

    return (xy_ord)
}

flag_intersections <- function (x, li)
{
    vapply (x, function (i) {
                lj <- sp::Line (i)
                lj <- sp::SpatialLines (list (sp::Lines (list (lj), ID = 'a')))
                !is.null (rgeos::gIntersection (li, lj))
                               },
                logical (1))
}

# check whether point x lies in xy
xy_in_ref <- function (x, xy)
{
    vapply (x, function (j) {
                n <- array (xy %in% j, dim = dim (xy))
                any (rowSums (n) == 2)
                },
                logical (1))
}

# Node numbers are not necessarily unique, so are replaced here with unique
# sequential numbers
renumber_nodes <- function (xy, i0)
{
    for (i in seq (xy))
    {
        rownames (xy [[i]]) <- i0 + seq (nrow (xy [[i]]))
        i0 <- i0 + nrow (xy [[i]])
    }

    return (xy)
}

# Match names in xbot [[1]] to previous xtop elements
match_xbot_xtop <- function (xbot, xtop)
{
    for (i in seq (xtop))
    {
        n <- array (xtop [[i]] %in% xbot [[1]], dim = dim (xtop [[i]]))
        n <- which (rowSums (n) == 2)
        if (length (n) > 0)
        {
            nname <- rownames (xtop [[i]]) [n]
            # Then find match in xbot [[1]]
            n <- array (xbot [[1]] %in% xtop [[i]], dim = dim (xbot [[1]]))
            n <- which (rowSums (n) == 2)
            # And rename it:
            rownames (xbot [[1]]) [n] <- nname
        }
    }
    return (xbot)
}
