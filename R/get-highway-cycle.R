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
        if (all (is.na (es$cyc_len_i)))
        {
            warning ('No cycles can be found or made')
            break
        } else if (max (es$cyc_len_i, na.rm = TRUE) <= cyc_len)
        {
            warning ('Cycle unable to be extended through all ways')
            break
        } else
            cyc_len <- max (es$cyc_len_i, na.rm = TRUE)

        nc <- nodes_to_connect (ways, es)
        ways <- insert_connecting_nodes (ways, es, nc)
    } # end while cyc_len < length (ways)

    return (ways)
}

get_maxvert <- function (ways)
{
    max (vapply (ways, function (x) max (as.numeric (rownames (x))), numeric (1))) + 1
}

#' add intersection nodes to junctions of ways where these don't already exist
#'
#' This routine is necessary because \code{ways} are generally just named
#' highways which will not necessarily include the actual OSM intersection nodes
#' between each way.
#'
#' @param ways A list of highways as returned by
#' \code{\link{extract_highways}}, each element of which is a list of distinct
#' segments for a particular OSM highway.
#'
#' @noRd
add_intersection_nodes <- function (ways, maxvert)
{
    for (i in seq (ways))
    {
        # The following function returns default of -1 for no geometric
        # intersection; 0 where intersections exists but area *NOT* vertices
        # of li, and 1 where intersections are vertices of li.
        test <- ways
        test [[i]] <- NULL
        intersections <- vapply (test, function (x)
                                 intersection_type (ways [[i]], x), numeric (1))
        if (any (intersections == 0))
            for (j in which (intersections == 0))
            {
                # Then they have to be added to ways [[i]] [[j]].
                lj <- sp::Line (test [[j]])
                lj <- sp::SpatialLines (list (sp::Lines (list (lj), ID = 'a')))
                xy <- sp::coordinates (rgeos::gIntersection (li, lj))
                ways [[i]] <- insert_join (xy, ways [[i]], maxvert)

                # Then add same vertex into the other elements
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
                # Then ni needs to point into the full ways instead of test
                ni <- seq (ways) [!seq (ways) %in% i] [ni]
                # Then insert xy into ways [[ni]] [[nj]]
                ways [[ni]] [[nj]] <- insert_join (xy, ways [[ni]] [[nj]],
                                                   maxvert)
                ways [[ni]] [[nj]] <- unique (ways [[ni]] [[nj]])

                maxvert <- maxvert + 1
            } # end for k over which (intersections == 0)

        test <- ways
        test [[i]] <- NULL
        test_flat <- do.call (c, test)
        # Check whether any of ways [[i]] cross any of test_flat *and* don't
        # already exist as vertices
        for (j in seq (ways [[i]]))
        {
            li <- sp::Line (ways [[i]] [[j]])
            li <- sp::SpatialLines (list (sp::Lines (list (li), ID = 'a')))
            # The following function returns default of -1 for no geometric
            # intersection; 0 where intersections exists but area *NOT* vertices
            # of li, and 2 where intersections are vertices of li.
            intersections <- sapply (test_flat, function (x)
                                     intersection_type (x, li))
            if (any (intersections == 0))
                for (k in which (intersections == 0))
                {
                    # Then they have to be added to ways [[i]] [[j]].
                    x <- test_flat [k] [[1]]
                    lj <- sp::Line (x)
                    lj <- sp::SpatialLines (list (sp::Lines (list (lj),
                                                             ID = 'a')))
                    xy <- sp::coordinates (rgeos::gIntersection (li, lj))
                    ways [[i]] [[j]] <- insert_join (xy, ways [[i]] [[j]],
                                                     maxvert)

                    # Then add same vertex into the other elements
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
                    # Then ni needs to point into the full ways instead of test
                    ni <- seq (ways) [!seq (ways) %in% i] [ni]
                    # Then insert xy into ways [[ni]] [[nj]]
                    ways [[ni]] [[nj]] <- insert_join (xy, ways [[ni]] [[nj]],
                                                       maxvert)
                    ways [[ni]] [[nj]] <- unique (ways [[ni]] [[nj]])

                    maxvert <- maxvert + 1
                } # end for k over which (intersections == 0)
        } # end for j over obj [[i]]
    } # end for i over all ways

    return (ways)
}

#' intersection_type
#'
#' Get type of intersection in a given line
#'
#' @param x1 Coordinates of one highway
#' @param x2 Coordinates of another highway
#'
#' @return Default of -1 for no geometric intersection; 0 where intersections
#' exists but area *NOT* vertices of li, and 1 where intersections are vertices
#' of li.
#'
#' @noRd
intersection_type <- function (x1, x2)
{
    type <- -1
    if (any (rownames (x1) %in% rownames (x2)))
        type <- 1
    else
    {
        li <- sp::Line (x1)
        li <- sp::SpatialLines (list (sp::Lines (list (li), ID = 'a')))
        lj <- sp::Line (x2)
        lj <- sp::SpatialLines (list (sp::Lines (list (lj), ID = 'a')))

        if (!is.null (rgeos::gIntersection (li, lj)))
            type <- 0
    }

    return (type)
}

#' extend_cycles
#'
#' Find lengths of cycles formed by adding all possible single individual links
#' in a conmat
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
    cyc_len_i <- rep (NA, length (i1))
    for (i in seq (i1))
    {
        conmat [i1 [i], i2 [i]] <- TRUE
        conmat [i2 [i], i1 [i]] <- TRUE
        cycles <- try (ggm::fundCycles (conmat), TRUE)
        if (is (attr (cycles, "condition"), "simpleError"))
            cycles <- NULL
        if (!is.null (cycles))
            cyc_len_i [i] <- max (sapply (cycles, nrow))
    }

    return (list ('cyc_len_i' = cyc_len_i, 'i1' = i1, 'i2' = i2,
                  'conmat' = conmat))
}

#' nodes_to_connect
#'
#' @param ways A list of highways as returned by
#' \code{\link{extract_highways}}, each element of which is a list of distinct
#' segments for a particular OSM highway.
#' @param es Result of call to \code{extend_cycles}
#'
#' Determine the actual nodes of the extended cycle which are to be connected
#'
#' @noRd
nodes_to_connect <- function (ways, es)
{
    # Then connect the actual ways corresponding to the longest cycle
    i1 <- es$i1 [which.max (es$cyc_len_i)]
    i2 <- es$i2 [which.max (es$cyc_len_i)]
    h1x <- array (ways [[i1]] [, 1],
                  dim = c(nrow (ways [[i1]]), nrow (ways [[i2]])))
    h1y <- array (ways [[i1]] [, 2],
                  dim = c(nrow (ways [[i1]]), nrow (ways [[i2]])))
    h2x <- t (array (ways [[i2]] [, 1],
                     dim = c(nrow (ways [[i2]]), nrow (ways [[i1]]))))
    h2y <- t (array (ways [[i2]] [, 2],
                     dim = c(nrow (ways [[i2]]), nrow (ways [[i1]]))))
    d <- sqrt ( (h1x - h2x) ^ 2 + (h1y - h2y) ^ 2)
    di1 <- which.min (apply (d, 1, min))
    di2 <- which.min (apply (d, 2, min))
    node1 <- rownames (ways [[i1]]) [di1]
    xy1 <- ways [[i1]] [di1, ]
    node2 <- rownames (ways [[i2]]) [di2]
    xy2 <- ways [[i2]] [di2, ]

    list ('node1' = node1, 'node2' = node2, 'xy1' = xy1, 'xy2' = xy2)
}

#' insert_connecting_nodes
#'
#' Nodes are only inserted in the respective highways where the joining node is
#' terminal. This insertion adds to the terminal node of one way an additional
#' node that will generally lie in the middle of some other way, thereby
#' connecting the two.
#'
#' @param ways A list of flat highways as returned by \code{flatten_highways}
#' @param es Result of call to \code{extend_cycles}
#' @param nc Result of call to \code{nodes_to_connect}
#'
#' @noRd
insert_connecting_nodes <- function (ways, es, nc)
{
    i1 <- es$i1 [which.max (es$cyc_len_i)]
    i2 <- es$i2 [which.max (es$cyc_len_i)]

    nsub1 <- nc$node2
    xysub1 <- nc$xy2
    ni1 <- max (c (-1, which (rownames (ways [[i1]]) == nc$node1)))
    if (!ni1 %in% c (1, nrow (ways [[i1]])))
    {
        nsub1 <- nc$node1
        xysub1 <- nc$xy1
        ni1 <- max (c (-1, which (rownames (ways [[i1]]) == nc$node2)))
    }
    nsub2 <- nc$node1
    xysub2 <- nc$xy1
    ni2 <- max (c (-1, which (rownames (ways [[i2]]) == nc$node2)))
    if (!ni2 %in% c (1, nrow (ways [[i2]])))
    {
        nsub2 <- nc$node2
        xysub2 <- nc$xy2
        ni2 <- max (c (-1, which (rownames (ways [[i2]]) == nc$node1)))
    }

    if (ni1 == 1)
    {
        hnames <- c (nsub1, rownames (ways [[i1]]))
        ways [[i1]] <- rbind (xysub1, ways [[i1]])
        rownames (ways [[i1]]) <- hnames
    } else if (ni1 == nrow (ways [[i1]]))
    {
        hnames <- c (rownames (ways [[i1]]), nsub1)
        ways [[i1]] <- rbind (ways [[i1]], xysub1)
        rownames (ways [[i1]]) <- hnames
    } else if (ni1 == 1)
    {
        hnames <- c (nsub2, rownames (ways [[i2]]))
        ways [[i2]] <- rbind (xysub2, ways [[i2]])
        rownames (ways [[i2]]) <- hnames
    } else if (ni2 == nrow (ways [[i2]]))
    {
        hnames <- c (rownames (ways [[i2]]), nsub2)
        ways [[i2]] <- rbind (ways [[i2]], xysub2)
        rownames (ways [[i2]]) <- hnames
    }

    return (ways)
}

#' insert_terminal_node
#'
#' Insert terminal connecting node (\code{nc$node1}) to form cycle
#'
#' @param highways A list of highways as returned by
#' \code{\link{extract_highways}}, each element of which is a list of distinct
#' segments for a particular OSM highway.
#' @param es Result of call to \code{extend_cycles}
#' @param nc Result of call to \code{nodes_to_connect}
#'
#' @noRd
insert_terminal_node <- function (highways, es, nc)
{
    es$i2 <- es$i2 [which.max (es$cyc_len_i)]
    ni1 <- sapply (highways [[es$i2]], function (x)
                   max (c (-1, which (rownames (x) == nc$node2))))
    ni2 <- which.max (ni1)
    if (max (ni1) == 1)
    {
        hnames <- rbind (nc$node1, names (highways [[es$i2]] [[ni2]]))
        highways [[es$i2]] [[ni2]] <- rbind (nc$xy1,
                                             highways [[es$i2]] [[ni2]])
        names (highways [[es$i2]] [[ni2]]) <- hnames
    } else if (max (ni1) == nrow (highways [[es$i2]] [[ni2]]))
    {
        hnames <- rbind (names (highways [[es$i2]] [[ni2]]), nc$node1)
        highways [[es$i2]] [[ni2]] <- rbind (highways [[es$i2]] [[ni2]],
                                             nc$xy1)
        names (highways [[es$i2]] [[ni2]]) <- hnames
    }

    return (highways)
}

#' get_join_index
#'
#' Get indexes into two ways to allow them to be sequentially joined - generally
#' in the form \code{1:n} or \code{n:1}.
#'
#' @param xy coordinates of intersection
#' @param obj object used to calculate minimal distance
#'
#' @noRd
get_join_index <- function (xy, obj)
{
    d <- sqrt ( (xy [1] - obj [, 1]) ^ 2 +
               (xy [2] - obj [, 2]) ^ 2)
    di <- which.min (d)
    n <- nrow (obj)
    # xy can be closest to d1, but still either
    # A. -------d1---xy--------------d2, or
    # B. xy-----d1-------------------d2
    # A. implies that |xy,d2|<|d1,d2|, and B vice-versa
    if (di == 1)
    {
        d12 <- sqrt (diff (obj [1:2, 1]) ^ 2 +
                     diff (obj [1:2, 2]) ^ 2)
        if (d12 < d [2])
            indx <- list (NULL, 1:n)
        else
            indx <- list (1, 2:n)
    } else if (di == n)
    {
        d12 <- sqrt (diff (obj [(n - 1:n), 1]) ^ 2 +
                     diff (obj [(n - 1:n), 2]) ^ 2)
        if (all (d12 < d [n - 1]))
            indx <- list (1:n, NULL)
        else
            indx <- list (1:(n - 1), n)
    } else if (d [di - 1] < d [di + 1])
        indx <- list (1:(di - 1), di:n)
    else
        indx <- list (1:di, (di + 1):n)

    return (indx)
}

#' insert_join
#'
#' Insert joining node into ways
#'
#' @param xy coordinates of intersection
#' @param way way in which joining node is to be inserted
#' @param maxvert maximal vertex number
#'
#' @note The \code{xy} values returned from \code{rgeos::gIntersection} are
#' \code{List} objects whenever there are multiple intersections. These cases
#' are simply ignored here.
#'
#' @noRd
insert_join <- function (xy, way, maxvert)
{
    if (is.matrix (xy))
        if (nrow (xy) == 1)
        {
            rnames <- rownames (way)
            indx <- get_join_index (xy, way)
            way <- rbind (way [indx [[1]], ], xy, way [indx [[2]], ])
            rownames (way) <- c (rnames [indx [[1]]], maxvert,
                                 rnames [indx [[2]]])
        }

    return (way)
}
