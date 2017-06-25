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
