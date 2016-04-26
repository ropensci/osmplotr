#' order_lines
#'
#' Accepts a SpatialLinesDataFrame representing an OpenStreetMap line object
#' such as a highway. The list items of these objects are arbitrarily organised
#' within OpenStreetMap. This function orders the components, returning a list
#' of components each of which is ordered sequentially along the line.
#' Points of intersection between components are also inserted where these are
#' not explicitly present in OpenStreetMap. Nodes are also sequentially
#' renumbered, starting at (i0+1)
#'
#' @param spLines A SpatialLinesDataFrame returned from extract_osm_objects()
#' @param i0 The first node is numbered (i0+1), with other nodes (except
#' junction nodes) numbered sequentially .
#' @return A list of ordered line segments.
#'
#' @section Note:
#' This function is primarily used in extract_highways()

order_lines <- function (spLines, i0=0)
{
    stopifnot (class (spLines) == 'SpatialLinesDataFrame')
    # Extract all coords from the SLDF and store as simple list:
    xy <- sp::coordinates (spLines)
    xy <- lapply (xy, function (x) array (unlist (x), dim=dim(x[[1]]) ))
    # Start the ordered data.frame with the first item of xy
    xy_ord <- xy [[1]]
    # If a way contains discrete segments, these are stored in xy_ord_list, and
    # xy_ord itself is reset
    xy_ord_list <- NULL
    xy [[1]] <- NULL
    while (length (xy) > 0)
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
            for (i in 1:length (nn))
            {
                nt <- sapply (xy, function (x) 
                              all (tail (xy [[nn [i] ]], 1) %in% x))
                nh <- sapply (xy, function (x) 
                              all (head (xy [[nn [i] ]], 1) %in% x))
                # And set to FALSE any that don't connect further
                if (length (which (nt)) < 2 & length (which (nh)) < 2)
                    n [nn [i]] <- FALSE
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
            if (head)
            {
                # Check whether xy [[n]] should be flipped before rbind:
                temp <- rbind (head (xy_ord, 1), xy [[n]])
                dup <- which (duplicated (temp))
                if (dup == 2) # then flip
                    xy [[n]] <- apply (t (xy [[n]]), 1, rev)
                xy_ord <- rbind (xy [[n]], xy_ord)
            } else
            {
                temp <- rbind (tail (xy_ord, 1), xy [[n]])
                dup <- which (duplicated (temp))
                if (dup == nrow (temp)) # then flip
                    xy [[n]] <- apply (t (xy [[n]]), 1, rev)
                xy_ord <- rbind (xy_ord, xy [[n]])
            }
            xy [[n]] <- NULL
        }
    }
    if (!is.null (xy_ord_list))
    {
        xy_ord_list [[length (xy_ord_list) + 1]] <- unique (xy_ord)
        xy_ord <- xy_ord_list
    } else
        xy_ord <- list (unique (xy_ord))

    # Then insert intersections where these do not explicitly exist:
    if (length (xy_ord) > 1)
        for (i in 1:length (xy_ord))
        {
            ref <- xy_ord
            ref [[i]] <- NULL
            li <- sp::Line (xy_ord [[i]])
            li <- sp::SpatialLines (list (Lines (list (li), ID='a'))) 
            # ID is filler
            intersections <- sapply (ref, function (x) {
                        lj <- sp::Line (x)
                        lj <- sp::SpatialLines (list (Lines (list (lj), ID='a'))) 
                        !is.null (rgeos::gIntersection (li, lj))
                        })
            inref <- sapply (ref, function (x) {
                             n <- array (xy_ord [[i]] %in% x,
                                              dim=dim (xy_ord [[i]]))
                             # If both x and y are in xy_ord [[i]], then both
                             # elements of a row with be TRUE, so:
                             any (rowSums (n) == 2)
                        })
            inum <- (1:length (xy_ord))[!1:length (xy_ord) %in% i]
            inum <- inum [which (intersections & !inref)]
            # inum the indexes elements of xy_ord which cross xy_ord [[i]] yet
            # do not have actual junction points. These points are now inserted:
            for (j in inum)
            {
                lj <- sp::Line (xy_ord [[j]])
                lj <- sp::SpatialLines (list (Lines (list (lj), ID='a'))) 
                xy <- sp::coordinates (rgeos::gIntersection (li, lj))
                # Then distances to from xy to xy_ord [[i]], to enable the
                # junction point to be inserted in the appropriate sequence.
                # There may however be more than one intersection, requiring
                # looping over xy.
                if (is.null (nrow (xy)))
                    xy <- matrix (xy, nrow=1, ncol=2)
                for (k in seq (nrow (xy))) # usually only one
                {
                    d <- sqrt ((xy [k,1] - xy_ord [[i]] [,1])^2 +
                               (xy [k,2] - xy_ord [[i]] [,2])^2)
                    di <- which.min (d)
                    n <- nrow (xy_ord [[i]])
                    # xy can be closest to d1, but still either
                    # A. -------d1---xy--------------d2, or
                    # B. xy-----d1-------------------d2
                    # A. implies that |xy,d2|<|d1,d2|, and B vice-versa
                    if (di == 1)
                    {
                        d12 <- sqrt (diff (xy_ord [[i]] [1:2,1]) ^ 2 +
                                     diff (xy_ord [[i]] [1:2,2]) ^ 2)
                        if (d12 < d [2])
                            indx <- list (NULL, 1:n)
                        else
                            indx <- list (1, 2:n)
                    } else if (di == n)
                    {
                        d12 <- sqrt (diff (xy_ord [[i]] [(n-1:n),1]) ^ 2 +
                                     diff (xy_ord [[i]] [(n-1:n),2]) ^ 2)
                        if (d12 < d [n-1])
                            indx <- list (1:n, NULL)
                        else
                            indx <- list (1:(n-1), n)
                    } else if (d [di - 1] < d [di + 1])
                        indx <- list (1:(di-1), di:n)
                    else
                        indx <- list (1:di, (di+1):n)
                    xy_ord [[i]] <- rbind (xy_ord [[i]] [indx [[1]], ],
                                           xy [k,],
                                           xy_ord [[i]] [indx [[2]], ])
                }
            } # end for j
        } # end for i
    
    # Then re-number all nodes. They have numbers generated by osmar::as_sp, but
    # these are not unique, and so must be replaced by unique codes. First just
    # number all sequentially:
    for (i in 1:length (xy_ord))
    {
        rownames (xy_ord [[i]]) <- i0 + 1:nrow (xy_ord [[i]])
        i0 <- i0 + nrow (xy_ord [[i]])
    }
    # Then renumber all shared nodes from [[2]] onwards to correspond to
    # any prior node names.
    # TODO: This is not going to work where there are multiple matches: FIX!
    xtop <- list (xy_ord [[1]])
    xbot <- xy_ord
    xbot [[1]] <- NULL
    while (length (xbot) > 0)
    {
        # Find whether any items in xbot [[1]] are in previous xtop elements:
        for (i in 1:length (xtop))
        {
            n <- array (xtop [[i]] %in% xbot [[1]], dim=dim (xtop [[i]]))
            n <- which (rowSums (n) == 2)
            if (length (n) > 0)
            {
                nname <- rownames (xtop [[i]]) [n]
                # Then find match in xbot [[1]]
                n <- array (xbot [[1]] %in% xtop [[i]], dim=dim (xbot [[1]]))
                n <- which (rowSums (n) == 2)
                # And rename it:
                rownames (xbot [[1]]) [n] <- nname
            }
        }

        # Finally move xbot [[1]] to xtop
        xtop [[length (xtop) + 1]] <- xbot [[1]]
        xbot [[1]] <- NULL
    }

    return (xtop)
}
