#' order.lines
#'
#' Accepts a SpatialLinesDataFrame representing an OpenStreetMap line object
#' such as a highway. The list items of these objects are arbitrarily organised
#' within OpenStreetMap. This function orders the components, returning a list
#' of components each of which is ordered sequentially along the line.
#' Points of intersection between components are also inserted where these are
#' not explicitly present in OpenStreetMap.,
#'
#' @param spLines A SpatialLinesDataFrame returned from extract.osm.objects
#' @return nothing (adds to graphics.device opened with plot.osm.basemap)

order.lines <- function (spLines)
{
    stopifnot (class (spLines) == "SpatialLinesDataFrame")
    # Extract all coords from the SLDF and store as simple list:
    xy <- lapply (slot (spLines,"lines"), function (x)
                  slot (slot (x, "Lines") [[1]], "coords"))
    # Start the ordered data.frame with the first item of xy
    xy.ord <- xy [[1]]
    # If a way contains discrete segments, these are stored in xy.ord.list, and
    # xy.ord itself is reset
    xy.ord.list <- NULL
    xy [[1]] <- NULL
    while (length (xy) > 0)
    {
        head <- NULL
        n <- sapply (xy, function (x) all (head (xy.ord, 1) %in% x))
        if (length (which (n)) > 0)
            head <- TRUE
        else if (length (which (n)) == 0)
        {
            n <- sapply (xy, function (x) all (tail (xy.ord, 1) %in% x))
            if (length (which (n)) > 0)
                head <- FALSE
        }
        if (length (which (n)) > 1)
        {
            # check one step ahead: (TODO: improve this)
            nn <- which (n)
            for (i in 1:length (nn))
            {
                nt <- sapply (xy, function (x) all (tail (xy [[nn [i] ]], 1) %in% x))
                nh <- sapply (xy, function (x) all (head (xy [[nn [i] ]], 1) %in% x))
                # And set to FALSE any that don't connect further
                if (length (which (nt)) < 2 & length (which (nh)) < 2)
                    n [nn [i]] <- FALSE
            }
        }

        if (is.null (head))
        {
            xy.ord.list [[length (xy.ord.list) + 1]] <- unique (xy.ord)
            xy.ord <- xy [[1]]
            xy [[1]] <- NULL
        } else
        {
            n <- which (n) [1] 
            # [1] because the above clause may still return multiple values
            if (head)
            {
                # Check whether xy [[n]] should be flipped before rbind:
                temp <- rbind (head (xy.ord, 1), xy [[n]])
                dup <- which (duplicated (temp))
                if (dup == 2) # then flip
                    xy [[n]] <- apply (t (xy [[n]]), 1, rev)
                xy.ord <- rbind (xy [[n]], xy.ord)
            } else
            {
                temp <- rbind (tail (xy.ord, 1), xy [[n]])
                dup <- which (duplicated (temp))
                if (dup == nrow (temp)) # then flip
                    xy [[n]] <- apply (t (xy [[n]]), 1, rev)
                xy.ord <- rbind (xy.ord, xy [[n]])
            }
            xy [[n]] <- NULL
        }
    }
    if (!is.null (xy.ord.list))
    {
        xy.ord.list [[length (xy.ord.list) + 1]] <- unique (xy.ord)
        xy.ord <- xy.ord.list
    } else
        xy.ord <- list (unique (xy.ord))

    # Then insert intersections where these do not explicitly exist:
    for (i in 1:length (xy.ord))
    {
        ref <- xy.ord
        ref [[i]] <- NULL
        li <- sp::Line (xy.ord [[i]])
        li <- sp::SpatialLines (list (Lines (list (li), ID="a"))) # ID is filler
        intersections <- sapply (ref, function (x) {
                    lj <- sp::Line (x)
                    lj <- sp::SpatialLines (list (Lines (list (lj), ID="a"))) 
                    !is.null (rgeos::gIntersection (li, lj))
                    })
        inref <- sapply (ref, function (x) {
                         n <- array (xy.ord [[i]] %in% x,
                                          dim=dim (xy.ord [[i]]))
                         # If both x and y are in xy.ord [[i]], then both
                         # elements of a row with be TRUE, so:
                         any (rowSums (n) == 2)
                    })
        inum <- (1:length (xy.ord))[!1:length (xy.ord) %in% i]
        inum <- inum [which (intersections & !inref)]
        # inum the indexes elements of xy.ord which cross xy.ord [[i]] yet do
        # not have actual junction points. These points are now inserted:
        stopifnot (length (inum) < 2) # TODO: Check!
        if (length (inum) == 1)
        {
            lj <- sp::Line (xy.ord [[inum]])
            lj <- sp::SpatialLines (list (Lines (list (lj), ID="a"))) 
            xy <- coordinates (rgeos::gIntersection (li, lj))
            # Then distances to from xy to xy.ord [[i]], to enable the junction
            # point to be inserted in the appropriate sequence:
            d <- sqrt ((xy [1] - xy.ord [[i]] [,1])^2 +
                       (xy [2] - xy.ord [[i]] [,2])^2)
            di <- which.min (d)
            n <- nrow (xy.ord [[i]])
            if (d [di-1] < d [di+1])
                xy.ord [[i]] <- rbind (xy.ord [[i]] [1:(di-1),], xy, 
                                     xy.ord [[i]] [di:n,])
            else
                xy.ord [[i]] <- rbind (xy.ord [[i]] [1:di,], xy, 
                                     xy.ord [[i]] [(di+1):n,])
        }
    }
    
    # Then re-number all nodes. They have numbers generated by osmar::as_sp, but
    # these are not unique, and so must be replaced by unique codes. First just
    # number all sequentially:
    i0 <- 0
    for (i in 1:length (xy.ord))
    {
        rownames (xy.ord [[i]]) <- i0 + 1:nrow (xy.ord [[i]])
        i0 <- i0 + nrow (xy.ord [[i]])
    }
    # Then renumber all shared nodes from [[2]] onwards to correspond to
    # any prior node names
    xtop <- list (xy.ord [[1]])
    xbot <- xy.ord
    xbot [[1]] <- NULL
    while (length (xbot) > 0)
    {
        # Find whether any items in xbot [[1]] are in previous xtop elements:
        nt <- lapply (xtop, function (i) {
                     n <- array (i %in% xbot [[1]], dim=dim (i))
                     which (rowSums (n) == 2) })
        nname <- lapply (seq (nt), function (i) 
                         rownames (xtop [[i]]) [nt [[i]]])
        # Then find where the matching elements are in xbot [[1]]
        nb <- lapply (xtop, function (i) 
                      {
                      n <- array (xbot [[1]] %in% i, dim=dim (xbot [[1]]))
                      which (rowSums (n) == 2)  
                      })
        # Rename them (this works because length (nname) == length (nb)
        for (i in seq (nb))
            if (length (nb [[i]]) > 0)
                rownames (xbot [[1]]) [nb [[i]]] <- nname [[i]]
        # And move xbot [[1]] to xtop
        xtop [[length (xtop) + 1]] <- xbot [[1]]
        xbot [[1]] <- NULL
    }

    return (xy.ord)
}
