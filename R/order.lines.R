#' order.lines
#'
#' Accepts a SpatialLinesDataFrame representing an OpenStreetMap line object
#' such as a highway. The list items of these objects are arbitrarily organised
#' within OpenStreetMap. This function orders the components, returning a single
#' data frame with coordinates ordered sequentially along the line.
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
    return (xy.ord)
}
