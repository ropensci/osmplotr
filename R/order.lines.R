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
    xy <- lapply (slot (spLines,"lines"), function (x)
                  slot (slot (x, "Lines") [[1]], "coords"))
    # Start the ordered data.frame with the first SpatialLines element 
    xy.ord <- xy [[1]]
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
        if (length (which (n)) == 0)
            warning ("xy.ord[[", length (xy.ord), "]] does not connect.")
        else if (length (which (n)) > 1)
        {
            # check one step ahead:
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
            warning ("head is null at len (xy) = ", length (xy))
        stopifnot (!is.null (head))

        n <- which (n) [1] 
        # [1] because the above clause may still return multiple values
        if (head)
            xy.ord <- rbind (xy [[n]], xy.ord)
        else
            xy.ord <- rbind (xy.ord, xy [[n]])
        xy [[n]] <- NULL
    }
    unique (xy.ord)
}
