#' line2poly
#'
#' Converts \code{sf::sfc_LINSTRING} objects to polygons by connecting end
#' points around the given bounding box. This is particularly useful for
#' plotting water and land delineated by coastlines. Coastlines in OpenStreetMap
#' are lines, not polygons, and so there is no directly way to plot ocean water
#' distinct from land. This function enables that by connecting the end points
#' of coastline \code{LINESTRING} objects to form closed polygons.
#'
#' The function always returns two polygons. In the case of coastlines, one will
#' represent the ocean; the other the land; and it will be up to the user to
#' determine which is which.
#'
#' @param obj A Simple Features (\code{sf}) data frame of lines, typically as
#' returned by \code{\link{extract_osm_objects}}, or by
#' \code{osmdata::osmdata_sf}.
#' @param bbox bounding box (Latitude-longitude range) to be plotted.  A 2-by-2
#' matrix of 4 elements with columns of min and max values, and rows of x and y
#' values. Can also be an object of class \code{sf}, for example as returned
#' from \code{extract_osm_objects} or the \code{osmdata} package, in which case
#' the bounding box will be extracted from the object coordinates.
#' @return A list of two Simple Features (\code{sf}) data frames, each
#' containing one polygon created by connecting the end points of \code{obj} to
#' the corresponding extrema of \code{bbox}.
#' @export
#'
#' @examples
#' # This example uses the \code{osmdata} package to extract data from 
#' # a named bounding box
#' \dontrun{
#' library (magrittr)
#' library (osmdata)
#' bb <- getbb ("melbourne, australia")
#' coast <- opq (bb) %>%
#'     add_osm_feature (key = "natural", value = "coastline") %>%
#'     osmdata_sf ()
#' coast <- line2poly (coast$osm_lines, bbox = bb)
#' # The following map then colours in just the ocean:
#' map <- osm_basemap (bbox = bb) %>%
#'     add_osm_objects (coast [[1]], col = "lightsteelblue") %>%
#'     print_osm_map ()
#' }
line2poly <- function (obj, bbox)
{
    if (!is (obj$geometry, "sfc_LINESTRING"))
        stop ("obj must be class 'sf' with fields of class 'sfc_LINESTRING'")

    g <- obj$geom
    out <- as.matrix (g [[1]])
    g [[1]] <- NULL
    alldone <- FALSE
    while (!alldone)
    {
        matches <- unlist (lapply (g, function (i)
                           any (rownames (i) == tail (rownames (out), 1) [1])))
        if (any (matches))
        {
            gadd <- g [[which (matches) [1] ]]
            out <- rbind (out, gadd [2:nrow (gadd), ])
            g [[which (matches) [1] ]] <- NULL
        } else
        {
            matches <- unlist (lapply (g, function (i)
                                   any (rownames (i) == rownames (out) [1])))
            if (!any (matches))
            {
                alldone <- TRUE
            } else
            {
                gadd <- g [[which (matches) [1] ]]
                out <- rbind (gadd, out [2:nrow (out), ])
                g [[which (matches) [1] ]] <- NULL
            }
        }
        if (length (g) == 0)
            alldone <- TRUE
    }

    # Then reduce out to only that portion within bb, plus one point either side
    indx <- which (out [, 1] >= bbox [1, 1] & out [, 1] <= bbox [1, 2] &
                   out [, 2] >= bbox [2, 1] & out [, 2] <= bbox [2, 2])
    indx <- c (min (indx) - 1, indx, max (indx) + 1)
    indx <- indx [which (indx > 0 & indx <= nrow (out))]
    out <- out [indx, ]

    if (length (g) > 0)
        message ("Not all line segments align continuously.")

    # polygon formed by expanding bbox through combination of increasing or
    # decreasing longitudes or latitudes. Requires explicit combinations for
    # each case. Note that bb is arranged thus:
    #   bb [1, 1]           bb [1, 2]
    #       |                   |
    #       O-------------------O  <- bb [2, 2]
    #       |                   |
    #       |                   |
    #       O-------------------O  <- bb [2, 1]
    #
    # The three required combinations for starting at -lon are then like this,
    # where double lines are the bbox, and single lines the expansion
    #
    # out[1,1],bb[2,2]      out[n,1],bb[2,2]
    #   |---O====================O---|
    #   |   ||                  ||   |
    # 1-|   ||                  ||   |-N
    #   |   ||                  ||   |
    #   |---O====================O---|
    # out[1,1],bb[2,1]      out[n,1],bb[2,1]
    #
    #
    # out[1,1],bb[2,2]      bb[1,2],bb[2,2]
    #   |---O====================O
    #   |   ||                  ||
    # 1-|   ||                  ||
    #   |   ||                  ||
    #   |   O====================O
    #   |             |          |
    #   |-------------N----------|
    # out[1,1],out[n,2]     bb[1,2],out[n,2]
    #
    #
    # out[1,1],out[n,2]     bb[1,2],out[n,2]
    #   |------------N-----------|
    #   |            |           |
    #   |   O====================O
    #   |   ||                  ||
    # 1-|   ||                  ||
    #   |   ||                  ||
    #   |---O====================O
    # out[1,1],bb[2,1]      bb[1,2],bb[2,1]
    #
    # ... explicit cases for each of the other 3 starting points (+lon, +/- lat)
    # then follow by extension.

    p1 <- p2 <- NULL
    n <- nrow (out)
    bb <- bbox # makes following code slightly easier to read
    if (out [1, 1] <= bbox [1, 1]) # -lon
    {
        if (out [n, 1] >= bb [1, 2]) # -lon -> +lon
        {
            p1 <- rbind (out,
                         c (out [n, 1], bb [2, 1]),
                         c (out [1, 1], bb [2, 1]),
                         out [1, ])
            p2 <- rbind (out,
                         c (out [n, 1], bb [2, 2]),
                         c (out [1, 1], bb [2, 2]),
                         out [1, ])
        } else if (out [n, 2] <= bb [2, 1]) # -lon -> -lat
        {
            p1 <- rbind (out,
                         c (out [1, 1], out [n, 2]),
                         out [1, ])
            p2 <- rbind (out,
                         c (bb [1, 2], out [n, 2]),
                         c (bb [1, 2], bb [2, 2]),
                         c (out [1, 1], bb [2, 2]),
                         out [1, ])
        } else if (out [n, 2] >= bb [2, 2]) # -lon -> +lat
        {
            p1 <- rbind (out,
                         c (out [1, 1], out [n, 2]),
                         out [1, ])
            p2 <- rbind (out,
                         c (bb [1, 2], out [n, 2]),
                         c (bb [1, 2], bb [2, 1]),
                         c (out [1, 1], bb [2, 1]),
                         out [1, ])
        }
    } else if (out [1, 1] >= bb [1, 2]) # +lon
    {
        if (out [n, 1] <= bb [1, 1]) # +lon -> -lon
        {
            p1 <- rbind (out,
                         c (out [n, 1], bb [2, 2]),
                         c (out [1, 1], bb [2, 2]),
                         out [1, ])
            p2 <- rbind (out,
                         c (out [n, 1], bb [2, 1]),
                         c (out [1, 1], bb [2, 1]),
                         out [1, ])
        } else if (out [n, 2] <= bb [2, 1]) # +lon -> -lat
        {
            p1 <- rbind (out,
                         c (out [1, 1], out [n, 2]),
                         out [1, ])
            p2 <- rbind (out,
                         c (bb [1, 1], out [n, 2]),
                         c (bb [1, 1], bb [2, 2]),
                         c (out [1, 1], bb [2, 2]),
                         out [1, ])
        } else if (out [n, 2] >= bb [2, 2]) # +lon -> +lat
        {
            p1 <- rbind (out,
                         c (out [1, 1], out [n, 2]),
                         out [1, ])
            p2 <- rbind (out,
                         c (bb [1, 1], out [n, 2]),
                         c (bb [1, 1], bb [2, 1]),
                         c (out [1, 1], bb [2, 1]),
                         out [1, ])
        }
    } else if (out [1, 2] <= bb [2, 1]) # -lat
    {
        if (out [n, 2] >= bb [2, 2]) # -lat -> +lat
        {
            p1 <- rbind (out,
                         c (bb [1, 2], out [n, 2]),
                         c (bb [1, 2], out [1, 2]),
                         out [1, ])
            p2 <- rbind (out,
                         c (bb [1, 1], out [n, 2]),
                         c (bb [1, 1], out [1, 2]),
                         out [1, ])
        } else if (out [n, 1] >= bb [1, 2]) # -lat -> +lon
        {
            p1 <- rbind (out,
                         c (out [n, 1], out [1, 2]),
                         out [1, ])
            p2 <- rbind (out,
                         c (out [n, 1], bb [2, 2]),
                         c (bb [1, 1], bb [2, 2]),
                         c (bb [1, 1], out [1, 2]),
                         out [1, ])
        } else if (out [n, 1] <= bb [1, 1]) # -lat -> -lon
        {
            p1 <- rbind (out,
                         c (out [n, 1], out [1, 2]),
                         out [1, ])
            p2 <- rbind (out,
                         c (out [n, 1], bb [2, 2]),
                         c (bb [1, 2], bb [2, 2]),
                         c (bb [1, 2], out [1, 2]),
                         out [1, ])
        }
    } else if (out [1, 2] >= bb [2, 2]) # +lat
    {
        if (out [n, 2] <= bb [2, 1]) # +lat -> -lat
        {
            p1 <- rbind (out,
                         c (bb [1, 2], out [n, 2]),
                         c (bb [1, 2], out [1, 2]),
                         out [1, ])
            p2 <- rbind (out,
                         c (bb [1, 1], out [n, 2]),
                         c (bb [1, 1], out [1, 2]),
                         out [1, ])
        } else if (out [n, 1] >= bb [1, 2]) # +lat -> +lon
        {
            p1 <- rbind (out,
                         c (out [n, 1], out [1, 2]),
                         out [1, ])
            p2 <- rbind (out,
                         c (out [n, 1], bb [2, 1]),
                         c (bb [1, 1], bb [2, 1]),
                         c (bb [1, 1], out [1, 2]),
                         out [1, ])
        } else if (out [n, 1] <= bb [1, 1]) # +lat -> -lon
        {
            p1 <- rbind (out,
                         c (out [n, 1], out [1, 2]),
                         out [1, ])
            p2 <- rbind (out,
                         c (out [n, 1], bb [2, 1]),
                         c (bb [1, 2], bb [2, 1]),
                         c (bb [1, 2], out [1, 2]),
                         out [1, ])
        }
    }

    res <- NULL
    if (!is.null (p1) & !is.null (p2))
        res <- list (make_sf (p1, g), make_sf (p2, g))
    return (res)
}



# The df bits directly adapted from same fn in osmdata/get-osmdata.R in
# simplified form; the initial "sfg" and "sfc" bits also cribbed from osmdata
make_sf <- function (x, g)
{
    x <- list (x)
    class (x) <- c ("XY", "POLYGON", "sfg")

    x <- list (x)
    attr (x, "n_empty") <- 0
    class (x) <- c ("sfc_POLYGON", "sfc")
    attr (x, "precision") <- 0.0
    attr (x, "bbox") <- attr (g, "bbox")
    attr (x, "crs") <- attr (g, "crs")

    df <- data.frame (row.names = "1")
    df [["geometry"]] <- x
    attr (df, "sf_column") <- "geometry"
    f <- factor(rep(NA_character_, length.out = ncol(df) - 1),
                levels = c ("constant", "aggregate", "identity"))
    names(f) <- names(df)[-ncol (df)]
    attr(df, "agr") <- f
    class(df) <- c("sf", class(df))
    return (df)
}
