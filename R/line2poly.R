#' osm_line2poly
#'
#' Converts \code{sf::sfc_LINSTRING} objects to polygons by connecting end
#' points around the given bounding box. This is particularly useful for
#' plotting water and land delineated by coastlines. Coastlines in OpenStreetMap
#' are lines, not polygons, and so there is no directly way to plot ocean water
#' distinct from land. This function enables that by connecting the end points
#' of coastline \code{LINESTRING} objects to form closed polygons.
#'
#' This is a tricky problem for a number of reasons, and the current implementation
#' may not be correct, although it does successfully deal with a few tough situations.
#' Some of the issues are: an osm coastline query returns a mixture of "ways" and polygons.
#' Polygons correspond to islands, but not all islands are polygons. A "way" is a connected
#' set of points with the land on the left. A piece of coastline in a bounding box
#' may consist of multiple ways, which need to be connected together to create
#' a polygon. Also, ways extend outside the query bounding box, and may join
#' other ways that enter the bounding box (e.g ends of a peninsula). The degree
#' to which this happens depends on the scale of the bounding box. Coastlines may
#' enter at any bounding box edge and exit at any other, including the one they
#' entered from. 
#'
#' @param obj A Simple Features (\code{sf}) data frame of lines, typically as
#' returned by \code{\link{extract_osm_objects}}, or by
#' \code{osmdata::osmdata_sf}.
#' @param bbox bounding box (Latitude-longitude range) to be plotted.  A 2-by-2
#' matrix of 4 elements with columns of min and max values, and rows of x and y
#' values. Can also be an object of class \code{sf}, for example as returned
#' from \code{extract_osm_objects} or the \code{osmdata} package, in which case
#' the bounding box will be extracted from the object coordinates.
#' @return A list of three Simple Features (\code{sf}) data frames, labelled sea
#' islands and land.
#' @export
#'
#' @examples
#' # This example uses the \code{osmdata} package to extract data from 
#' # a named bounding box
#' \dontrun{
#' library (magrittr)
#' library (osmdata)
#' bb <- osmdata::getbb ("melbourne, australia")
#' coast <- extract_osm_objects (bbox = bb, key = "natural", value = "coastline",
#'                               return_type = "line")
#' coast <- osm_line2poly (coast, bbox = bb)
#' # The following map then colours in just the ocean:
#' map <- osm_basemap (bbox = bb) %>%
#'     add_osm_objects (coast$sea, col = "lightsteelblue") %>%
#'     print_osm_map ()
#' }
osm_line2poly <- function (obj, bbox)
{
    if (!is (obj$geometry, "sfc_LINESTRING"))
        stop ("obj must be class 'sf' with fields of class 'sfc_LINESTRING'")

    if (nrow(obj) == 0)
        stop("obj is empty - check osm query results")
    g <- obj$geom

    if (is.vector (bbox))
        bbox <- matrix (bbox, nrow = 2)
    # These geometries can contain several coastline "ways" that need to be
    # linked together. There may be multiple sets of these (think) peninsulas
    # being crossed by bounding box.  There can also be ways that link to form
    # polygons, and they need to be filtered separately.

    # Note that really strange things can happen, depending on the scale.  It is
    # possible for parts of a coastline to be connected outside the bounding
    # box, which means we need to be extra careful when clipping.

    # Look for the ones that aren't loops first, then deal with the loops
    # We identify link points using the rownames from osm as the key.

    # Get the first and last rowname from each line segment
    head_tail <- t (sapply (g, function (x) rownames (x) [c (1, nrow (x))]))

    m2 <- match (head_tail [, 2], head_tail [, 1])
    m1 <- match (head_tail [, 1], head_tail [, 2])

    # NA in m2 indicates the end of a chain.
    # NA in m1 indicates the start of a chain
    startidx <- which (is.na (m1))
    if (nrow (head_tail) > 1 & length (startidx) >= 1)
    {
        # Need to test this with disconnected bits
        linkorders <- lapply (startidx, function (x) unroll (x), V = m2)
        linkorders <- lapply (linkorders, function (x) x [!is.na (x)])
        links <- lapply (linkorders, function (x) head_tail [x, , drop = FALSE]) #nolint
        head_tail <- head_tail [-unlist (linkorders), , drop = FALSE] #nolint
        links <- lapply (links, function (x) lookup_ways (x), g = g)
    } else
        links <- list ()

    # Now we deal with loops.  Keep extracting loops until nothing left
    to_become_polygons <- list()
    lidx <- 1
    while (nrow (head_tail) > 0)
    {
        m2 <- match(head_tail [, 2], head_tail [, 1])
        if (any (!is.na (m2)))
        {
            l1 <- unroll_loop (1, m2)
            to_become_polygons [[lidx]] <- head_tail [l1, ]
            lidx <- lidx + 1
            head_tail <- head_tail [-l1, ]
        } else
            head_tail <- head_tail [-1, ]
    }
    to_become_polygons <- lapply (to_become_polygons,
                                  lookup_ways, g = g)
    to_become_polygons <- lapply (to_become_polygons,
                                  make_sf, g = g)
    to_become_polygons <- do.call (rbind, to_become_polygons)
    # Don't need to clip the polygons against the bounding box - they'll already
    # be inside, otherwise they wouldn't have been registered as polygons.  Even
    # if they aren't inside, we should be able to fill them

    #  if (length (g) > 0)
    #    message ("Not all line segments align continuously.")

    # polygon formed by expanding bbox through combination of increasing or
    # decreasing longitudes or latitudes. Requires explicit combinations for
    # each case. Note that bb is arranged thus:
    #   bb [1, 1]           bb [1, 2]
    #       |                   |
    #       O-------------------O  bb [2, 2]
    #       |                   |
    #       |                   |
    #       O-------------------O  bb [2, 1]
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
    # And N can be outside the same edge as 1!
    #
    # ... explicit cases for each of the other 3 starting points (+lon, +/- lat)
    # then follow by extension.

    bbxcorners_rh <- c("NE", "SE", "SW", "NW")
    bbxcoords <- rbind(c(bbox[1, 2], bbox[2, 2]),
                       c(bbox[1, 2], bbox[2, 1]),
                       c(bbox[1, 1], bbox[2, 1]),
                       c(bbox[1, 1], bbox[2, 2])
                       )
    rownames(bbxcoords) <- bbxcorners_rh

    p1 <- p2 <- NULL
    if (length(links) >= 1)
    {
        links <- lapply (links, function (x) clip_one (x), bbox = bbox)
        linkpoly <- lapply (links, make_poly,
                            bbox = bbox, g = g)
        p1 <- lapply (linkpoly, "[[", "p1")
        p2 <- lapply (linkpoly, "[[", "p2")

    } else
    {
        warning("No open curves found - check for polygons")
    }

    res <- NULL
    if (!is.null (p1) & !is.null (p2))
    {
        res <- list (sea = do.call(rbind, p1), land = do.call(rbind, p2))
    }
    if (length(to_become_polygons) >= 1)
    {
        res$islands <- to_become_polygons
    }
    return (res)
}

# Clip one line by reducing it to only that portion within the bb, plus one
# point either side
clip_one <- function (out, bbox)
{
    indx <-  (out [, 1] >= bbox [1, 1] & out [, 1] <= bbox [1, 2] &
              out [, 2] >= bbox [2, 1] & out [, 2] <= bbox [2, 2])
    # Need to deal with curves that join outside the bbox.  Need to dilate the
    # indx vector by 1 (max with left and right shifted versions of itself)
    indx <- as.logical (pmax (indx, c(indx [-1], FALSE),
                              c (FALSE, indx [-length (indx)])))
    out [indx, ]
}


lookup_ways <- function (L, g)
{
    gg <- g [rownames (L)]
    gg <- do.call (rbind, lapply (gg, as.matrix))
    rr <- duplicated (rownames (gg))
    gg <- gg [!rr, ]
    return (gg)
}

# For reordering the ways
unroll <- function(firstpos, V)
{
  res <- firstpos
  a <- V [firstpos]
  while (!is.na (a))
  {
    res <- c(res, a)
    a <- V [a]
  }
  return(res)
}

unroll_loop <- function(firstpos, V)
{
  ## iterative index following, for loops
  res <- firstpos
  a <- V [firstpos]
  visted <- rep (FALSE, length(V))
  visted [firstpos] <- TRUE
  while (!visted [a])
  {
    res <- c(res, a)
    visted [a] <- TRUE
    a <- V [a]
  }
  return(res)
}


# return whether a point is N,S,E,W of bounding box - i.e. which edge Could be a
# pathological corner case, which we'll ignore for now.
classify_pt_dir <- function(pt, bbox)
{
    directions <- 1:4
    names(directions) <- c("N", "E", "S", "W")
    compass <- c("W", "S", "E", "N")
    dim(compass) <- c(2, 2)

    lt <- pt < bbox [, "min"]
    gt <- pt > bbox [, "max"]

    td <- cbind (lt, gt)
    return (directions [compass [td]])
}

wrp <- function(idxs)
{
    (idxs - 1) %% 4 + 1
}

make_poly <- function (out, bbox, g)
{
    p1 <- p2 <- NULL
    n <- nrow (out)

    first_pt <- out [1, ]
    last_pt <- out [n, ]

    # Figure out which edges of the BB we cross (N,S,E,W)
    first_pt_dir <- classify_pt_dir (first_pt, bbox)
    last_pt_dir <- classify_pt_dir (last_pt, bbox)
    # We need these to generate a list of corners going clockwise and
    # anticlockwise.  Remember that the corner i is clockwise from edge i.

    # Modify the bbox by extending the corners. Just line up the extremes, much
    # as done for clipping

    bb <- bbox # makes following code slightly easier to read

    bb ["x", "min"] <- min (c (bb ["x", "min"], out [, 1]))
    bb ["x", "max"] <- max (c (bb ["x", "max"], out [, 1]))
    bb ["y", "min"] <- min (c (bb ["y", "min"], out [, 2]))
    bb ["y", "max"] <- max (c (bb ["y", "max"], out [, 2]))

    bb21 <- bb [2, 1]
    bb12 <- bb [1, 2]
    bb22 <- bb [2, 2]
    bb11 <- bb [1, 1]

    ext_corners <- rbind (c (bb12, bb22), c (bb12, bb21),
                          c (bb11, bb21), c (bb11, bb22))

    # create lists of corners in each direction
    if (last_pt_dir == first_pt_dir)
    {
        # Special rules if loop from one edge Need to check whether Lst is
        # clockwise from Fst or not, as the intersection edge doesn't tell us.

        v_first_last <- last_pt - first_pt
        v_edge <- ext_corners [first_pt_dir, ] -
            ext_corners [wrp (first_pt_dir - 1), ]
        dp <- sign (sum (v_first_last * v_edge))
        if (dp < 0)
        {
            ## Anticlockwise coast (relative to bb corners)
            cw_indx <- c (wrp (last_pt_dir - 1), last_pt_dir )
            ccw_indx <- (last_pt_dir - 1):(last_pt_dir - 4)
            ccw_indx <- wrp (ccw_indx)
            ccw_indx <- ccw_indx [1:which.max (ccw_indx == first_pt_dir)]
        } else
        {
            cw_indx <- last_pt_dir:(last_pt_dir + 4)
            cw_indx <- wrp (cw_indx)
            cw_indx <- cw_indx [1:which.max (cw_indx == wrp (first_pt_dir - 1))]
            ccw_indx <- c (last_pt_dir, wrp (last_pt_dir - 1))
        }
    } else
    {
        cw_indx <- last_pt_dir:(last_pt_dir + 4)
        cw_indx <- wrp (cw_indx)
        cw_indx <- cw_indx [1:which.max (cw_indx == first_pt_dir)]

        ccw_indx <- (last_pt_dir - 1):(last_pt_dir - 4)
        ccw_indx <- wrp (ccw_indx)
        ccw_indx <- ccw_indx [1:which.max (ccw_indx == first_pt_dir)]
    }

    p1 <- rbind (out, ext_corners [cw_indx, ], out [1, ])
    p2 <- rbind (out, ext_corners [ccw_indx, ], out [1, ])

    return (list (p1 = make_sf (p1, g), p2 = make_sf (p2, g)))
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
