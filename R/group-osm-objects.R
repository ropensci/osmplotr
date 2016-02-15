#' group_osm_objects
#'
#' Plots spatially distinct groups of osm objects in different colours. OSM
#' objects are attributed to groups based on mean coordinates, so this routine
#' work best for SpatialPolygons, while may give odd results for SpatialLines.
#'
#' @param obj An sp SPDF or SLDF (list of polygons or lines) returned by
#' \code{get.osm.polygons}
#' @param groups A list of spatial points objects, each of which contains the
#' coordinates of points defining one group
#' @param make_hull Either a single boolean value or a vector of same length as
#' groups specifying whether a convex hull should be constructed around the
#' group (TRUE), or whether they group already defines a hull (convex or
#' otherwise; FALSE).
#' @param boundary (negative, 0, positive) values define whether the boundary of
#' groups should (exlude, bisect, include) objects which straddle the precise
#' boundary. (Has no effect if col_extra is NULL.)
#' @param cols Either a vector of >= 4 colours passed to colour_mat (is
#' colmat=T) to arrange as a 2-D map of visually distinct colours (NULL default
#' uses rainbow colours), or 2. If !colmat, a vector of the same length as
#' groups specifying individual colours for each.
#' @param col_extra If NULL, then any polygons *NOT* within the convex hulls are
#' assigned to nearest group and coloured accordingly (and boundary has no
#' effect); if NOT NULL, then any
#' polygons not within groups are coloured this colour.
#' @param colmat If TRUE generates colours according to \code{get.colours},
#' otherwise the colours of groups are specified directly by the vector of cols.
#' @return nothing (adds to graphics.device opened with plot.osm.basemap)
#'
#' @section Warning:
#' Bisecting objects along group boundaries (\code{boundary=0}) can take
#' considerably longer than simple allocation of objects either side of
#' boundary.

group_osm_objects <- function (obj=obj, groups=NULL, make_hull=FALSE,
                               boundary=-1, cols=NULL, col_extra=NULL,
                               colmat=TRUE)
{
    if (is.null (dev.list ()))
        stop ("group.osm.objects can only be called after plot.osm.basemap")

    if (is.null (groups))
    {
        warning (paste0 ("No groups defined in group_osm_objects; ",
                         "passing to add_osm_objects"))
        add_osm_objects (obj, col=col_extra)
        return ()
    } else if (class (groups) != "list")
    {
        stopifnot (class (groups) == "SpatialPoints")
        groups <- list (groups)
    } else if (!all ((lapply (groups, class)) == "SpatialPoints"))
    {
        e <- simpleError ("Cannot coerce groups to SpatialPoints")
        tryCatch (
            groups <- lapply (groups, function (x) 
                              as (x, "SpatialPoints")),
            finally = stop (e))
    }
                
    stopifnot (length (make_hull) == 1 | length (make_hull) == length (groups))

    plot_poly <- function (i, col=col) 
        polypath (i, border=NA, col=col)
    plot_line <- function (i, col=col) 
        lines (i, col=col)

    if (class (obj) == "SpatialPolygonsDataFrame")
    {
        objtxt <- c ("polygons", "Polygons")
        plotfun <- "plot_poly"
    } else if (class (obj) == "SpatialLinesDataFrame")
    {
        objtxt <- c ("lines", "Lines")
        plotfun <- "plot_line"
    } else
        stop ("obj must be SpatialPolygonsDataFrame or SpatialLinesDataFrame")

    # Set up group colours
    if (length (cols) < 4)
    {
        warning ("There are < 4 colors; passing directly to group colours")
        if (is.null (cols))
            cols <- rainbow (length (groups))
        else if (length (cols) < length (groups))
            cols <- rep (cols, length.out=length (groups))
        colmat <- FALSE
        if (length (groups) == 1 & is.null (col_extra))
        {
            warning ("There is only one group; using default col_extra")
            if (is.null (cols))
            {
                cols <- "red"
                col_extra <- "gray40"
            } else if (cols [1] != "gray40")
                col_extra <- "gray40"
            else
                col_extra <- "white"
        }
    }
    if (colmat)
    {
        ncols <- 20
        cmat <- colour_mat (ncols, cols=cols)
        cols <- rep (NA, length (groups)) 
        # cols is then a vector of colours to be filled by matching group
        # centroids to relative positions within cmat
    }

    # first extract mean coordinates for every polygon or line in obj:
    xy_mn <- lapply (slot (obj, objtxt [1]),  function (x)
                  colMeans  (slot (slot (x, objtxt [2]) [[1]], "coords")))
    xmn <- sapply (xy_mn, function (x) x [1])
    ymn <- sapply (xy_mn, function (x) x [2])

    usr <- par ("usr")
    boundaries <- list ()
    group_indx <- rep (NA, length (obj))
    xy_list <- list () 
    # xy_list list for centroids of each object in each group; used to reallocate
    # stray objects if is.null (col_extra)
    for (i in seq (groups))
    {
        if ((length (make_hull) == 1 & make_hull) |
            (length (make_hull) > 1 & make_hull [i]))
            {
                x <- slot (groups [[i]], "coords") [,1]
                y <- slot (groups [[i]], "coords") [,2]
                xy <- spatstat::ppp (x, y, xrange=range (x), yrange=range (y))
                ch <- spatstat::convexhull (xy)
                bdry <- cbind (ch$bdry[[1]]$x, ch$bdry[[1]]$y)
            }
        else
            bdry <- coordinates (groups [[i]])
        bdry <- rbind (bdry, bdry [1,]) #enclose bdry back to 1st point
        indx <- sapply (xy_mn, function (x) spatialkernel::pinpoly (bdry, x))
        indx <- which (indx == 2) # pinpoly returns 2 for points within hull
        group_indx [indx] <- i
        xy_list [[i]] <- cbind (xmn [indx], ymn [indx])

        boundaries [[i]] <- bdry

        if (colmat)
        {
            # Then get colour from colour.mat
            xi <- ceiling (ncols * (mean (xmn [indx]) - usr [1]) / 
                           (usr [2] - usr [1]))
            yi <- ceiling (ncols * (mean (ymn [indx]) - usr [3]) / 
                           (usr [4] - usr [3]))
            cols [i] <- cmat [xi, yi]
        }
    }

    # Extract coordinates of each item and cbind memberships for each group.
    # NOTE that pinpooly returns (0,1,2) for (not, on, in) boundary
    coords <- lapply (slot (obj, objtxt [1]),  function (x)
                      slot (slot (x, objtxt [2]) [[1]], "coords"))
    coords <- lapply (coords, function (i)
                    {
                        pins <- lapply (boundaries, function (j)
                                        spatialkernel::pinpoly (j, i))
                        pins <- do.call (cbind, pins)
                        cbind (i, pins)
                    })
    if (is.null (col_extra)) 
    {
        # Then each component is assigned to a single group based on entire
        # boundary.  NOTE that in cases where membership is *equally*
        # distributed between 2 groups, the which.max function will always
        # return the numerically first group. TODO: Improve this.
        membs <- sapply (coords, function (i)
                         {
                             temp <- i [,3:ncol (i)]
                             temp [temp == 2] <- 1
                             n <- colSums (temp)
                             if (max (n) < 3) # must have > 2 elements in group
                                 n <- 0
                             else
                                 n <- which.max (n)
                             return (n)
                         })
        indx <- which (membs == 0)
        x0 <- xmn [indx]
        y0 <- ymn [indx]
        dists <- array (NA, dim=c (length (indx), length (groups)))
        for (i in seq (groups))
        {
            ng <- dim (xy_list [[i]]) [1]
            x0mat <- array (x0, dim=c(length (x0), ng))
            y0mat <- array (y0, dim=c(length (y0), ng))
            xmat <- t (array (xy_list [[i]] [,1], dim=c(ng, length (x0))))
            ymat <- t (array (xy_list [[i]] [,2], dim=c(ng, length (x0))))
            dg <- sqrt ((xmat - x0mat) ^ 2 + (ymat - y0mat) ^ 2)
            # Then the minimum distance for each stray object to any object in
            # group [i]:
            dists [, i] <- apply (dg, 1, min)
        }
        # Then simply extract the group holding the overall minimum dist:
        membs [indx] <- apply (dists, 1, which.min)
        xy <- lapply (coords, function (i) i [,1:2])
        # And re-map membs == 0:
        membs [membs == 0] <- length (groups) + 1
    } else
    {
        # Allocate objects within boundaries to groups, and all remaining
        # objects to group#0
        if (boundary != 0)
        {
            xy <- lapply (coords, function (i) i [,1:2])
            membs <- lapply (coords, function (i)
                             {
                                 temp <- i [,3:ncol (i)]
                                 temp [temp == 2] <- 1
                                 n <- colSums (temp)
                                 if (boundary < 0 & max (n) < nrow (temp))
                                     n <- 0
                                 else if (boundary > 0 & max (n) > 0)
                                     n <- which.max (n)
                                 else
                                     n <- 0
                                 return (n)
                             })
        } else
        { 
            # potentially split objects across boundaries, thereby extending coords
            # and thus requiring an explicit loop. TODO: Rcpp this?
            xy <- list () # new coords, including group membership
            membs <- NULL
            for (i in coords)
            {
                temp <- i [,3:ncol (i)]
                temp [temp == 2] <- 1
                n <- colSums (temp)
                if (max (n) < 3)
                {
                    xy [[length (xy) + 1]] <- i [,1:2]
                    membs <- c (membs, 0)
                } else 
                {
                    indx <- which (n > 2)
                    for (j in indx)
                    {
                        indx <- which (temp [,j] == 1)
                        if (length (indx) > 2)
                        {
                            xy [[length (xy) + 1]] <- i [indx, 1:2]
                            membs <- c (membs, j)
                        }
                    } # end for j
                } # end else !(max (n) < 3)
            } # end for i
        } # end else split objects across boundaries
        membs [membs == 0] <- length (groups) + 1
    } # end else col_extra

    # cbind membs to xy and submit to plot, so that membs maps straight onto
    # colours
    xym <- mapply (cbind, xy, membs)
    cols <- c (cols, col_extra)
    junk <- lapply (xym, function (x)
                    do.call (plotfun, list ( x [,1:2], col=cols [x [1,3]])))
}

