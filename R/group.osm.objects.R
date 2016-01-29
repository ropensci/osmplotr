#' group.osm.objects
#'
#' Plots spatially distinct groups of osm objects in different colours. OSM
#' objects are attributed to groups based on mean coordinates, so this routine
#' work best for SpatialPolygons, while may give odd results for SpatialLines.
#'
#' @param obj = an sp SPDF or SLDF (list of polygons or lines) returned by
#' get.osm.polygons 
#' @param groups = a list of spatial points objects, each of which contains the
#' coordinates of points defining one group
#' @param cols = 1. Either a vector of >= 4 colours passed to colour.mat (is
#' colmat=T) to arrange as a 2-D map of visually distinct colours (NULL default
#' uses rainbow colours), or 2. If !colmat, a vector of the same length as
#' groups specifying individual colours for each.
#' @param col.extra if NULL, then any polygons *NOT* within the convex hulls are
#' assigned to nearest group and coloured accordingly; if NOT NULL, then any
#' polygons not within groups are coloured this colour.
#' @return nothing (adds to graphics.device opened with plot.osm.basemap)

group.osm.objects <- function (obj=obj, groups=NULL, cols=NULL,
                               col.extra=NULL, colmat=TRUE)
{
    plot.poly <- function (i, col=col) 
    {
        xy <- slot (slot (i, "Polygons") [[1]], "coords")
        polypath (xy, border=NA, col=col)
    }
    plot.line <- function (i, col=col) 
    {
        xy <- slot (slot (i, "Lines") [[1]], "coords")
        lines (xy, col=col)
    }

    if (class (obj) == "SpatialPolygonsDataFrame")
    {
        objtxt <- c ("polygons", "Polygons")
        plotfun <- "plot.poly"
    } else if (class (obj) == "SpatialLinesDataFrame")
    {
        objtxt <- c ("lines", "Lines")
        plotfun <- "plot.line"
    } else
        stop ("obj must be SpatialPolygonsDataFrame or SpatialLinesDataFrame")

    if (is.null (dev.list ()))
        stop ("group.osm.objects can only be called after plot.osm.basemap")

    stopifnot (all ((lapply (groups, class)) == "SpatialPoints"))

    # first extract mean coordinates for every polygon or line in obj:
    xy.mn <- lapply (slot (obj, objtxt [1]),  function (x)
                  colMeans  (slot (slot (x, objtxt [2]) [[1]], "coords")))
    xmn <- sapply (xy.mn, function (x) x [1])
    ymn <- sapply (xy.mn, function (x) x [2])

    if (colmat)
    {
        ncols <- 20
        cmat <- colour.mat (ncols, cols=cols)
        cols <- rep (NA, length (groups)) 
    }
    # cols is a vector of colours to be filled by matching group centroids to
    # relative positions within cmat
    group.indx <- rep (NA, length (obj))
    xy.list <- list () 
    # xy list for centroids of each object in each group; used to reallocate
    # stray objects if is.null (col.extra)

    usr <- par ("usr")

    for (i in seq (groups))
    {
        x <- slot (groups [[i]], "coords") [,1]
        y <- slot (groups [[i]], "coords") [,2]
        xy <- spatstat::ppp (x, y, xrange=range (x), yrange=range (y))
        ch <- spatstat::convexhull (xy)
        bdry <- cbind (ch$bdry[[1]]$x, ch$bdry[[1]]$y)
        bdry <- rbind (bdry, bdry [1,]) #enclose bdry back to 1st point
        indx <- sapply (xy.mn, function (x) spatialkernel::pinpoly (bdry, x))
        indx <- which (indx == 2) # pinpoly returns 2 for points within hull
        group.indx [indx] <- i
        xy.list [[i]] <- cbind (xmn [indx], ymn [indx])

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

    if (is.null (col.extra)) 
    {
        # then assign remaining sp objects to nearest groups based on Euclidean
        # distances.
        indx <- which (is.na (group.indx))
        x0 <- xmn [indx]
        y0 <- ymn [indx]
        dists <- array (NA, dim=c (length (indx), length (groups)))
        for (i in seq (groups))
        {
            ng <- dim (xy.list [[i]]) [1]
            x0mat <- array (x0, dim=c(length (x0), ng))
            y0mat <- array (y0, dim=c(length (y0), ng))
            xmat <- t (array (xy.list [[i]] [,1], dim=c(ng, length (x0))))
            ymat <- t (array (xy.list [[i]] [,2], dim=c(ng, length (x0))))
            dg <- sqrt ((xmat - x0mat) ^ 2 + (ymat - y0mat) ^ 2)
            # Then the minimum distance for each stray object to any object in
            # group [i]:
            dists [, i] <- apply (dg, 1, min)
        }
        # Then simply extract the group holding the overall minimum dist:
        min.group <- apply (dists, 1, which.min)
        group.indx [indx] <- min.group
    } else { # plot stray objects with col.extra
        indx <- which (is.na (group.indx))
        junk <- lapply (slot (obj [indx,], objtxt [1]), function (x)
                        do.call (plotfun, c (x, col.extra)))
    }

    for (i in 1:length (groups))
    {
        indx <- which (group.indx == i)
        junk <- lapply (slot (obj [indx,], objtxt [1]), function (x)
                        do.call (plotfun, c (x, cols [i])))
    }
}

