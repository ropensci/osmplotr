#' group.osm.objects
#'
#' Plots spatially distinct groups of osm objects is different colours. OSM
#' objects are attributed to groups based on mean coordinates, so this routine
#' work best for SpatialPolygons, while may give odd results for SpatialLines.
#'
#' @param obj = an sp SPDF or SLDF (list of polygons or lines) returned by
#' get.osm.polygons 
#' @param groups = a list of spatial points objects, each of which contains the
#' coordinates of points defining one group
#' @param cols = Vector of >= 4 colours passed to colour.mat to arrange as a 2-D
#' map of visually distinct colours (default uses rainbow colours). 
#' @param col.extra if NULL, then any polygons *NOT* within the convex hulls are
#' assigned to nearest group and coloured accordingly; if NOT NULL, then any
#' polygons not within groups are coloured this colour.
#' @return nothing (adds to graphics.device opened with plot.osm.basemap)

group.osm.objects <- function (obj=obj, groups=NULL, cols=NULL,
                               col.extra=NULL)
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

    if (if.null (dev.list ()))
        stop ("group.osm.objects can only be called after plot.osm.basemap")

    stopifnot (all ((lapply (groups, class)) == "SpatialPoints"))

    ncols <- 20
    colmat <- colour.mat (ncols)
    cols <- rep (NA, length (groups)) 
    # cols is a vector of colours to be filled by matching group centroids to
    # relative positions within colmat
    group.indx <- rep (NA, length (obj))

    usr <- par ("usr")

    for (i in seq (groups))
    {
        x <- slot (groups [[i]], "coords") [,1]
        y <- slot (groups [[i]], "coords") [,2]
        xy <- ppp (x, y, xrange=range (x), yrange=range (y))
        ch <- convexhull (xy)
        bdry <- cbind (ch$bdry[[1]]$x, ch$bdry[[1]]$y)
        bdry <- rbind (bdry, bdry [1,]) #enclose bdry back to 1st point
        xy.mn <- lapply (slot (obj, objtxt [1]),  function (x)
                      colMeans  (slot (slot (x, objtxt [2]) [[1]], "coords")))
        indx <- sapply (xy.mn, function (x) pinpoly (bdry, x))
        indx <- which (indx == 2) # pinpoly returns 2 for points within hull
        group.indx [indx] <- i

        # Then get colour from colour.mat
        xmn <- mean (sapply (xy.mn [indx], function (x) x [1]))
        ymn <- mean (sapply (xy.mn [indx], function (x) x [2]))
        xi <- ceiling (ncols * (xmn - usr [1]) / (usr [2] - usr [1]))
        yi <- ceiling (ncols * (ymn - usr [3]) / (usr [4] - usr [3]))
        cols [i] <- colmat [xi, yi]
    }

    # TODO: Assign remaining sp objects to groups if col.extra==NULL

    for (i in 1:length (groups))
    {
        indx <- which (group.indx == i)
        junk <- lapply (slot (obj [indx,], objtxt [1]), function (x)
                        do.call (plotfun, c (x, cols [i])))
    }
}

