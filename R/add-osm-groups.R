#' add_osm_groups
#'
#' Plots spatially distinct groups of osm objects in different colours. 
#'
#' @param obj An sp SPDF or SLDF (list of polygons or lines) returned by
#' extract_osm_objects()
#' @param groups A list of spatial points objects, each of which contains the
#' coordinates of points defining one group
#' @param make_hull Either a single boolean value or a vector of same length as
#' groups specifying whether a convex hull should be constructed around the
#' group (TRUE), or whether they group already defines a hull (convex or
#' otherwise; FALSE).
#' @param boundary (negative, 0, positive) values define whether the boundary of
#' groups should (exlude, bisect, include) objects which straddle the precise
#' boundary. (Has no effect if 'col_extra' is NULL.)
#' @param cols Either a vector of >= 4 colours passed to colour_mat() (if
#' 'colmat=T') to arrange as a 2-D map of visually distinct colours (default
#' uses rainbow colours), or (if 'colmat=F'), a vector of the same length as
#' groups specifying individual colours for each.
#' @param col_extra If NULL, then any polygons *NOT* within the convex hulls are
#' assigned to nearest group and coloured accordingly (and boundary has no
#' effect); if NOT NULL, then any polygons not within groups are coloured this
#' colour.
#' @param colmat If TRUE generates colours according to colour_mat(), otherwise
#' the colours of groups are specified directly by the vector of cols.
#' @param rotate Passed to colour_mat() to rotate colours by the specified
#' number of degrees clockwise.
#' @param lwd Width of boundary line (0 for no line)
#' @return nothing (adds to graphics.device opened with plot_osm_basemap())
#' @export
#'
#' @section Note:
#' Any group that is entire contained within any other group is assumed to
#' represent a hole, such that points internal to the smaller contained group
#' are *excluded* from the group, while those outside the smaller yet inside the
#' bigger group are included.
#'
#' @section Warning:
#' Bisecting objects along group boundaries ('boundary=0') can take
#' considerably longer than simple allocation of objects either side of
#' boundary.

add_osm_groups <- function (obj=obj, groups=NULL, make_hull=FALSE,
                               boundary=-1, cols=NULL, col_extra=NULL,
                               colmat=TRUE, rotate=NULL, lwd=0)
{
    if (is.null (dev.list ()))
        stop ('add_osm_groups can only be called after plot_osm_basemap')

    if (is.na (col_extra))
        col_extra <- NULL
    if (is.null (groups))
    {
        warning (paste0 ('No groups defined in add_osm_groups; ',
                         'passing to add_osm_objects'))
        if (is.null (cols))
            cols <- col_extra
        add_osm_objects (obj, col=cols [1])
        return ()
    } else if (class (groups) != 'list')
    {
        stopifnot (class (groups) == 'SpatialPoints')
        groups <- list (groups)
    } else if (!all ((lapply (groups, class)) == 'SpatialPoints'))
    {
        e <- simpleError ('Cannot coerce groups to SpatialPoints')
        tryCatch (
                  groups <- lapply (groups, function (x) 
                                    as (x, 'SpatialPoints')),
                  finally = stop (e))
    }

    stopifnot (length (make_hull) == 1 | length (make_hull) == length (groups))

    if (length (groups) == 1 & is.null (col_extra))
        col_extra <- 'gray40'

    if (class (obj) == 'SpatialPolygonsDataFrame')
    {
        objtxt <- c ('polygons', 'Polygons')
        plotfun <- function (i, col=col) polypath (i, border=NA, col=col)
    } else if (class (obj) == 'SpatialLinesDataFrame')
    {
        objtxt <- c ('lines', 'Lines')
        plotfun <- function (i, col=col) lines (i, col=col)
    } else
        stop ('obj must be SpatialPolygonsDataFrame or SpatialLinesDataFrame')

    # Determine whether any groups are holes
    if (length (groups) > 1)
    {
        holes <- rep (FALSE, length (groups))
        group_pairs <- combn (length (groups), 2)
        for (i in seq (ncol (group_pairs)))
        {
            x1 <- coordinates (groups [[group_pairs [1, i] ]]) [,1]
            y1 <- coordinates (groups [[group_pairs [1, i] ]]) [,2]
            indx <- which (!duplicated (cbind (x1, y1)))
            x1 <- x1 [indx]
            y1 <- y1 [indx]
            xy1 <- spatstat::ppp (x1, y1, xrange=range (x1), yrange=range (y1))
            ch1 <- spatstat::convexhull (xy1)
            bdry1 <- cbind (ch1$bdry[[1]]$x, ch1$bdry[[1]]$y)
            x2 <- coordinates (groups [[group_pairs [2, i] ]]) [,1]
            y2 <- coordinates (groups [[group_pairs [2, i] ]]) [,2]
            indx <- which (!duplicated (cbind (x2, y2)))
            x2 <- x2 [indx]
            y2 <- y2 [indx]
            xy2 <- spatstat::ppp (x2, y2, xrange=range (x2), yrange=range (y2))
            ch2 <- spatstat::convexhull (xy2)
            bdry2 <- cbind (ch2$bdry[[1]]$x, ch2$bdry[[1]]$y)
            
            #indx <- sapply (bdry1, function (x) 
            #                spatialkernel::pinpoly (bdry1, bdry2))
            indx <- sapply (bdry1, function (x) 
                            sp::point.in.polygon (bdry2 [,1], bdry2 [,2],
                                                  bdry1 [,1], bdry1 [,2]))
            if (all (indx == 1))
                holes [group_pairs [1, i]] <- TRUE
            #indx <- sapply (bdry2, function (x) 
            #                spatialkernel::pinpoly (bdry2, bdry1))
            indx <- sapply (bdry2, function (x) 
                            sp::point.in.polygon (bdry1 [,1], bdry1 [,2],
                                                  bdry2 [,1], bdry2 [,2]))
            if (all (indx == 1))
                holes [group_pairs [2, i]] <- TRUE
        }
    }

    # Set up group colours
    if (!colmat)
    {
        if (is.null (cols))
            cols <- rainbow (length (groups))
        else if (length (cols) < length (groups))
            cols <- rep (cols, length.out=length (groups))
        if (length (groups) == 1 & is.null (col_extra))
        {
            warning ('There is only one group; using default col_extra')
            if (is.null (cols))
            {
                cols <- 'red'
                col_extra <- 'gray40'
            } else if (cols [1] != 'gray40')
                col_extra <- 'gray40'
            else
                col_extra <- 'white'
        }
    } else
    {
        if (is.null (cols) | length (cols) < 4)
            cols <- rainbow (4)
        ncols <- 20
        cmat <- colour_mat (ncols, cols=cols, rotate=rotate)
        cols <- rep (NA, length (groups)) 
        # cols is then a vector of colours to be filled by matching group
        # centroids to relative positions within cmat
    }

    # first extract mean coordinates for every polygon or line in obj:
    xy_mn <- lapply (slot (obj, objtxt [1]),  function (x)
                     colMeans  (slot (slot (x, objtxt [2]) [[1]], 'coords')))
    xmn <- sapply (xy_mn, function (x) x [1])
    ymn <- sapply (xy_mn, function (x) x [2])

    usr <- par ('usr')
    boundaries <- list ()
    xy_list <- list () 
    # The following loop constructs:
    # 1.  xy_list list for centroids of each object in each group; used to
    # reallocate stray objects if is.null (col_extra)
    # 2. boundaries list of enclosing polygons, creating convex hulls if
    # necessary.
    for (i in seq (groups))
    {
        if ((length (make_hull) == 1 & make_hull) |
            (length (make_hull) > 1 & make_hull [i]))
        {
            x <- slot (groups [[i]], 'coords') [,1]
            y <- slot (groups [[i]], 'coords') [,2]
            xy <- spatstat::ppp (x, y, xrange=range (x), yrange=range (y))
            ch <- spatstat::convexhull (xy)
            bdry <- cbind (ch$bdry[[1]]$x, ch$bdry[[1]]$y)
        }
        else
            bdry <- sp::coordinates (groups [[i]])
        bdry <- rbind (bdry, bdry [1,]) #enclose bdry back to 1st point
        # The next 4 lines are only used if is.null (col_extra)
        #indx <- sapply (xy_mn, function (x) spatialkernel::pinpoly (bdry, x))
        indx <- sapply (xy_mn, function (x)
                        sp::point.in.polygon (x [1], x [2], 
                                              bdry [,1], bdry [,2]))
        indx <- which (indx > 0) # see below for point.in.polygon values
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
    # while point.in.polygon returns (0,1,2-3) for (not, in, on)
    # pinpoly (poly, pts), but point.in.polygon (pt.x, pt.y, pol.x, pol.y).
    # pinpoly had to be ditched because spatialkernel caused an error:
    # 'package ... eventually depends on the the following package which
    # restricts usage'
    coords <- lapply (slot (obj, objtxt [1]),  function (x)
                      slot (slot (x, objtxt [2]) [[1]], 'coords'))
    coords <- lapply (coords, function (i)
                      {
                          #pins <- lapply (boundaries, function (j)
                          #                spatialkernel::pinpoly (j, i))
                          pins <- lapply (boundaries, function (j)
                                          sp::point.in.polygon (i [,1], i [,2],
                                                                j [,1], j [,2]))
                          pins <- do.call (cbind, pins)
                          cbind (i, pins)
                      })
    if (is.null (col_extra)) 
    {
        # Then each component is assigned to a single group based on entire
        # boundary.  NOTE that in cases where membership is *equally*
        # distributed between 2 groups, one of these is *randomly* selected.
        membs <- sapply (coords, function (i)
                         {
                             temp <- i [,3:ncol (i)]
                             if (!is.matrix (temp))
                                 temp <- matrix (temp, ncol=1, 
                                                 nrow=length (temp))
                             temp [temp > 1] <- 1
                             n <- colSums (temp)
                             if (max (n) < 3) # must have > 2 elements in group
                                 n <- 0
                             else
                             {
                                 indx <- which (n == max (n))
                                 n <- indx [ceiling (runif (1) * length (indx))]
                             }
                             return (n)
                         })
        indx <- which (membs == 0)
        x0 <- xmn [indx]
        y0 <- ymn [indx]
        dists <- array (NA, dim=c (length (indx), length (groups)))
        for (i in seq (groups))
        {
            ng <- dim (xy_list [[i]]) [1]
            if (ng > 0)
            {
                x0mat <- array (x0, dim=c(length (x0), ng))
                y0mat <- array (y0, dim=c(length (y0), ng))
                xmat <- t (array (xy_list [[i]] [,1], dim=c(ng, length (x0))))
                ymat <- t (array (xy_list [[i]] [,2], dim=c(ng, length (x0))))
                dg <- sqrt ((xmat - x0mat) ^ 2 + (ymat - y0mat) ^ 2)
                # Then the minimum distance for each stray object to any object in
                # group [i]:
                dists [, i] <- apply (dg, 1, min)
            } else
                dists [, i] <- Inf
        }
        # Then simply extract the group holding the overall minimum dist:
        membs [indx] <- apply (dists, 1, which.min)
        xy <- lapply (coords, function (i) i [,1:2])
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
                                 if (!is.matrix (temp))
                                     temp <- matrix (temp, ncol=1, 
                                                     nrow=length (temp))
                                 temp [temp > 1] <- 1
                                 n <- colSums (temp)
                                 if (boundary < 0)
                                 {
                                     if (max (n) < nrow (temp))
                                         n <- 0
                                     else
                                         n <- which.max (n)
                                 } else if (boundary > 0 & max (n) > 0)
                                     n <- which.max (n)
                                 else
                                     n <- 0
                                 return (n)
                             })
        } else
        { 
            # potentially split objects across boundaries, thereby extending coords
            # and thus requiring an explicit loop. TODO: Rcpp this?
            split_objs <- sapply (coords, function (i)
                                  {
                                      temp <- i [,3:ncol (i)]
                                      if (!is.matrix (temp))
                                          temp <- matrix (temp, ncol=1, 
                                                          nrow=length (temp))
                                      temp [temp > 1] <- 1
                                      n <- colSums (temp)
                                      if (max (n) > 0 & max (n) < nrow (temp))
                                          return (which.max (n))
                                      else
                                          return (0)
                                  })
            split_objs <- which (split_objs > 0)
            # Then split coords into 2 lists, one for non-split objects and one
            # containing those listed in split_objs
            coords_split <- lapply (split_objs, function (i) coords [[i]])
            indx <- seq (coords) [!seq (coords) %in% split_objs]
            coords <- lapply (indx, function (i) coords [[i]])
            # Then make new lists of xy and memberships by spliiting objects in
            # coords_split. These lists are of unknown length, requiring an
            # unsightly double loop.
            xy <- list () 
            membs <- NULL
            for (i in coords_split)
            {
                temp <- i [,3:ncol (i)]
                temp [temp > 1] <- 1
                if (!is.matrix (temp))
                    temp <- matrix (temp, ncol=1, nrow=length (temp))
                n <- colSums (temp)
                if (max (n) < 3)
                {
                    xy [[length (xy) + 1]] <- i [,1:2]
                    membs <- c (membs, 0)
                } else 
                {
                    # Allow for multiple group memberships
                    indx_i <- which (n > 2)
                    for (j in indx_i)
                    {
                        indx_j <- which (temp [,j] == 1)
                        if (length (indx_j) > 2)
                        {
                            xy [[length (xy) + 1]] <- i [indx_j, 1:2]
                            membs <- c (membs, j)
                        }
                        indx_j <- which (temp [,j] == 0)
                        if (length (indx_j) > 2)
                        {
                            xy [[length (xy) + 1]] <- i [indx_j, 1:2]
                            membs <- c (membs, 0)
                        }
                    } # end for j
                } # end else !(max (n) < 3)
            } # end for i
            # Then add the non-split groups
            xy <- c (xy, lapply (coords, function (i) i [,1:2]))
            membs2 <- sapply (coords, function (i)
                              {
                                  temp <- i [,3:ncol (i)]
                                  if (!is.matrix (temp))
                                      temp <- matrix (temp, ncol=1, 
                                                      nrow=length (temp))
                                  temp [temp > 1] <- 1
                                  n <- colSums (temp)
                                  if (max (n) < nrow (temp))
                                      n <- 0
                                  else
                                      n <- which.max (n)
                                  return (n)
                              })
            membs <- c (membs, membs2)
        } # end else split objects across boundaries
        # Re-map membs == 0:
        membs [membs == 0] <- length (groups) + 1
    } # end else col_extra

    # cbind membs to xy and submit to plot, so that membs maps straight onto
    # colours
    xym <- mapply (cbind, xy, membs)
    if (!is.null (col_extra))
        cols <- c (cols, col_extra)
    junk <- lapply (xym, function (x)
                    do.call (plotfun, list ( x [,1:2], col=cols [x [1,3]])))
    if (lwd > 0)
        for (i in seq (groups))
        {
            indx <- which (membs == i)
            if (length (indx) > 1)
            {
                x <- unlist (lapply (indx, function (j) xy [[j]] [,1]))
                y <- unlist (lapply (indx, function (j) xy [[j]] [,2]))
                xy2 <- unique (cbind (x, y))
                x <- xy2 [,1]
                y <- xy2 [,2]
                xy2 <- spatstat::ppp (x, y, xrange=range (x), yrange=range (y))
                ch <- spatstat::convexhull (xy2)
                bdry <- cbind (ch$bdry[[1]]$x, ch$bdry[[1]]$y)
                lines (ch$bdry [[1]]$x, ch$bdry [[1]]$y, lwd=lwd, col=cols [i])
            }
        }
}

