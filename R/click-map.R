#' click_map
#'
#' Translates clicks on a map into a convex hull object which can be passed to
#' group_osm_objects(). Clicking is stopped when same point on map is
#' clicked twice.
#'
#' @return A data frame containing coordinates of convex hull boundary.

click_map <- function ()
{
    if (is.null (dev.list ()))
        stop ('group.osm.objects can only be called after plot.osm.basemap')

    cat ('click on same location twice to finish\n')

    xy <- NULL
    loc_old <- 0
    loc <- 1
    while (!all (unlist (loc) == unlist (loc_old)))
    {
        loc_old <- loc
        loc <- locator (n = 1)
        xy <- rbind (xy, loc)
    }

    # Then get hull
    x <- as.numeric (xy [1:(nrow (xy) - 1),1])
    y <- as.numeric (xy [1:(nrow (xy) - 1),2])
    xy <- spatstat::ppp (x, y, xrange=range (x), yrange=range (y))
    ch <- spatstat::convexhull (xy)
    bdry <- cbind (ch$bdry[[1]]$x, ch$bdry[[1]]$y)
    SpatialPoints (bdry)
}
