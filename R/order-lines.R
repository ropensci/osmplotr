#' order_lines
#'
#' Accepts a single way as list of matrices representing an OpenStreetMap line
#' object such as a highway. The list items of these objects are arbitrarily
#' organised within OpenStreetMap. This function orders the components,
#' returning a list of components each of which is ordered sequentially along
#' the line. This list itself may contain several components where individual
#' highway components either branch or are discrete.
#'
#' @param sp_lines A \code{SpatialLinesDataFrame} returned from
#' \code{extract_osm_objects}.
#' @return A list of ordered line segments.
#'
#' @section Note:
#' This function is primarily used in \code{extract_highways}.
#'
#' @noRd
order_lines <- function (xy)
{
    xy_ord <- list (xy [[1]])
    xy [[1]] <- NULL
    while (length (xy) > 0)
    {
        ex <- extend_ord_list (xy, xy_ord)
        xy <- ex$xy
        xy_ord <- ex$xy_ord
    }

    return (xy_ord)
}

extend_ord_list <- function (xy, xy_ord)
{
    fn <- "head"
    ordi <- which_ord (xy_ord, xy, fn)
    if (ordi == 0)
    {
        fn <- "tail"
        ordi <- which_ord (xy_ord, xy, fn)
    }

    if (ordi > 0)
    {
        xy_ordi <- xy_ord [[ordi]]
        # xy_ordi is element of xy_ord that has either the head or tail of xy

        xyi <- which (which_xy (xy = xy, xy_ordi = xy_ordi, fn = fn)) [1]
        # [1] in case of mulitple matches

        xtmp <- xy [[xyi]]
        xy [[xyi]] <- NULL
        xy_ord [[ordi]] <- rbind_xy (xtmp, xy_ordi)
    } else # no join so add first element of xy to xy_ord_list
    {
        xy_ord [[length (xy_ord) + 1]] <- xy [[1]]
        xy [[1]] <- NULL
    }

    return (list ('xy' = xy, 'xy_ord' = xy_ord))
}

# which element of xy_ord contains head or tail element of any component of xy.
# xy_ord has muliple list items only when a single highway has distinct or
# branching components. This fn finds which component the element xy is to be
# added to.
#
# @return single int index into xy_ord
which_ord <- function (xy_ord, xy, fn = 'head')
{
    max (0, which (vapply (xy_ord, function (i)
                           max (0, which_xy (i, xy = xy, fn = fn)),
                           numeric (1)) > 0))
}

# which element of xy has head or tail of xy_ord [[i]]
#
# @return logical vector same length as xy
which_xy <- function (xy, xy_ordi, fn = 'head')
{
    vapply (xy, function (i)
            do.call (fn, list (rownames (xy_ordi), 1)) %in% rownames (i),
            logical (1))
}

#' rbind_xy
#'
#' rbind xy to xy_ordi, flipping both where necessary
#'
#' @noRd
rbind_xy <- function (xy, xy_ord)
{
    if (head (rownames (xy_ord), 1) %in% rownames (xy))
        xy_ord <- apply (xy_ord, 2, rev) # flip to rbind at bottom
    if (tail (rownames (xy), 1) %in% rownames (xy_ord))
        xy <- apply (xy, 2, rev) # flip to rbind at top

    # rownames don't carry if xy only has 2 rows - tibble it?
    xynm <- rownames (xy) [2:nrow (xy)]
    xy <- matrix (xy [2:nrow (xy), ], ncol = 2)
    rownames (xy) <- xynm
    rbind (xy_ord, xy)
}
