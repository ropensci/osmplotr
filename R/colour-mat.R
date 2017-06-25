#' colour_mat
#'
#' Generates a 2D matrix of graduated colours by interpolating between the given
#' colours specifying the four corners.
#'
#' @param cols vector of length >= 4 of colors (example, default = \code{rainbow
#' (4)}, or \code{RColorBrewer::brewer.pal (4, 'Set1')}).
#' \code{cols} are wrapped clockwise around the corners from top left to bottom
#' left. 
#' @param n number of rows and columns of colour matrix (default = 10; if length
#' 2, then dimensions of rectangle). 
#' @param rotate rotates the entire colour matrix by the specified angle (in
#' degrees).
#' @param plot plots the colour matrix.
#' @return \code{Matrix} of colours.
#' @export
#'
#' @seealso \code{\link{add_osm_groups}}.
#'
#' @examples
#' cm <- colour_mat (n = 5, cols = rainbow(4), rotate = 90, plot = TRUE)
#'
#' # 'colour_mat' is intended primarily for use in colouring groups added with
#' # 'add_osm_groups' using the 'colmat = TRUE' option:
#' bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
#' # Generate random points to serve as group centres
#' set.seed (2)
#' ngroups <- 6
#' x <- bbox [1,1] + runif (ngroups) * diff (bbox [1,])
#' y <- bbox [2,1] + runif (ngroups) * diff (bbox [2,])
#' groups <- cbind (x, y)
#' groups <- apply (groups, 1, function (i) 
#'                  sp::SpatialPoints (matrix (i, nrow = 1, ncol = 2)))
#' # plot a basemap and add groups
#' map <- osm_basemap (bbox = bbox, bg = "gray20")
#' map <- add_osm_groups (map, obj = london$dat_BNR, group = groups, cols = rainbow (4),
#'                        colmat = TRUE, rotate = 90)
#' print_osm_map (map)

colour_mat <- function (cols, n = c(10, 10), rotate, plot = FALSE)
{
    # ---------------  sanity checks and warnings  ---------------
    # ---------- cols
    if (missing (cols)) stop ('cols must be provided')
    if (is.null (cols)) return (NULL)
    else if (length (cols) < 4) stop ('cols must have length >= 4')
    if (any (is.na (cols))) stop ('One or more cols is NA')
    if (class (cols) != 'matrix')
    {
        cols <- sapply (cols, function (i) {
                        tryCatch (
                              col2rgb (i),
                              error = function (e)
                              {
                                  e$message <-  paste0 ('Invalid colours: ', i)
                             })
                    })
    } else if (rownames (cols) != c ('red', 'green', 'blue'))
        stop ('Colour matrix has unknown format')
    if (any (grep ('Invalid colours', cols)))
        stop (cols [grep ('Invalid colours', cols) [1]])

    indx <- floor (1:4 * ncol (cols) / 4)
    indx [1] <- 1
    cols <- cols [, indx]
    # ---------- n
    if (length (n) == 1)
        n <- rep (n, 2)
    if (!all (is.numeric (n))) stop ('n must be numeric')
    if (any (is.na (n))) stop ('n can not be NA')
    if (any (n < 2)) stop ('n must be > 1')
    # ---------- rotate
    if (!missing (rotate))
    {
        if (length (rotate) > 1)
        {
            warning ('rotate has length > 1; using only first element')
            rotate <- rotate [1]
        }
        if (!is.numeric (rotate)) stop ('rotate must be numeric')
        if (is.na (rotate)) stop ('rotate can not be NA')
    }
    # ---------------  end sanity checks and warnings  ---------------

    if (!missing (rotate))
        cols <- rotate_colourmat (cols, rotate)

    tl <- cols [, 1] # top left
    tr <- cols [, 2] # top right
    br <- cols [, 3] # bottom right
    bl <- cols [, 4] # bottom left
    # Then interpolate, starting with top and bottom rows
    ih <- t (array (seq (n [2]) - 1, dim = c (n [2], 3))) / (n [2] - 1)
    top <- (1 - ih) * tl + ih * tr
    bot <- (1 - ih) * bl + ih * br
    arr <- array (NA, dim = n)
    col_arrs <- list (r = arr, g = arr, b = arr)
    for (i in seq (3))
    {
        col_arrs [[i]] [1, ] <- top [i, ]
        col_arrs [[i]] [n [1], ] <- bot [i, ]
    }
    # Then fill intervening rows
    indx <- (seq (n [1]) - 1) / (n [1] - 1)
    for (i in seq (3))
        col_arrs [[i]] <- apply (col_arrs [[i]], 2, function (x)
                             (1 - indx) * x [1] + indx * tail (x, 1))
    # Then fill the actual colourmat with RGB colours composed of the 3 indices:
    carr <- array (rgb (col_arrs [[1]], col_arrs [[2]], col_arrs [[3]],
                        maxColorValue = 255), dim = n)

    if (plot)
        plot_colourmat (carr)

    return (carr)
} # end function colour.mat

rotate_colourmat <- function (cols, rotate)
{
    # rotation generally lowers RGB values, so they are increased following
    # rotation according to the following value:
    max_int <- max (cols)

    while (rotate < 0)
        rotate <- rotate + 360
    while (rotate > 360)
        rotate <- rotate - 360

    cols <- cbind (cols, cols)

    # Clockwise rotation shifts the top left to the top right, meaning the
    # index of four colours must move *down* or *to the left* of cols
    i <- floor (rotate / 90) # number of columns to move
    i1 <- 1:4 - i
    if (min (i1) < 1)
        i1 <- i1 + 4
    i2 <- i1 + 1
    x <- (rotate %% 90) / 360
    cols <- (1 - x) * cols [, i1] + x * cols [, i2]
    cols <- apply (cols, 2, function (x)
                   {
                       if (max (x) == 0) rep (0, 3)
                       else x * max_int / max (x)
                   })

    return (cols)
}

plot_colourmat <- function (carr)
{
    plot.new ()
    par (mar = rep (0, 4))
    plot (NULL, NULL, xlim = c(0, dim (carr) [2]), ylim = c (0, dim (carr) [1]))
    for (i in seq (dim (carr) [1]))
        for (j in seq (dim (carr) [2]))
            rect (j - 1, i - 1, j, i, col = carr [i, j])
}
