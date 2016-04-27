#' colour_mat
#'
#' Generates a 2D matrix of graduated colours by interpolating between the given
#' colours specifying the four corners.
#'
#' @param n number of rows and columns of colour matrix (default = 10; if length
#' 2, then dimensions of rectangle). 
#' @param cols vector of length >= 4 of colors (example, default = rainbow
#' (4), or from RColorBrewer, brewer.pal (4, 'Set1')). cols are wrapped
#' clockwise around the corners from top left to bottom left. 
#' @param rotate rotates the entire colour matrix by the specified angle (in
#' degrees).
#' @param plot plots the colour matrix 
#' @return matrix of colours
#' @export
#'
#' @seealso \code{\link{add_osm_groups}}.
#'
#' @examples
#' cm <- colour_mat (n=5, cols=rainbow(4), rotate=90, plot=TRUE)
#'
#' # 'colour_mat' is intended primarily for use in colouring groups added with
#' # 'add_osm_groups' using the 'colmat=TRUE' option:
#' bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
#' # Generate random points to serve as group centres
#' set.seed (2)
#' ngroups <- 6
#' x <- bbox [1,1] + runif (ngroups) * diff (bbox [1,])
#' y <- bbox [2,1] + runif (ngroups) * diff (bbox [2,])
#' groups <- cbind (x, y)
#' groups <- apply (groups, 1, function (i) 
#'                  sp::SpatialPoints (matrix (i, nrow=1, ncol=2)))
#' # plot a basemap and add groups
#' map <- plot_osm_basemap (bbox=bbox, bg="gray20")
#' map <- add_osm_groups (map, obj=london$dat_BNR, group=groups, cols=rainbow (4),
#'                        colmat=TRUE, rotate=90)
#' print (map)

colour_mat <- function (n=c(10, 10), cols, rotate, plot=FALSE)
{
    if (missing (cols))
        cols <- rainbow (4)
    else if (length (cols) < 4)
        stop ('cols must have length = 4')

    cols <- cols [round (1:4 * length (cols) / 4)]
    if (class (cols [1]) != 'matrix')
        cols <- col2rgb (cols)

    if (length (n) == 1)
        n <- rep (n, 2)

    if (!missing (rotate))
    {
        # rotation generally lowers RGB values, so they are increased following
        # rotation according to the following value:
        max_int <- max (cols)
        stopifnot (is.numeric (rotate))
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
        cols <- (1 - x) * cols [,i1] + x * cols [,i2]
        cols <- apply (cols, 2, function (x) x * max_int / max (x))
    }

    tl <- cols [,1] # top left
    tr <- cols [,2] # top right
    br <- cols [,3] # bottom right
    bl <- cols [,4] # bottom left
    # Then interpolate, starting with top and bottom rows
    ih <- t (array (seq (n [2]) - 1, dim=c (n [2], 3))) / (n [2] - 1)
    iv <- t (array (seq (n [1]) - 1, dim=c (n [1], 3))) / (n [1] - 1)
    top <- (1 - ih) * tl + ih * tr
    bot <- (1 - ih) * bl + ih * br
    arr <- array (NA, dim=n)
    col_arrs <- list (r=arr, g=arr, b=arr)
    for (i in seq (3))
    {
        col_arrs [[i]] [1,] <- top [i,]
        col_arrs [[i]] [n [1],] <- bot [i,]
    }
    # Then fill intervening rows
    indx <- (seq (n [1]) - 1) / (n [1] - 1)
    for (i in seq (3))
        col_arrs [[i]] <- apply (col_arrs [[i]], 2, function (x)
                             (1 - indx) * x [1] + indx * tail (x, 1))
    # Then fill the actual colourmat with RGB colours composed of the 3 indices:
    carr <- array (rgb (col_arrs [[1]], col_arrs [[2]], col_arrs [[3]], 
                        maxColorValue=255), dim=n)

    if (plot) {
        plot.new ()
        par (mar=rep (0, 4))
        plot (NULL, NULL, xlim=c(0, n[2]), ylim=c (0, n[1]))
        for (i in 1:n [1]) 
            for (j in 1:n [2]) 
                rect (j - 1, i - 1, j, i, col=carr [i, j])
    }
    return (carr)
} # end function colour.mat
