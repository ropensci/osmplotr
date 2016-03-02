#' colour_mat
#'
#' Generates a 2D matrix of graduated colours.
#'
#' @param n number of rows and columns (default = 10; if length 2, then
#' dimensions of rectangle). 
#' @param cols vector of length >= 4 of colors (example, default = rainbow
#' (4), or from RColorBrewer, brewer.pal (4, 'Set1')). cols are wrapped
#' clockwise around the corners from top left to bottom left. 
#' @param rotate rotates the entire colour matrix by the specified angle (in
#' degrees).
#' @param plot plots the colour matrix (default FALSE)
#' @return matrix of colours
#' @export
#'
#' @examples
#' library (osmplotr)
#' cm <- colour_mat (n=20, cols=rainbow(4), rotate=90, plot=TRUE)

colour_mat <- function (n=c(10, 10), cols=NULL, rotate=NULL, plot=FALSE)
{
    if (is.null (cols))
        cols <- rainbow (4)
    else if (length (cols) < 4)
        stop ('cols must have length = 4')

    cols <- cols [round (1:4 * length (cols) / 4)]
    if (class (cols [1]) != 'matrix')
        cols <- col2rgb (cols)

    if (!is.null (rotate))
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

    if (length (n) == 1)
        n <- rep (n, 2)

    tl <- cols [,1] # top left
    tr <- cols [,2] # top right
    br <- cols [,3] # bottom right
    bl <- cols [,4] # bottom left
    # Then interpolate, starting with top and bottom rows
    ih <- t (array (seq (n [1]) - 1, dim=c (n [1], 3))) / (n [1] - 1)
    iv <- t (array (seq (n [2]) - 1, dim=c (n [2], 3))) / (n [2] - 1)
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
    indx <- (seq (n [2]) - 1) / (n [2] - 1)
    for (i in seq (3))
        col_arrs [[i]] <- apply (col_arrs [[i]], 2, function (x)
                             (1 - indx) * x [1] + indx * tail (x, 1))
    # Then fill the actual colourmat with RGB colours composed of the 3 indices:
    carr <- array (rgb (col_arrs [[1]], col_arrs [[2]], col_arrs [[3]], 
                        maxColorValue=255), dim=n)

    if (plot) {
        plot.new ()
        par (mar=rep (0, 4))
        plot (NULL, NULL, xlim=c(0, n[1]), ylim=c (0, n[2]))
        for (i in 1:n [1]) {
            for (j in 1:n [2]) {
                rect (i - 1, j - 1, i, j, col=carr [i, j])
            } # end for j
        } # end for i
    }
    return (carr)
} # end function colour.mat
