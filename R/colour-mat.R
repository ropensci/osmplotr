#' colour_mat
#'
#' Generates a 2D matrix of graduated colours.
#'
#' @param n = number of rows and columns (default = 10; if length 2, then
#' dimensions of rectangle). 
#' @param cols = vector of length >= 4 of colors (example, default = rainbow
#' (4), or from RColorBrewer, brewer.pal (4, "Set1")). cols are wrapped
#' clockwise around the corners from top left to bottom left. 
#' @param rotate rotates the entire colour matrix by the specified angle (in
#' degrees).
#' @param plot = FALSE (default) plots the colour matrix
#' @return matrix of colours
#' @examples
#' \dontrun{
#'   cm <- colour_mat (n=20, cols=rainbow(4), rotate=90, plot=TRUE)
#' }

colour_mat <- function (n=c(10, 10), cols=NULL, rotate=NULL, plot=FALSE)
{
    if (is.null (cols))
        cols <- rainbow (4)
    else if (length (cols) < 4)
        stop ("cols must have length = 4")

    cols <- cols [round (1:4 * length (cols) / 4)]
    if (class (cols [1]) != "matrix")
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
        i <- ceiling ((rotate + 1) / 90)
        cols <- cbind (cols, cols)
        i1 <- i:(i+3)
        i2 <- i1 + 1
        x <- (rotate %% 90) / 360
        cols <- (1 - x) * cols [,i1] + x * cols [,i2]
        cols <- apply (cols, 2, function (x) x * max_int / max (x))
    }

    if (length (n) == 1)
        n <- rep (n, 2)

    tl <- cols [,1] # top left
    tr <- cols [,2] # top right
    bl <- cols [,3] # bottom left
    br <- cols [,4] # bottom right
    # the corner colours are then (linearly) graduated across the top and bottom
    # rows
    indx_tr <- seq (tl [1], tr [1], length.out=n [1]) # top red shade
    indx_tg <- seq (tl [2], tr [2], length.out=n [1]) # top green shade
    indx_tb <- seq (tl [3], tr [3], length.out=n [1]) # top blue shade
    indx_br <- seq (bl [1], br [1], length.out=n [1]) 
    indx_bg <- seq (bl [2], br [2], length.out=n [1]) 
    indx_bb <- seq (bl [3], br [3], length.out=n [1]) 

    # Then fill colums with the vector indxs of RGB colours:
    indx_r <- indx_g <- indx_b <- array (NA, dim=n)
    tr_arr <- array (indx_tr, dim=n)
    br_arr <- array (indx_br, dim=n)
    tg_arr <- array (indx_tg, dim=n)
    bg_arr <- array (indx_bg, dim=n)
    tb_arr <- array (indx_tb, dim=n)
    bb_arr <- array (indx_bb, dim=n)
    p <- t (array ((1:n[2]) / n[2], dim=rev (n))) # proportion of Top vs. Bottom colours

    indx_r <- p * tr_arr + (1 - p) * br_arr
    indx_g <- p * tg_arr + (1 - p) * bg_arr
    indx_b <- p * tb_arr + (1 - p) * bb_arr

    # Then fill the actual colourmat with RGB colours composed of the 3 indices:
    carr <- array (rgb (indx_r, indx_g, indx_b, maxColorValue=255), dim=n)

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
