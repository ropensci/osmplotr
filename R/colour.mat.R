#' color.mat
#'
#' Generates a 2D matrix of graduated colours.
#'
#' @param n = number of rows and columns (default = 10; if length 2, then
#' dimensions of rectangle). 
#' @param cols = vector of length >= 4 of colors (example, default = rainbow
#' (4), or from RColorBrewer, brewer.pal (4, "Set1")). cols are wrapped
#' clockwise around the corners from top left to bottom left, so matrix may be
#' rotated by altering the sequence of cols.
#' @param plot = FALSE (default) plots the colour matrix
#' @return matrix of colours

colour.mat <- function (n=c(10, 10), cols=NULL, plot=TRUE)
{
    if (is.null (cols))
        cols <- rainbow (4)
    else if (length (cols) < 4)
        stop ("cols must have length = 4")

    cols <- cols [round (1:4 * length (cols) / 4)]
    if (class (cols [1]) != "matrix")
        cols <- col2rgb (cols)

    if (length (n) == 1)
        n <- rep (n, 2)

    tl <- cols [,1] # top left
    tr <- cols [,2] # top right
    bl <- cols [,3] # bottom left
    br <- cols [,4] # bottom right
    # the corner colours are then (linearly) graduated across the top and bottom
    # rows
    indx.tr <- seq (tl [1], tr [1], length.out=n [1]) # top red shade
    indx.tg <- seq (tl [2], tr [2], length.out=n [1]) # top green shade
    indx.tb <- seq (tl [3], tr [3], length.out=n [1]) # top blue shade
    indx.br <- seq (bl [1], br [1], length.out=n [1]) 
    indx.bg <- seq (bl [2], br [2], length.out=n [1]) 
    indx.bb <- seq (bl [3], br [3], length.out=n [1]) 

    # Then fill colums with the vector indxs of RGB colours:
    indx.r <- indx.g <- indx.b <- array (NA, dim=n)
    tr.arr <- array (indx.tr, dim=n)
    br.arr <- array (indx.br, dim=n)
    tg.arr <- array (indx.tg, dim=n)
    bg.arr <- array (indx.bg, dim=n)
    tb.arr <- array (indx.tb, dim=n)
    bb.arr <- array (indx.bb, dim=n)
    p <- t (array ((1:n[2]) / n[2], dim=rev (n))) # proportion of Top vs. Bottom colours

    indx.r <- p * tr.arr + (1 - p) * br.arr
    indx.g <- p * tg.arr + (1 - p) * bg.arr
    indx.b <- p * tb.arr + (1 - p) * bb.arr

    # Then fill the actual colourmat with RGB colours composed of the 3 indices:
    carr <- array (rgb (indx.r, indx.g, indx.b, maxColorValue=255), dim=n)

    if (plot) {
        x11 ()
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
