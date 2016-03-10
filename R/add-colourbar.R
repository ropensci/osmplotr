#' add_colorbar
#'
#' Adds a colourbar to an existing map. Intended to be used in combination with
#' add_osm_surface (). At present, only plots on right side of map.
#'
#' @param len Relative positions of start and end of colourbar
#' @param width Relative width
#' @param side (1,2,3,4) for colourbar (below, left, above, right) of map. (Not
#' yet implemented.)
#' @param cols Vector of colours
#' @param zlims Vector of (min,max) values for scale of colourbar. These should
#' be the values returned from add_osm_surface (). 
#' @param transp Transparency level of region immediately surrounding colourbar,
#' including behind text. Lower values are more transparent.
#' @param tcol Colour of text, tick marks, and lines on colourbar
#' @param ps PointSize of text labels
#' @export

add_colourbar <- function (len=c(0.1, 0.9), width=0.02, side=1, cols=NULL,
                           zlims=c(0,1), transp=0.4, tcol="black", ps=8)
{
    if (is.null (cols))
        stop ("cols must be specified in add_colourbar")

    usr <- par ("usr")
    # width = positions of elements: (l-edge/text, lbar, rbar, r-edge)
    if (side == 2)
    {
        tpos <- 4
        width <- c (1.5, 0.25, 1.25, 0) * width
    } else
    {
        width <- 1 - c (1.5, 1.25, 0.25, 0) * width
        tpos <- 2
    }
    ulen <- usr [3] + len * (usr [4] - usr [3])
    width <- usr [1] + width * (usr [2] - usr [1])
    # Transparent box underlying colourbar:
    y <- ulen [1] + c (-0.05, 1.05) * diff (ulen)
    rect (width [1], y [1], width [4], y [2], col=rgb (1, 1, 1, transp), border=NA)

    ncols <- length (cols)
    y <- seq (ulen [1], ulen [2], len=ncols)
    for (i in 2:length (y))
        rect (width [2], y [i-1], width [3], y [i], col=cols [i], border=NA)
    rect (width [2], ulen [1], width [3], ulen [2], col=NA, border="black")
    yvals <- pretty (zlims)
    yvals <- yvals [yvals > zlims [1] & yvals < zlims [2]]
    # max character string (width, height) in "usr" coordinates
    par (ps=ps)
    ch_width <- max (sapply (yvals, strwidth))
    # Then extend width by equivalent of one character
    nc <- max (sapply (yvals, nchar))
    ch_width <- ch_width * (nc + 1) / nc
    if (side == 2)
        wx <- c (width [1], width [1] + ch_width)
    else
        wx <- c (width [1] - ch_width, width [1])
    ch_height <- max (sapply (yvals, strheight))
    ch_height <- ch_height * 1.5
    ytxt <- sapply (yvals, as.character)
    # Transform yvals to plot scale:
    yvals <- (yvals - zlims [1]) / diff (zlims)
    yvals <- usr [3] + (len [1] + yvals * diff (len)) * (usr [4] - usr [3])
    for (i in seq (yvals)) 
    {
        lines (c (width [1], width [3]), rep (yvals [i], 2), col="black")
        rect (wx [1], yvals [i] - ch_height / 2, 
              wx [2], yvals [i] + ch_height / 2, 
              col=rgb (1, 1, 1, transp), border=NA)
        text (width [1], yvals [i], pos=tpos, labels=ytxt [i], offset=0.2)
    }
}
