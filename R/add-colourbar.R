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
#'
#' @examples
#' plot_osm_basemap (bbox=get_bbox (c (-0.15, 51.5, -0.1, 51.52)), col="gray20")
#' add_osm_objects (london$dat_BNR, col="gray40") # non-residential buildings
#' add_axes ()
#' add_colourbar (cols=heat.colors(20), tcol="white", side=4)

add_colourbar <- function (len=c(0.1, 0.9), width=0.02, side=1, cols=NULL,
                           zlims=c(0,1), transp=0.4, tcol="black", ps=8)
{
    if (is.null (cols))
        stop ("cols must be specified in add_colourbar")
    # Default sanity checks
    if (!side %in% 1:4) side <- 4
    if (!transp >= 0 & transp <= 1) transp <- 0.4
    if (!width > 0 & width < 0.5) width <- 0.02
    if (!is.numeric (ps)) ps <- 8
    if (!all (is.numeric (len))) len <- c (0.1, 0.9)

    ncols <- length (cols)
    usr <- par ("usr")
    tpos <- c (3, 4, 1, 2) [side] # 'pos' for text () command
    # epos = positions of elements: (l-edge/text, lbar, rbar, r-edge)
    if (side == 1)
        epos <- c (0, 0.25, 1.25, 1.5) * width
    else if (side == 2)
        epos <- c (1.5, 0.25, 1.25, 0) * width
    else
        epos <- 1 - c (1.5, 1.25, 0.25, 0) * width

    i2 <- 1:2 # used in text positioning below
    if (side == 1 | side == 3)
    {
        indx <- c (2, 1, 4, 3)
        ulen <- usr [1] + len * (usr [2] - usr [1])
        epos <- usr [3] + epos * (usr [4] - usr [3])
    } else
    {
        i2 <- i2 + 2
        indx <- 1:4
        ulen <- usr [3] + len * (usr [4] - usr [3])
        epos <- usr [1] + epos * (usr [2] - usr [1])
    }
    # Transparent box underlying colourbar:
    tb <- ulen [1] + c (-0.02, 1.02) * diff (ulen)
    xy <- c (epos [1], tb [1], epos [4], tb [2]) [indx]
    rect (xy [1], xy [2], xy [3], xy [4], col=rgb (1, 1, 1, transp), border=NA)
    # Then actual colourbar
    cb <- seq (ulen [1], ulen [2], len=ncols)
    for (i in 2:length (cb))
    {
        ri <- c (epos [2], cb [i-1], epos [3], cb [i])
        rect (ri [indx [1]], ri [indx [2]], ri [indx [3]], ri [indx [4]],
              col=cols [i], border=NA)
    }
    xy <- c (epos [2], ulen [1], epos [3], ulen [2]) [indx]
    rect (xy [1], xy [2], xy [3], xy [4], col=NA, border="black")

    lvals <- pretty (zlims)
    lvals <- lvals [lvals > zlims [1] & lvals < zlims [2]]
    # max character string (width, height) in "usr" coordinates
    par (ps=ps)
    ch_width <- max (sapply (lvals, strwidth))
    # Then extend width by equivalent of one character
    nc <- max (sapply (lvals, nchar))
    ch_width <- ch_width * (nc + 1) / nc
    ch_height <- max (sapply (lvals, strheight))
    ch_height <- ch_height * 1.5
    ytxt <- sapply (lvals, as.character)
    lvals <- (lvals - zlims [1]) / diff (zlims)
    # Transform lvals to plot scale:
    lvals <- usr [i2 [1]] + (len [1] + lvals * diff (len)) * 
                            (usr [i2 [2]] - usr [i2 [1]])
    # Then print the axis labels. These require distinctly different adjustments
    # for horizontal and vertical options, so it the only bit requiring an
    # extensive if-clause.
    if (side == 1 | side == 3)
    {
        wx <- ch_width * c (-0.5, 0.5)
        if (side == 1)
        {
            ty <- epos [4]
            wy <- epos [4] + c (0, ch_height)
        } else
        {
            ty <- epos [1]
            wy <- epos [1] - c (ch_height, 0)
        }
        for (i in seq (lvals)) 
        {
            lines (rep (lvals [i], 2), c (epos [2], epos [4]), col="black")
            rect (lvals [i] - wx, wy [1], lvals [i] + wx, wy [2],
                  col=rgb (1, 1, 1, transp), border=NA)
            text (lvals [i], ty, pos=tpos, labels=ytxt [i], offset=0.1)
        }
    } else
    {
        if (side == 2)
            wx <- c (epos [1], epos [1] + ch_width)
        else
            wx <- c (epos [1] - ch_width, epos [1])
        for (i in seq (lvals)) 
        {
            lines (c (epos [1], epos [3]), rep (lvals [i], 2), col="black")
            rect (wx [1], lvals [i] - ch_height / 2, 
                  wx [2], lvals [i] + ch_height / 2, 
                  col=rgb (1, 1, 1, transp), border=NA)
            text (epos [1], lvals [i], pos=tpos, labels=ytxt [i], offset=0.2)
        }
    }
}
