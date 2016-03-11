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
    # Default sanity checks
    if (!side %in% 1:4) side <- 4
    if (!transp >= 0 & transp <= 1) transp <- 0.4
    if (!width > 0 & width < 0.5) width <- 0.02
    if (!is.numeric (ps)) ps <- 8
    if (!all (is.numeric (len))) len <- c (0.1, 0.9)

    ncols <- length (cols)
    usr <- par ("usr")
    if (side == 2 | side == 4)
    {
        ulen <- usr [3] + len * (usr [4] - usr [3])
        if (side == 2)
        {
            tpos <- 4
            # width = positions of elements: (l-edge/text, lbar, rbar, r-edge)
            width <- c (1.5, 0.25, 1.25, 0) * width
        } else 
        {
            width <- 1 - c (1.5, 1.25, 0.25, 0) * width
            tpos <- 2
        }
        width <- usr [1] + width * (usr [2] - usr [1])
        # Transparent box underlying colourbar:
        y <- ulen [1] + c (-0.02, 1.02) * diff (ulen)
        rect (width [1], y [1], width [4], y [2], col=rgb (1, 1, 1, transp), 
              border=NA)
        y <- seq (ulen [1], ulen [2], len=ncols)
        for (i in 2:length (y))
            rect (width [2], y [i-1], width [3], y [i], col=cols [i], border=NA)
        rect (width [2], ulen [1], width [3], ulen [2], col=NA, border="black")
    } else
    {
        ulen <- usr [1] + len * (usr [2] - usr [1])
        if (side == 1)
        {
            tpos <- 3
            height <- c (0, 0.25, 1.25, 1.5) * width
        } else
        {
            tpos <- 1
            height <- 1 - c (1.5, 1.25, 0.25, 0) * width
        }
        x <- ulen [1] + c (-0.02, 1.02) * diff (ulen)
        height <- usr [3] + height * (usr [4] - usr [3])
        rect (x [1], height [1], x [2], height [4],
              col=rgb (1, 1, 1, transp), border=NA)
        x <- seq (ulen [1], ulen [2], len=ncols)
        for (i in 2:length (x))
            rect (x [i-1], height [2], x [i], height [3], col=cols [i],
                  border=NA)
        rect (ulen [1], height [2], ulen [2], height [3], col=NA,
              border="black")
    }

    yvals <- pretty (zlims)
    yvals <- yvals [yvals > zlims [1] & yvals < zlims [2]]
    # max character string (width, height) in "usr" coordinates
    par (ps=ps)
    ch_width <- max (sapply (yvals, strwidth))
    # Then extend width by equivalent of one character
    nc <- max (sapply (yvals, nchar))
    ch_width <- ch_width * (nc + 1) / nc
    ch_height <- max (sapply (yvals, strheight))
    ch_height <- ch_height * 1.5
    ytxt <- sapply (yvals, as.character)
    if (side == 1 | side == 3)
    {
        wx <- ch_width * c (-0.5, 0.5)
        if (side == 1)
        {
            ty <- height [4]
            wy <- height [4] + c (0, ch_height)
        } else
        {
            ty <- height [1]
            wy <- height [1] - c (ch_height, 0)
        }
        # Transform yvals to plot scale:
        yvals <- (yvals - zlims [1]) / diff (zlims)
        yvals <- usr [1] + (len [1] + yvals * diff (len)) * (usr [2] - usr [1])
        for (i in seq (yvals)) 
        {
            lines (rep (yvals [i], 2), c (height [2], height [4]), col="black")
            rect (yvals [i] - wx, wy [1], yvals [i] + wx, wy [2],
                  col=rgb (1, 1, 1, transp), border=NA)
            text (yvals [i], ty, pos=tpos, labels=ytxt [i], offset=0.1)
        }
    } else
    {
        if (side == 2)
            wx <- c (width [1], width [1] + ch_width)
        else
            wx <- c (width [1] - ch_width, width [1])
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
}
