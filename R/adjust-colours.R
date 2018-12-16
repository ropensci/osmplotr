#' adjust_colours
#'
#' Adjusts a given colour by lightening or darkening it by the specified amount
#' (relative scale of -1 to 1).  Adjustments are made in RGB space, for
#' limitations of which see \code{?convertColor}
#'
#' @param cols A vector of \code{R} colours (for allowable formats of which, see
#' \code{?col2rgb}).
#' @param adj A number between -1 and 1 determining how much to lighten
#' (positive values) or darken (negative values) the colours.
#' @param plot If \code{TRUE}, generates a plot to allow visual comparison of
#' original and adjusted colours.
#' @return Corresponding vector of adjusted colours (as hexadecimal strings).
#' @export
#'
#' @seealso \code{\link{osm_structures}}, \code{?col2rgb}.
#' 
#' @examples
#' cols <- adjust_colours (cols = heat.colors (10), adj = -0.2, plot = TRUE)
#'
#' # 'adjust_colours' also offers an easy way to adjust the default colour
#' # schemes provided by 'osm_structures'. The following lines darken the
#' # highway colour of the 'light' colour scheme by 20%
#' structures <- osm_structures (structures = c('building', 'highway', 'park'),
#'                               col_scheme = 'light')
#' structures$cols [2] <- adjust_colours (structures$cols [2], adj = -0.2)
#' # Plot these structures:
#' bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
#' \dontrun{
#' dat_B <- extract_osm_objects (key = 'building', bbox = bbox)
#' dat_H <- extract_osm_objects (key = 'highway', bbox = bbox)
#' dat_P <- extract_osm_objects (key = 'park', bbox = bbox)
#' }
#' # These data are also included in the 'london' data of 'osmplotr'
#' osm_data <- list (dat_B = london$dat_BNR, dat_H = london$dat_HP, dat_P = london$dat_P)
#' dat <- make_osm_map (structures = structures, osm_data = osm_data, bbox = bbox)
#' print_osm_map (dat$map)


adjust_colours <- function (cols, adj = 0, plot = FALSE)
{
    # ---------------  sanity checks and warnings  ---------------
    check_col_arg (cols)
    if (length (cols) == 0)
        stop ('cols must be non-null')
    if (class (cols [1]) != 'matrix')
        cols <- col2rgb (cols)
    # ---------- adj
    adj <- check_arg (adj, 'adj', 'numeric')
    if (is.character (adj))
        stop (adj)
    else if (adj < -1 | adj > 1)
        stop ('adj must be between -1 and 1')
    # ---------- plot
    plot <- check_arg (plot, 'plot', 'logical')
    if (is.na (plot))
        stop ('plot can not be coerced to logical')
    # ---------------  end sanity checks and warnings  ---------------

    cols_old <- apply (cols, 2, function (x)
                       rgb (x[1], x[2], x[3], maxColorValue = 255))

    if (adj > 0)
        cols <- cols + adj * (255 - cols)
    else
        cols <- cols + adj * cols
    cols <- apply (cols, 2, function (x)
                   rgb (x[1], x[2], x[3], maxColorValue = 255))

    if (plot)
        adj_colours_plot (cols, cols_old)

    return (cols)
} # end function colour.mat

adj_colours_plot <- function (cols, cols_old)
{
    n <- length (cols)
    plot.new ()
    par (mar = rep (0, 4))
    graphics::plot (NULL, NULL, xlim = c(0, n), ylim = c (0, 2),
                    xaxs = 'i', yaxs = 'i')
    for (i in seq (n))
    {
        rect (i - 1, 1, i, 2, col = cols_old [i], border = NA)
        rect (i - 1, 0, i, 1, col = cols [i], border = NA)
    }
    rect (0, 1.4, n, 1.6, col = rgb (1, 1, 1, 0.5), border = NA)
    text (n / 2, 1.5, labels = 'old')
    rect (0, 0.4, n, 0.6, col = rgb (1, 1, 1, 0.5), border = NA)
    text (n / 2, 0.5, labels = 'new')
}
