% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add-osm-surface.R
\name{add_osm_surface}
\alias{add_osm_surface}
\title{add_osm_surface}
\usage{
add_osm_surface(
  map,
  obj,
  dat,
  method = "idw",
  grid_size = 100,
  cols = heat.colors(30),
  bg,
  size,
  shape
)
}
\arguments{
\item{map}{A \code{ggplot2} object to which the surface are to be added}

\item{obj}{An \code{sp} \code{SpatialPolygonsDataFrame} or
\code{SpatialLinesDataFrame} (list of polygons or lines) returned by
\code{\link{extract_osm_objects}}}

\item{dat}{A matrix or data frame of 3 columns (x, y, z), where (x, y) are
(longitude, latitude), and z are the values to be interpolated}

\item{method}{Either \code{idw} (Inverse Distance Weighting as
\code{spatstat.explore::idw}; default), \code{Gaussian} for kernel smoothing
(as \code{spatstat.explore::Smooth.ppp}), or any other value to avoid
interpolation. In this case, \code{dat} must be regularly spaced in \code{x}
and \code{y}.}

\item{grid_size}{size of interpolation grid}

\item{cols}{Vector of colours for shading z-values (for example,
\code{terrain.colors (30)})}

\item{bg}{If specified, OSM objects outside the convex hull surrounding
\code{dat} are plotted in this colour, otherwise they are included in the
interpolation (which will generally be inaccurate for peripheral values)}

\item{size}{Size argument passed to \code{ggplot2} (polygon, path, point)
functions: determines width of lines for (polygon, line), and sizes of
points.  Respective defaults are (0, 0.5, 0.5). If \code{bg} is provided and
\code{size} has 2 elements, the second determines the \code{size} of the
background objects.}

\item{shape}{Shape of lines or points, for details of which see
\code{?ggplot::shape}. If \code{bg} is provided and \code{shape} has 2
elements, the second determines the \code{shape} of the background objects.}
}
\value{
modified version of \code{map} to which surface has been added
}
\description{
Adds a colour-coded surface of spatial objects (polygons, lines, or points
generated by \code{\link{extract_osm_objects}} to a graphics object
initialised with \code{\link{osm_basemap}}. The surface is spatially
interpolated between the values given in \code{dat}, which has to be a matrix
of \code{data.frame} of 3 columns (x, y, z), where (x,y) are (longitude,
latitude), and z are the values to be interpolated. Interpolation uses
\code{spatstat.explore::Smooth.ppp}, which applies a Gaussian kernel smoother
optimised to the given data, and is effectively non-parametric.
}
\note{
Points beyond the spatial boundary of \code{dat} are included in the surface
if \code{bg} is not given. In such cases, values for these points may exceed
the range of provided data because the surface will be extrapolated beyond
its domain.  Actual plotted values are therefore restricted to the range of
given values, so any extrapolated points greater or less than the range of
\code{dat} are simply set to the respective maximum or minimum values. This
allows the limits of \code{dat} to be used precisely when adding colourbars
with \code{\link{add_colourbar}}.
}
\examples{
# Get some data
bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
# dat_B <- extract_osm_objects (key = 'building', bbox = bbox)
# These data are also provided in
dat_B <- london$dat_BNR # actuall non-residential buildings
# Make a data surface across the map coordinates, and remove periphery
n <- 5
x <- seq (bbox [1, 1], bbox [1, 2], length.out = n)
y <- seq (bbox [2, 1], bbox [2, 2], length.out = n)
dat <- data.frame (
    x = as.vector (array (x, dim = c (n, n))),
    y = as.vector (t (array (y, dim = c (n, n)))),
    z = x * y
)
map <- osm_basemap (bbox = bbox, bg = "gray20")
map <- add_osm_surface (map, dat_B, dat = dat, cols = heat.colors (30))
\donttest{
print_osm_map (map)
}

# If data do not cover the entire map region, then the peripheral remainder
# can be plotted by specifying the 'bg' colour. First remove periphery from
# 'dat':
d <- sqrt ((dat$x - mean (dat$x))^2 + (dat$y - mean (dat$y))^2)
dat <- dat [which (d < 0.01), ]
map <- osm_basemap (bbox = bbox, bg = "gray20")
map <- add_osm_surface (
    map,
    dat_B,
    dat = dat,
    cols = heat.colors (30),
    bg = "gray40"
)
\donttest{
print_osm_map (map)
}

# Polygons and (lines/points) can be overlaid as data surfaces with different
# colour schemes.
# dat_HP <- extract_osm_objects (key = 'highway',
#                                value = 'primary',
#                                bbox = bbox)
# These data are also provided in
dat_HP <- london$dat_HP
cols <- adjust_colours (heat.colors (30), adj = -0.2) # darken by 20\%
map <- add_osm_surface (
    map,
    dat_HP,
    dat,
    cols = cols,
    bg = "gray60",
    size = c (1.5, 0.5)
)
\donttest{
print_osm_map (map)
}

# Adding multiple surfaces of either polygons or (lines/points) produces a
# 'ggplot2' warning, and forces the colour gradient to revert to the last
# given value.
dat_T <- london$dat_T # trees
map <- osm_basemap (bbox = bbox, bg = "gray20")
map <- add_osm_surface (
    map,
    dat_B,
    dat = dat,
    cols = heat.colors (30),
    bg = "gray40"
)
map <- add_osm_surface (
    map,
    dat_HP,
    dat,
    cols = heat.colors (30),
    bg = "gray60",
    size = c (1.5, 0.5)
)
map <- add_osm_surface (
    map,
    dat_T,
    dat,
    cols = topo.colors (30),
    bg = "gray70",
    size = c (5, 2),
    shape = c (8, 1)
)
\donttest{
print_osm_map (map) # 'dat_HP' is in 'topo.colors' not 'heat.colors'
}

# Add axes and colourbar
map <- add_axes (map)
map <- add_colourbar (
    map,
    cols = heat.colors (100),
    zlims = range (dat$z),
    barwidth = c (0.02),
    barlength = c (0.6, 0.99),
    vertical = TRUE
)
\donttest{
print_osm_map (map)
}
}
\seealso{
\code{\link{osm_basemap}}, \code{\link{add_colourbar}}.

Other maps-with-data: 
\code{\link{add_osm_groups}()}
}
\concept{maps-with-data}
