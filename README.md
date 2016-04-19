[![Build Status](https://travis-ci.org/mpadge/osmplotr.svg?branch=master)](https://travis-ci.org/mpadge/osmplotr) [![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/osmplotr?color=orange)](http://cran.r-project.org/package=osmplotr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/osmplotr)](http://cran.r-project.org/package=osmplotr)

![map1](./figure/map1.png)

R package to produce visually impressive customisable images of OpenStreetMap (OSM) data downloaded internally from the [overpass api](http://overpass-api.de/). Maps can be made with the following easy steps:

1.  Specify the bounding box for the desired region

    ``` r
    bbox <- get_bbox (c(-0.15,51.5,-0.10,51.52))
    ```

2.  Download the desired data---in this case, all building perimeters.

    ``` r
    dat_B <- extract_osm_objects (key="building", bbox=bbox)
    ```

3.  Initiate an `osm_basemap` with desired background (`bg`) colour

    ``` r
    map <- plot_osm_basemap (bbox=bbox, bg="gray20")
    ```

4.  Overlay objects on plot in the desired colour.

    ``` r
    map <- add_osm_objects (map, dat_B, col="gray40")
    ```

5.  Print the map to graphics device of choice

    ``` r
    print (map)
    ```

Installation
------------

``` r
install.packages ('osmplotr')
```

or the development version

``` r
devtools::install_github ('mpadge/osmplotr')
```

A simple map
------------

Simple maps can be made by overlaying different kinds of OSM data in different colours:

``` r
dat_H <- extract_osm_objects (key="highway", bbox=bbox)
dat_P <- extract_osm_objects (key="park", bbox=bbox)
dat_G <- extract_osm_objects (key="landuse", value="grass", bbox=bbox)
```

``` r
map <- plot_osm_basemap (bbox=bbox, bg="gray20")
map <- add_osm_objects (map, dat_B, col="gray40")
map <- add_osm_objects (map, dat_H, col="gray80")
map <- add_osm_objects (map, dat_P, col="darkseagreen")
map <- add_osm_objects (map, dat_G, col="darkseagreen1")
print (map)
```

![map2](./figure/map2.png)

Highlighting selected areas
---------------------------

`osmplotr` is primarily intended as a data visualisation tool, particularly through enabling selected regions to be highlighted. Regions can be defined according to simple point boundaries:

``` r
pts <- sp::SpatialPoints (cbind (c (-0.115, -0.13, -0.13, -0.115),
                             c (51.505, 51.505, 51.515, 51.515)))
```

and OSM objects within the defined regions highlighted with different colour schemes. `cols` defines colours for each group (with only one here), while `bg` defines the colour of the remaining, background area.

``` r
map <- plot_osm_basemap (bbox=bbox, bg="gray20")
map <- add_osm_groups (map, dat_B, groups=pts, cols="orange", bg="gray40")
map <- add_osm_objects (map, london$dat_P, col="darkseagreen1")
map <- add_osm_groups (map, london$dat_P, groups=pts, cols='darkseagreen1',
                   bg='darkseagreen', boundary=0)
print (map)
```

![map3](./figure/map3.png)

Or maybe highlighted in dark-on-light:

``` r
map <- plot_osm_basemap (bbox=bbox, bg="gray95")
map <- add_osm_groups (map, dat_B, groups=pts, cols="gray40", bg="gray85",
                   boundary=1)
map <- add_osm_groups (map, dat_H, groups=pts, cols="gray20", bg="gray70",
                   boundary=0)
print (map)
```

![map4](./figure/map4.png)

### Highlighting clusters

`add_osm_groups` also enables plotting an entire region as a group of spatially distinct clusters of defined colours. The argument `groups` in the following call is a list of `SpatialPoints` objects defining 12 small, spatially separated regions. Calling `add_osm_groups` with no `bg` argument forces all points lying outside those defined groups to be allocated to the nearest groups, and thus produces an inclusive grouping extending across an entire region.

``` r
map <- plot_osm_basemap (bbox=bbox, bg='gray20')
map <- add_osm_groups (map, dat_B, groups=groups, make_hull=TRUE,
                       borderWidth=2)
print (map)
```

![map5](./figure/map5.png)

### Highlighting areas bounded by named highways

An alternative way of defining highlighted groups is by naming the highways encircling desired regions.

``` r
highways <- c ('Davies.St', 'Berkeley.Sq', 'Berkeley.St', 'Piccadilly',
               'Regent.St', 'Oxford.St')
highways1 <- highways2polygon (highways=highways, bbox=bbox)
highways <- c ('Regent.St', 'Oxford.St', 'Shaftesbury')
highways2 <- highways2polygon (highways=highways, bbox=bbox)
highways <- c ('Piccadilly', 'Shaftesbury.Ave', 'Charing.Cross.R',
               'Saint.Martin', 'Trafalgar.Sq', 'Cockspur.St',
               'Pall.Mall', 'St.James')
highways3 <- highways2polygon (highways=highways, bbox=bbox)
highways <- c ('Charing.Cross', 'Duncannon.St', 'Strand', 'Aldwych',
               'Kingsway', 'High.Holborn', 'Shaftesbury.Ave')
highways4 <- highways2polygon (highways=highways, bbox=bbox)
highways <- c ("Kingsway", "Holborn", "Farringdon.St", "Strand",
               "Fleet.St", "Aldwych")
highways5 <- highways2polygon (highways=highways, bbox=bbox)
groups <- list (highways1, highways2, highways3, highways4, highways5)
```

And then passing the SpatialPolygon returned by `highways2polygon` to `add_osm_groups`, this time with some Wes Anderson flair.

``` r
map <- plot_osm_basemap (bbox=bbox, bg='gray20')
library (wesanderson)
cols <- wes_palette ("Darjeeling", 5) 
map <- add_osm_groups (map, dat_B, groups=groups, boundary=1,
                       cols=cols, bg='gray40', colmat=FALSE)
map <- add_osm_groups (map, dat_H, groups=groups, boundary=0,
                       cols=cols, bg='gray70', colmat=FALSE)
print (map)
```

![map6](./figure/map6.png)

See package vignettes ('Downloading Data' and 'Making Maps') for a lot more detail and further capabilities of `osmplotr`.

### Data surfaces

Finally, `osmplotr` contains a function `add_osm_surface` to spatially interpolate a given set of spatial data points and colour OSM objects according to a specified colour gradient. This is illustrated here with the `volcano` data projected onto the `bbox`.

``` r
x <- seq (bbox [1,1], bbox [1,2], length.out=dim (volcano)[1])
y <- seq (bbox [2,1], bbox [2,2], length.out=dim (volcano)[2])
xy <- cbind (rep (x, dim (volcano) [2]), rep (y, each=dim (volcano) [1]))
z <- as.numeric (volcano)
```

`add_osm_surface` returns the limits of the *interpolated* surface. These may differ from the original limits, and should be used to scale `add_colourbar`.

``` r
map <- plot_osm_basemap (bbox=bbox, bg="gray20")
cols <- gray (0:50 / 50)
map <- add_osm_surface (map, dat_B, dat=cbind (xy, z), cols=cols)
map <- add_osm_surface (map, dat_H, dat=cbind (xy, z), 
                        cols=adjust_colours (cols, -0.2)) # Darken cols by ~20%
map <- add_colourbar (map, cols=cols, zlims=range (volcano))
map <- add_axes (map)
print (map)
```

![map7](./figure/map7.png)
