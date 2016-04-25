**Fix TODOs!** This vignette extends from the vignette `making-maps` to demonstrate how user-defined data layers may be used to further customise the appearance of maps produced with `osmplotr`.

1. Introduction
---------------

``` r
library (osmplotr)
library (maptools) # Needed for this vignette
```

As in the first vignette, maps produced in this vignette contain data for a small portion of central London, U.K.

``` r
bbox <- get_bbox (c(-0.13,51.50,-0.11,51.52))
```

5. Highlighting particular areas
--------------------------------

One of the primary aims of `osmplotr` is to offer a convenient means to highlight particular regions within a city simply by applying different colours to the same OSM structures. The routine which enables this is `add_osm_groups`, the two primary arguments to which are `obj`, which defines the OSM structure to be used for plotting the regions (for example, `dat_B` defining the previous buildings), and `groups` which is a list of SpatialPoints objects defining the desired regions.

The simplest way of defining a region is with `click_map`, which enables a map to be clicked and returns a set of `SpatialPoints` corresponding to the clicks. (`click_map` finishes when the same point is clicked twice.) This set of points, or a set of points generated through any other means, can then be passed as the `groups` argument to `add_osm_groups`. The following illustrates an area defined by manually entering coordinates of bounding points.

``` r
pts <- SpatialPoints (cbind (c (-0.115, -0.125, -0.125, -0.115),
                             c (51.505, 51.505, 51.515, 51.515)))
bbox <- get_bbox (c(-0.13,51.5,-0.11,51.52))
map <- plot_osm_basemap (bbox=bbox, bg="gray20")
map <- add_osm_groups (map, dat_B, groups=pts, col="orange", bg="gray40",
                   colmat=FALSE) 
```

``` r
print (map)
```

![map10](map10.png)

### 5.1 Inclusive, exclusive, and bisected polygons

The highlighted region of the previous map is irregular because inclusion for each polygon within a group is defined by mean coordinates. `add_osm_groups` has a `boundary` argument which defines whether objects should be assigned to groups inclusively (`boundary>0`) or exclusively (`boundary<0`), or whether they should be precisely bisected by a group boundary (`boundary=0`). The two options in addition to the above default of `boundary=-1` produce the following maps.

``` r
map <- plot_osm_basemap (bbox=bbox, bg="gray20")
map <- add_osm_groups (map, dat_B, pts, col="orange", bg="gray40", colmat=FALSE,
                       boundary=0)
```

``` r
print (map)
```

![map11](map11.png)

``` r
map <- plot_osm_basemap (bbox=bbox, bg="gray20")
map <- add_osm_groups (map, dat_B, groups=pts, col="orange", bg="gray40", 
                       colmat=FALSE, boundary=1)
```

``` r
print (map)
```

![map12](map12.png)

The ability to combine inclusive and bisected polygons is particularly useful when selected areas partially contain large polygons such as parks. The following map is created with buildings plotting *inclusively* within the group, and parks bisected by the boundary.

``` r
pts <- SpatialPoints (cbind (c (-0.117, -0.122, -0.122, -0.117),
                             c (51.512, 51.512, 51.518, 51.518)))
```

``` r
map <- plot_osm_basemap (bbox=bbox, bg="gray20")
map <- add_osm_groups (map, dat_B, groups=pts, col="orange", bg="gray40", 
                       colmat=FALSE, boundary=1)
col_park_in <- rgb (50, 255, 50, maxColorValue=255)
col_park_out <- rgb (50, 155, 50, maxColorValue=255)
map <- add_osm_groups (map, london$dat_P, groups=pts, col=col_park_in, 
                   bg=col_park_out, colmat=FALSE, boundary=0)
```

``` r
print (map)
```

![map13](map13.png)

Bisection allocates points either to within or beyond a given boundary, with resultant polygons generally separated by a visible gap between locations at which the polygons are defined. Because plotting is progressively overlaid, such gaps can nevertheless be avoided simply by initially plotting underlying layers prior to grouping objects:

``` r
map <- plot_osm_basemap (bbox=bbox, bg="gray20")
map <- add_osm_groups (map, dat_B, groups=pts, col="orange", bg="gray40", 
                   colmat=FALSE, boundary=1)
map <- add_osm_objects (map, london$dat_P, col=col_park_out)
map <- add_osm_groups (map, london$dat_P, groups=pts, col=col_park_in, 
                   bg=col_park_out, colmat=FALSE, boundary=0)
```

``` r
print (map)
```

![map14](map14.png)

### 5.2 Dark-on-Light Highlights

A particularly effective way to highlight particular areas is through using dark colours upon otherwise light coloured maps.

``` r
map <- plot_osm_basemap (bbox=bbox, bg="gray95")
map <- add_osm_groups (map, dat_B, groups=pts, col="gray40", bg="gray85",
                   colmat=FALSE, boundary=1)
map <- add_osm_groups (map, dat_H, groups=pts, col="gray20", bg="gray70",
                   colmat=FALSE, boundary=0)
map <- add_osm_groups (map, dat_HP, groups=pts, col="gray10", bg="white",
                   colmat=FALSE, boundary=0)
```

``` r
print (map)
```

![map15](map15.png)

### 5.3 The Colour Matrix: Colouring Several Regions

Individual studies of particular regions are almost always based on *representative* data sampled at some particular set of points. Presuming such sample points to represent the underlying sample structure, they are commonly subject to clustering analyses of some form or other, resuling in a spatial partition between clusters (whether potentially overlapping or not). In such cases, every location within a given plot will be considered to belong to some particular group(s), yet the plot must distinguish these groups by colour alone. Beyond a handful of groups, manually devising an appropriate colour scheme may become difficult.

`osmplotr` offers a convenient way to allocate systematiclly distinct colours to spatially distinct groups with the `colour_mat` function.

``` r
plot.new ()
cmat <- colour_mat (plot=TRUE)
```

![](making-maps-with-data_files/figure-markdown_github/unnamed-chunk-13-1.png)<!-- -->

``` r
graphics.off ()
```

This function accepts a vector of 4 or more colours assinged to each corner of a rectangular grid of defined size(s). The interior of the grid is then filled through interpolation. The default colours are `rainbow (4)` (or red, green, violet, blue), as illustrated above.

Regional groups may be coloured using `colour_mat` by setting `colmat=TRUE` in `add_osm_groups` and by submitting a desired vector of colours. A `colour_mat` may be illustrated by plotting inclusive groups defined by random points. `add_osm_groups` first discerns which components (polygons or lines) lie *within* groups, then if `col_extra=NULL` (or `NA`), all points are assigned to the nearest group. This nevertheless requires the initial groups to be of non-zero size, and so the following lines initially select random points and then extend them by creating small rectangles around each.

``` r
ngroups <- 12
x <- bbox [1,1] + runif (ngroups) * diff (bbox [1,])
y <- bbox [2,1] + runif (ngroups) * diff (bbox [2,])
groups <- cbind (x, y)
groups <- apply (groups, 1, function (i) 
              sp::SpatialPoints (matrix (i, nrow=1, ncol=2)))
# Then create small rectangles around each pts
groups <- lapply (groups, function (i)
               {
                   x <- sp::coordinates (i) [1] + c (-0.002, 0.002, 0.002,
                                                     -0.002)
                   y <- sp::coordinates (i) [2] + c (-0.002, -0.002, 0.002,
                                                     0.002)
                   sp::SpatialPoints (cbind (x, y))
               })
```

The `ngroups` can then simply be plotted as follows.

``` r
map <- plot_osm_basemap (bbox=bbox, bg="gray20")
map <- add_osm_groups (map, dat_B, groups=groups, make_hull=FALSE,
                   colmat=TRUE, borderWidth=2)
```

``` r
print (map)
```

![map17](map17.png)

The `colour_mat` function has an option to rotate the colour space. This may also be passed directly to `add_osm_groups` enabling, for example, the colours in the previous plot to be rotated by 90 degrees.

``` r
map <- plot_osm_basemap (bbox=bbox, bg="gray20")
map <- add_osm_groups (map, dat_B, groups=groups, make_hull=FALSE,
                   colmat=TRUE, rotate=90, borderWidth=2)
```

``` r
print (map)
```

![map18](map18.png)

6 Bounding areas within named highways
--------------------------------------

The function `connect_highways` takes a list of OSM highway names and a bounding box, and returns the boundary of a polygon encircling the named highways. This can be used to highlight selected regions simply by naming the highways which encircle them, producing maps which look like this:

![map19](map19.png)

Note that the lists of highways were obtained using expanding bounding boxes as described above, and given in the `london` data provided with `osmplotr`. The following plots thus only include restricted portions of these entire regions, while the full polygons surrounding the named areas can be seen by repeating the extraction of data with a larger bounding box:

``` r
bbox <- get_bbox (c(-0.15,51.5,-0.1,51.52)) 
```

### 6.1 The `connect_highways` function

The primary function enabling the delination of groups like the above is `connect_highways`, an example of which is the orange area above, the boundary of which was obtained from:

``` r
highways <- c ("Kingsway", "Holborn", "Farringdon.St", "Strand",
               "Fleet.St", "Aldwych")
highways1 <- connect_highways (highways=highways, bbox=bbox)
```

``` r
## Warning in connect_highways(ways): Cycle unable to be extended through all
## ways
```

The reason for the warning will be explored further below. In the meantime, note that,

``` r
class (highways1)
```

    ## [1] "SpatialPoints"
    ## attr(,"package")
    ## [1] "sp"

``` r
head (sp::coordinates (highways1))
```

    ##              x        y
    ## 474 -0.1050311 51.51721
    ## 147 -0.1050755 51.51722
    ## 146 -0.1052826 51.51729
    ## 145 -0.1054163 51.51734
    ## 144 -0.1055184 51.51737
    ## 143 -0.1057503 51.51745

``` r
dim (sp::coordinates (highways1))
```

    ## [1] 177   2

``` r
highways <- c ("Queen.s.Walk", "Blackfriars", "Waterloo", "The.Cut")
highways2 <- connect_highways (highways=highways, bbox=bbox)
highways <- c ("Regent.St", "Oxford.St", "Shaftesbury")
highways3 <- connect_highways (highways=highways, bbox=bbox)
```

Multiple regions may be highlighted simply by passing a list of bounding polygons (each of class `SpatialPoints`) to `add_osm_groups`.

``` r
groups <- list (highways1, highways2, highways3)
```

Highlighted groups are then added as above using `add_osm_groups`. These are overlaid on top of a basemap. The following maps highlight the selected regions with both buildings and highways, while the remaining structures are plotted on the initial basemap with the following lines. (Note that `make_osm_map` returns `osm_data` with any additional structures not previously present added; in the following case, these data are not altered.)

``` r
structures <- c ("amenity", "grass", "park", "natural")
structs <- osm_structures (structures=structures, col_scheme="dark")
bbox <- get_bbox (c(-0.13,51.5,-0.11,51.52)) # back to smaller bbox
dat <- make_osm_map (bbox=bbox, osm_data=london, structures=structs)
```

``` r
print (dat$map)
```

![map20](map20.png)

The desired areas are then highlighted in the following lines using colours from `RColorBrewer`. These lines also demonstrate how particular colours may be lightened or darkened for use as highlights.

``` r
cols <- c ("tomato", "skyblue", "lawngreen")
cols_dark <- adjust_colours (cols, -0.3)
```

And finally add groups to plot using these colours, resulting in the plot shown at the start of this section.

``` r
map <- add_osm_groups (map, london$dat_BNR, groups=groups, boundary=0,
                   bg=bg_B, colmat=FALSE, col=cols)
map <- add_osm_groups (map, london$dat_BR, groups=groups, boundary=0,
                   bg=bg_B, colmat=FALSE, col=cols)

map <- add_osm_groups (map, london$dat_H, groups=groups, boundary=0,
                   bg=bg_H, colmat=FALSE, col=cols_dark)
map <- add_osm_groups (map, london$dat_HP, groups=groups, boundary=0,
                   bg=bg_H, colmat=FALSE, col=cols_dark)
```

The extraction of bounding polygons from named highways is not failsafe, as demonstrated by the above error message. To understand why it may not work, it is usefull to examine `connect_highways` in more detail, as follows. points.
