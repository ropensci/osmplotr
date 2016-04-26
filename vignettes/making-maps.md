`osmplotr` enables OpenStreetMap (OSM) data to be downloaded (using the [overpass API](https://overpass-api.de)) and used to produce highly customisable maps. This vignette demonstrates both data downloading and the creation of simple maps. The subsequent vignette ('making-maps-with-data') demonstrates how `osmplotr` enables user-defined data to be visualised using OSM data. The maps in this vignette represent a small portion of central London, U.K.

Contents
--------

[1. Introduction](#1%20intro)

[2. Downloading Data](#2%20downloading)

[    2.1 Negation](#2.1%20negation)

[    2.2 Additional `key-value` pairs](#2.2%20keyval-pairs)

[    2.3 A note on rivers](#2.3%20rivers)

[    2.4 Downloading with `osm_structures` and `make_osm_map`](#2.4%20downloading2)

[          2.4.1 The `london` data of `osmplotr`](#2.4.1%20london-data)

[    2.5 Downloading connected highways](#2.5%20highways)

[          2.5.1 `connect_highways` in detail](#2.5.1%20highways-detail)

[3. Producing maps](#3%20maps)

[    3.1 Plotting different OSM Structures](#3.1%20osm-structures)

[    3.2. Automating map production](#3.2%20make-osm-map)

[    3.3 Axes](#3.3%20axes)

<a name="1 intro"></a>1. Introduction
-------------------------------------

A map can be generated using the following simple steps:

``` r
library (osmplotr)
library (maptools) # Needed for this vignette
```

1.  Specify the bounding box for the desired region

``` r
bbox <- get_bbox (c(-0.13,51.50,-0.11,51.52))
```

1.  Download the desired data---in this case, all building perimeters.

``` r
dat_B <- extract_osm_objects (key='building', bbox=bbox)
```

1.  Initiate an `osm_basemap` with desired background (`bg`) colour

``` r
map <- plot_osm_basemap (bbox=bbox, bg='gray20')
```

1.  Add desired plotting objects in the desired colour.

``` r
map <- add_osm_objects (map, dat_B, col='gray40')
```

1.  Print the map

``` r
print (map)
```

![map1](map_a1.png)

Additional capabilities of `osmplotr` are described in the following sections, beginning with downloading and extraction of data.

<a name="2 downloading"></a>2. Downloading Data
-----------------------------------------------

The main function for downloading OSM data from the [overpass API](https://overpass-api.de) is `extract_osm_objects`. Data of a particular type can be extracted by passing the appropriate OSM `key`, as in the above example:

``` r
bbox <- get_bbox (c(-0.13,51.51,-0.11,51.52))
dat_B <- extract_osm_objects (key='building', bbox=bbox)
dat_H <- extract_osm_objects (key='highway', bbox=bbox)
```

These objects are of appropriate `Spatial` classes:

``` r
class (dat_B); class (dat_H); class (dat_T)
```

    ## [1] "SpatialPolygonsDataFrame"
    ## attr(,"package")
    ## [1] "sp"

    ## [1] "SpatialLinesDataFrame"
    ## attr(,"package")
    ## [1] "sp"

    ## [1] "SpatialPointsDataFrame"
    ## attr(,"package")
    ## [1] "sp"

The `SpatialPolygonsDataFrame`, `SpatialLinesDataFrame`, and `SpatialPointsDataFrame` of London buildings, highways, and trees respectively contain

``` r
length (dat_B); length (dat_H); length (dat_T)
```

    ## [1] 2178

    ## [1] 2139

    ## [1] 1310

... 2,178 building polygons, 2,139 highway lines, and 1,310 trees. `extract_osm_objects` also accepts `key-value` pairs which are passed to the [overpass API](https://overpass-api.de) :

``` r
dat_T <- extract_osm_objects (key='natural', value='tree', bbox=bbox)
```

<a name="2.1 negation"></a>2.1 Negation
---------------------------------------

Negation can be specified by pre-pending `!` to the `value` argument so that, for example, all `natural` objects that are **not** trees can be extracted with

``` r
dat_NT <- extract_osm_objects (key='natural', value='!tree', bbox=bbox)
```

`london$dat_H` contains all non-primary highways, and was extracted with,

``` r
dat_H <- extract_osm_objects (key='highway', value='!primary', bbox=bbox)
```

<a name="2.2 keyval-pairs"></a>2.2 Additional `key-value` pairs
---------------------------------------------------------------

Any number of `key-value` pairs may be passed to `extract_osm_objects`. For example, a named building can be extracted with

``` r
extra_pairs <- c ('name', 'Royal.Festival.Hall')
dat <- extract_osm_objects (key='building', extra_pairs=extra_pairs, 
                                       bbox=bbox)
```

This data is stored in `london$dat_RFH`. Note that periods or dots are used for whitespace, and in fact symbolise (in `grep` terms) any character whatsoever. The polygon of a building at a particular street address can be extracted with

``` r
extra_pairs <- list (c ('addr:street', 'Stamford.St'),
                     c ('addr:housenumber', '150'))
dat <- extract_osm_objects (key='building', extra_pairs=extra_pairs, 
                                      bbox=bbox)
```

This data is stored as `london$dat_ST`. Note that addresses generally require combining both `addr:street` with `addr:housenumber`.

<a name="2.3 rivers"></a>2.3 A note on rivers
---------------------------------------------

OSM objects are extracted within `R` through using the `osmar` package which does not currently handle the extraction of rivers in a failsafe way. Rivers are generally defined by `(key,value)=('waterway','riverbank')`, with relations returned as an OSM `multipolygon`. These `multipolygon` objects are, however, **not** extracted by `osmar` when they extend beyond a requested `bbox`, and there is no way to know in advance whether or not that may be the case. This requests to download river polygons may not yield the desired results. This problem will be addressed in future releases of `osmplotr`.

<a name="2.4 downloading2"></a>2.4 Downloading with `osm_structures` and `make_osm_map`
---------------------------------------------------------------------------------------

The functions `osm_structures` and `make_osm_map` aid both downloading multiple OSM data types and plotting (with the latter described below). `osm_structures` returns a `data.frame` of OSM structure types, associated `key-value` pairs, unique suffices which may be appended to data structures for storage purposes, and suggested colours. Passing this list to `make_osm_map` will return a list of the requested OSM data items, named through combining the `dat_prefix` specified in `make_osm_map` and the suffices specified in `osm_structures`.

``` r
osm_structures ()
```

    ##     structure      key value suffix      cols
    ## 1    building building           BU #646464FF
    ## 2     amenity  amenity            A #787878FF
    ## 3    waterway waterway            W #646478FF
    ## 4       grass  landuse grass      G #64A064FF
    ## 5     natural  natural            N #647864FF
    ## 6        park  leisure  park      P #647864FF
    ## 7     highway  highway            H #000000FF
    ## 8    boundary boundary           BO #C8C8C8FF
    ## 9        tree  natural  tree      T #64A064FF
    ## 10 background                          gray20

Many structures are identified by keys only, in which cases the values are empty strings.

``` r
osm_structures()$value [1:4]
```

    ## [1] ""      ""      ""      "grass"

The last row of `osm_structures` exists only to define the background colour of the map, as explained below (**3.2 Automating map production**).

The suffices include as many letters as are necessary to represent all unique structure names. `make_osm_map` returns a list of two components:

1.  `osm_data` containing the data objects passed in the `osm_structures` argument. Any existing `osm_data` may also be submitted to `make_osm_map`, in which case any objects not present in the submitted data will be appended to the returned version. If `osm_data` is not submitted, all objects in `osm_structures` will be downloaded and returned.
2.  `map` containing the `ggplot2` map objects with layers overlaid according to the sequence and colour schemes specified in `osm_structures`

The data specified in `osm_structures` can then be downloaded simply by calling:

``` r
dat <- make_osm_map (structures=osm_structures (), bbox=bbox)
```

``` r
names (dat); sapply (dat, class); names (dat$osm_data)
```

    ## [1] "osm_data" "map"

    ## $osm_data
    ## [1] "list"
    ## 
    ## $map
    ## [1] "gg"     "ggplot"

    ## [1] "dat_BU" "dat_A"  "dat_W"  "dat_G"  "dat_N"  "dat_P"  "dat_H"  "dat_BO"
    ## [9] "dat_T"

Where `dat$osm_data` contains the requested data. A list of desired structures can also be passed to this function, for example,

``` r
osm_structures (structures=c('building', 'highway'))
```

    ##    structure      key value suffix      cols
    ## 1   building building            B #646464FF
    ## 2    highway  highway            H #000000FF
    ## 3 background                          gray20

Passing this to `make_osm_map` will download only these two structures. Finally, note that the example of,

``` r
osm_structures (structures='grass')
```

    ##    structure     key value suffix      cols
    ## 1      grass landuse grass      G #64A064FF
    ## 2 background                         gray20

demonstrates the conversion of `keys` to OSM-appropriate `key-value` pairs.

### <a name="2.4.1 london-data"></a>2.4.1 The `london` data of `osmplotr`

To illustrate the use of `osm_structures` to download data, this section reproduces the code that was used to generate the `london` data object which forms part of the `osmplotr` package.

``` r
structures <- c ('highway', 'highway', 'building', 'building', 'building',
                 'amenity', 'park', 'natural', 'tree')   
structs <- osm_structures (structures=structures, col_scheme='dark')   
structs$value [1] <- '!primary'   
structs$value [2] <- 'primary'
structs$suffix [2] <- 'HP'
structs$value [3] <- '!residential'
structs$value [4] <- 'residential'
structs$value [5] <- 'commercial'
structs$suffix [3] <- 'BNR'
structs$suffix [4] <- 'BR'
structs$suffix [5] <- 'BC'
```

Note that suffices are generated automatically from structure names only, not values, requiring the suffices for negated forms to be specified manually. The `london` data was then simply downloaded by calling `extract_osm_objects` on each row of the `structs` data frame. The additional lines in the following code rename each downloaded object according to the suffices given in `structs$suffix`.

``` r
london <- make_osm_map (structures=structs, bbox=bbox)
london <- london$osm_data
```

As shown above, the requested data are contained in the `$osm_data` list item. The `map` item is examined below (see **3.1 Automating map production**).

<a name="2.5 highways"></a>2.5 Downloading connected highways
-------------------------------------------------------------

The visualisation functions considered below enable particular regions of maps to be highlighted. While it may often be desirable to highlight regions according to a user's own data, `osmplotr` also enables regions to be defined by providing a list of the names of encircling highways. The function which achieves this is `connect_highways`, which returns a sequential list of `SpatialPoints` from those segments of the named highways which connected continuously and sequentially to form a single enclosed space. An example is,

``` r
highways <- c ('Monmouth.St', 'Short.?s.Gardens', 'Endell.St', 'Long.Acre',
               'Upper.Saint.Martin')
highways1 <- connect_highways (highways=highways, bbox=bbox)
```

Note the use of the 'regex' character '?' which declares that the previous character is optional. This matches both "Shorts Gardens" and "Short's Gardens", both of which appear in OSM data.

``` r
class (highways1); length (highways1); head (coordinates (highways1))
```

    ## [1] "SpatialPoints"
    ## attr(,"package")
    ## [1] "sp"

    ## [1] 55

    ##     coords.x1 coords.x2
    ## 14 -0.1250003  51.51480
    ## 15 -0.1254141  51.51457
    ## 16 -0.1257428  51.51440
    ## 17 -0.1259169  51.51430
    ## 18 -0.1260387  51.51424
    ## 19 -0.1269318  51.51381

Other examples, which are also included in the provided `london` data, include:

``` r
highways <- c ('Endell.St', 'High.Holborn', 'Drury.Lane', 'Long.Acre')
highways2 <- connect_highways (highways=highways, bbox=bbox)
highways <- c ('Drury.Lane', 'High.Holborn', 'Kingsway', 'Great.Queen.St')
highways3 <- connect_highways (highways=highways, bbox=bbox)
```

The extraction of bounding polygons from named highways is not failsafe, and may generate various warning messages. To understand the kinds of conditions under which it may not work, it is useful to examine `connect_highways` in more detail.

### <a name="2.5.1 highways-detail"></a>2.5.1 `connect_highways` in detail

`connect_highways` finds a sequence of line segments that circularly connect the named highways. Cases where no circular connection is possible generate an error message. The function which actually connects the given highways into a circular sequence is `get_highway_cycle`, which,

> Takes a list of OpenStreetMap highways returned by `extract_highways` and sequentially connects closest nodes of adjacent highways until the set of highways connects to form a cycle.

`connect_highways` proceeds through the three stages of,

1.  Adding intersection nodes to junctions of ways where these don't already exist

2.  Filling a connectivity matrix between the listed highways and extracting the **longest** cycle connecting them all

3.  Inserting extra connections between highways until the length of the longest cycle is equal to `length (highways)`.

However, even once the highways are connected, the individual components of each highway may not necessarily connect in a continuous manner to complete the cycle. The final task, completed within the `connect_highways` routine, is thus ensuring that the components of each individual highway actually connect, through sequentially connecting the closest pair of components until a shortest path is possible between the two components which connect with other highways.

This procedure can not be guaranteed failsafe owing both to the inherently unpredictable nature of OpenStreetMap, as well as to the unknown relationships between named highways. To enable problematic cases to be examined and hopefully resolved, `connect_highways` has a `plot` option:

``` r
# TODO: Set eval=FALSE before cran re-sub!!
bbox_big <- get_bbox (c(-0.15,51.5,-0.10,51.52))
highways <- c ('Kingsway', 'Holborn', 'Farringdon.St', 'Strand',
               'Fleet.St', 'Aldwych')
highway_list <- connect_highways (highways=highways, bbox=bbox_big, plot=TRUE)
```

    ## Downloading OSM data ...
    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===========                                                      |  17%
      |                                                                       
      |======================                                           |  33%
      |                                                                       
      |================================                                 |  50%
      |                                                                       
      |===========================================                      |  67%
      |                                                                       
      |======================================================           |  83%
      |                                                                       
      |=================================================================| 100%

    ## Warning in get_highway_cycle(ways): Cycle unable to be extended through all
    ## ways

![](making-maps_files/figure-markdown_github/connect_highways-1.png)<!-- -->

The plot depicts each highway in a different colour, along with numbers at start and end points of each segment. This plot reveals in this case that highway\#6 ('Aldwych') is actually nested within two components of highway\#4 ('Strand'). `connect_highways` searches for the shortest path connecting all named highways, and since 'Strand' connects to both highways\#1 and \#5, the shortest path excludes \#6. This exclusion of one of the named components generates the warning message.

<a name="3 maps"></a>3. Producing maps
--------------------------------------

Maps will generally contain multiple kinds of OSM data, for example,

``` r
dat_B <- extract_osm_objects (key='building', bbox=bbox)
dat_H <- extract_osm_objects (key='highway', bbox=bbox)
dat_T <- extract_osm_objects (key='natural', value='tree', bbox=bbox)
```

As illustrated above, plotting maps requires first making a basemap with a specified background colour. Portions of maps can also be plotted by creating a `basemap` with a smaller bounding box.

``` r
bbox_small <- get_bbox (c(-0.13,51.51,-0.11,51.52))
map <- plot_osm_basemap (bbox=bbox_small, bg='gray20')
map <- add_osm_objects (map, dat_H, col='gray70')
map <- add_osm_objects (map, dat_B, col='gray40')
```

The proportions of graphics devices should be scaled in proportion to the bounding box (although this is of course not necessary).

``` r
dev.new (width=8, height=8 * diff (bbox2 [2,]) / diff (bbox2 [1,]))
# or png, pdf, or whatever device is used for printing
print (map)
```

![map2](map_a2.png)

Other graphical parameters can also be passed to `add_osm_objects`, such as border colours or line widths and types. For example,

``` r
map <- plot_osm_basemap (bbox=bbox_small, bg='gray20')
map <- add_osm_objects (map, dat_B, col='gray40', border='orange', size=0.2)
print (map)
```

![map3](map_a3.png)

The `size` argument is passed to the corresponding `ggplot2` routine for plotting polygons, lines, or points, and respectively determines widths of lines (for polygon outlines and for lines), and sizes of points. The `col` argument determines the fill colour of polygons, or the colour of lines or points.

``` r
map <- add_osm_objects (map, dat_H, col='gray70', size=0.7)
map <- add_osm_objects (map, dat_T, col='green', size=2, shape=1)
print (map)
```

![map4](map_a4.png)

Note also that the `shape` parameter determines the point shape, for details of which see `?ggplot2::shape`. Also note that plot order affects the final outcome, because components are sequentially overlaid and thus the same map components plotted in a different order will generally produce a different result.

The `osmplotr` package is intended to produce high quality graphical output written to particular graphic devices such as `png` or `jpeg` (see `?png` for a list of possible devices). `ggplot` readily enables map objects to be printed to the active device so that graphics files can be generated by, for example,

``` r
png (height=mapht, width=mapwd, file='map.png')
print (map)
graphics.off ()
```

<a name="3.1 osm-structures"></a>3.1 Plotting different OSM Structures
----------------------------------------------------------------------

The ability demonstrated above to use negation in `extract-osm-objects` allows different kinds of the same object to be visually contrasted, for example primary and non-primary highways:

``` r
dat_HP <- extract_osm_objects (key='highway', value='primary', bbox=bbox)
dat_H <- extract_osm_objects (key='highway', value='!primary', bbox=bbox)
```

``` r
map <- plot_osm_basemap (bbox=bbox_small, bg='gray20')
map <- add_osm_objects (map, dat_H, col='gray50')
map <- add_osm_objects (map, dat_HP, col='gray80', size=2)
print (map)
```

![map5](map_a5.png)

The additional `key-value` pairs demonstrated above (for Royal Festival Hall, `dat_RFH` and 150 Stamford Street, `dat_ST`) also above allow for highly customised maps.

These objects can then be individually plotted with different colour schemes.

``` r
bbox_small2 <- get_bbox (c (-0.118, 51.504, -0.110, 51.507))
map <- plot_osm_basemap (bbox=bbox_small2, bg='gray95')
map <- add_osm_objects (map, dat_H, col='gray80')
map <- add_osm_objects (map, dat_HP, col='gray60', size=2)
map <- add_osm_objects (map, dat_RFH, col='orange', border='red', size=2)
map <- add_osm_objects (map, dat_ST, col='skyblue', border='blue', size=2)
dev.new (width=8, height=8 * diff (bbox_small2 [2,]) / diff (bbox_small2 [1,]))
print (map)
```

![map7](map_a7.png)

<a name="3.2 make-osm-map"></a>3.2. Automating map production
-------------------------------------------------------------

As indicated above (**2.4 Downloading with `osm_structures` and `make_osm_map`**), the production of maps overlaying various type of OSM objects is facilitated with `make_osm_map`. The structure of a map is defined by `osm_structures` as described above.

Producing a map with customised data is as simple as,

``` r
structs <- c ('highway', 'building', 'park', 'grass', 'tree')   
structures <- osm_structures (structures=structs, col_scheme='light')   
dat <- make_osm_map (structures=structures, bbox=bbox)
map <- dat$map
dev.new (width=8, height=8 * diff (bbox [2,]) / diff (bbox [1,]))
print (map)
```

![map8](map_a8.png)

Because no bounding box was passed to `make_osm_map`, a bounding box is extracted as the **largest** box spanning all objects in `osm_data`. These objects include highways which extend beyond the defining bounding box, and the large park in the south east. Passing the previous bounding box to the same call, and using the data downloaded from that call to avoid repeating the download, gives:

``` r
dat <- make_osm_map (osm_data=dat$osm_data, structures=structures,
                     bbox=bbox_small)
print (dat$map)
```

![map9](map_a9.png)

Objects in maps are overlaid on the plot according to the order of rows in `osm_structures`, with the single exception that `background` is plotted first. This order can be readily changed or restricted simply by submitting structures in a desired order.

``` r
structs <- c ('amenity', 'building', 'grass', 'highway', 'park')
osm_structures (structs, col_scheme='light')
```

    ##    structure      key value suffix      cols
    ## 1    amenity  amenity            A #DCDCDCFF
    ## 2   building building            B #C8C8C8FF
    ## 3      grass  landuse grass      G #C8FFC8FF
    ## 4    highway  highway            H #969696FF
    ## 5       park  leisure  park      P #C8DCC8FF
    ## 6 background                          gray95

As described above (**2.4 Downloading with `osm_structures` and `make_osm_map`**), if existing `osm_data` are not passed to `make_osm_map`, then all data will be downloading by that function and returned in the `$osm_data` list component.

<a name="3.3 axes"></a>3.3 Axes
-------------------------------

Axes may be added to maps using the `add_axes` function. In contrast to many `R` packages for producing maps, maps in `osmplotr` fill the entire plotting space, and axes are added *internal* to this space. The separate function for adding axes allows them to be overlaid on top of all previous layers.

Axes added to a dark version of the previous map look like this:

``` r
structures <- osm_structures (structures=structs, col_scheme='dark')   
dat <- make_osm_map (structures=structures, osm_data=dat$osm_dat, bbox=bbox)
map <- add_axes (dat$map, colour='black')
```

Note that, as described above, `make_osm_map` returns a list of two items: (i) potentially modified data (in `$osm_data`) and (ii) the map object (in `$map`). All other `add_` functions take a map object as one argument and return the single value of the modified map object.

``` r
print (map)
```

![map10](map_a10.png)

This map reveals that the axes and labels are printed above semi-transparent background rectangles, with transparency controlled by the `alpha` parameter. Axes are always plotted on the left and lower side, but positions can be adjusted with the `pos` parameter which specifies the, &gt; Positions of axes and labels relative to entire plot device

``` r
map <- add_axes (map, colour='blue', pos=c(0.1,0.2))
print (map)
```

![map11](map_a11.png)

The second call to `add_axes` overlaid additional axes on a map that already had axes from the previous call. The current version of `osmplotr` does not allow text labels of axes to be rotated. (This is because the semi-transparent underlays are generated with `ggplot2::geom_label` which currently prevents rotation.)
