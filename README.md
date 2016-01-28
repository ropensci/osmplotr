# urbanplotr

R package to produce visually impressive customisable images of urban areas from
OpenStreetMap data, like this:

![fig](./examples/london.png)

Images are only likely to work well for urban areas with high densities of OSM
structures. OSM data are downloaded from the 
[overpass api](http://overpass-api.de/). There is no internal check for bounding
box sizes, so if they're big, expect downloads to be time-consuming.
