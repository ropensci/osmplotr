context ("add-surface")

test_that ('basemap', {
           expect_error (add_osm_surface (), 
                         'map must be supplied to add_osm_objects')
           expect_error (add_osm_surface (NULL), 'map must be a ggplot object')
})

test_that ('obj', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- plot_osm_basemap (bbox=bbox, bg="gray20")
           expect_error (add_osm_surface (map), 
                         'object must be supplied to add_osm_objects')
           expect_error (add_osm_surface (map, NULL), 'obj must be Spatial')
})
