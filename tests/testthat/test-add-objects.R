context ("add-objects")

test_that ('basemap', {
           expect_error (add_osm_objects (), 'a non-null map must be provided')
           expect_error (add_osm_objects (NULL), 'map must be a ggplot2 object')
           expect_error (add_osm_objects (col = 1:4),
                         'a non-null map must be provided')
})

test_that ('obj', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           expect_error (add_osm_objects (map), 'obj must be provided')
           expect_error (add_osm_objects (map, NULL),
                         'obj must be a spatial object')
})

test_that ('col', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           obj <- london$dat_BNR
           expect_silent (add_osm_objects (map, obj))
           expect_error (add_osm_objects (map, obj, col = "a"),
                         "Invalid colour: a")
           expect_error (add_osm_objects (map, obj, col = -2),
                         "Invalid colour: -2")
           expect_silent (add_osm_objects (map, obj, col = NA))
           expect_error (add_osm_objects (map, obj, col = NULL),
                         "a non-null col must be provided")
})

test_that ('border', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           obj <- london$dat_BNR
           expect_silent (add_osm_objects (map, obj, border = NULL))
           expect_error (add_osm_objects (map, obj, border = "a"),
                         "Invalid colour: a")
           expect_error (add_osm_objects (map, obj, border = -2),
                         "Invalid colour: -2")
           expect_silent (add_osm_objects (map, obj, border = NA))
})

test_that ('size', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           obj <- london$dat_BNR
           expect_warning (add_osm_objects (map, obj, size = NULL),
                         "size should be numeric; defaulting to 0")
           expect_warning (add_osm_objects (map, obj, size = "a"),
                         "size should be numeric; defaulting to 0")
           expect_warning (add_osm_objects (map, obj, size = -2),
                         "size should be positive; defaulting to 0")
           obj <- london$dat_HP
           expect_warning (add_osm_objects (map, obj, size = NULL),
                         "size should be numeric; defaulting to 0")
           expect_warning (add_osm_objects (map, obj, size = "a"),
                         "size should be numeric; defaulting to 0.5")
           expect_warning (add_osm_objects (map, obj, size = -2),
                         "size should be positive; defaulting to 0.5")
           obj <- london$dat_T
           expect_warning (add_osm_objects (map, obj, size = NULL),
                         "size should be numeric; defaulting to 0")
           expect_warning (add_osm_objects (map, obj, size = "a"),
                         "size should be numeric; defaulting to 0.5")
           expect_warning (add_osm_objects (map, obj, size = -2),
                         "size should be positive; defaulting to 0.5")
})

test_that ('shape', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           obj <- london$dat_BNR # shape is ignored
           expect_silent (add_osm_objects (map, obj, shape = NULL))
           expect_silent (add_osm_objects (map, obj, shape = "a"))
           expect_silent (add_osm_objects (map, obj, shape = -2))
           obj <- london$dat_HP
           expect_warning (add_osm_objects (map, obj, shape = NULL),
                         "shape should be numeric; defaulting to 1")
           expect_warning (add_osm_objects (map, obj, shape = "a"),
                         "shape should be numeric; defaulting to 1")
           expect_warning (add_osm_objects (map, obj, shape = -2),
                         "shape should be positive; defaulting to 1")
           obj <- london$dat_T
           expect_warning (add_osm_objects (map, obj, shape = NULL),
                         "shape should be numeric; defaulting to 19")
           expect_warning (add_osm_objects (map, obj, shape = "a"),
                         "shape should be numeric; defaulting to 19")
           expect_warning (add_osm_objects (map, obj, shape = -2),
                         "shape should be positive; defaulting to 19")
})
