context ('extract-objects')

test_that ('missing objects', {
           expect_error (extract_osm_objects (), 'key must be provided')
           expect_error (extract_osm_objects ('aaa'), 'bbox must be provided')
})

test_that ('key missing', {
           bbox <- get_bbox (c (-0.12, 51.51, -0.11, 51.52))
           expect_error (extract_osm_objects (bbox=bbox, key=''), 
                         'key must be provided')
           expect_error (extract_osm_objects (bbox=bbox), 'key must be provided')
})

test_that ('invalid key', {
           bbox <- get_bbox (c (-0.12, 51.51, -0.11, 51.52))
           expect_warning (dat <- extract_osm_objects (bbox=bbox, key='aaa'),
                           'No valid data for')
})

test_that ('valid key', {
           bbox <- get_bbox (c (-0.12, 51.51, -0.11, 51.52))
           expect_silent (dat <- extract_osm_objects (bbox=bbox,
                                                      key='building'))
           expect_is (dat, 'SpatialPolygonsDataFrame')
})

