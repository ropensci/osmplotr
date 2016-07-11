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
           if (curl::has_internet ()) # otherwise it'll be an error
           {
               # No warning specified here because different warnings will be
               # given if download fails
               expect_warning (dat <- extract_osm_objects (bbox=bbox,
                                                           key='aaa'))
           }
})

test_that ('valid key', {
           bbox <- get_bbox (c (-0.12, 51.515, -0.115, 51.52))
           if (curl::has_internet ()) # otherwise it'll be an error
           {
               dat <- extract_osm_objects (bbox=bbox, key='building')
               # Will return NULL if any warnings are generated
               if (!is.null (dat))
                   expect_is (dat, 'SpatialPolygonsDataFrame')
           }
})

