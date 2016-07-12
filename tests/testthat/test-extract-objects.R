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

if (curl::has_internet ()) # otherwise all of these return errors not warnings
{
    test_that ('invalid key', {
               bbox <- get_bbox (c (-0.12, 51.51, -0.11, 51.52))
               # No warning specified here because different warnings will be
               # given if download fails
               expect_warning (dat <- extract_osm_objects (bbox=bbox,
                                                           key='aaa'))
               expect_null (dat)
    })

    test_that ('valid key', {
               bbox <- get_bbox (c (-0.12, 51.515, -0.115, 51.52))
               dat <- extract_osm_objects (bbox=bbox, key='building')
               # Will return NULL if any warnings are generated (such as by
               # download fail)
               if (!is.null (dat))
                   expect_is (dat, 'SpatialPolygonsDataFrame')
    })

    test_that ('return_type', {
            key <- 'route'
            value <- 'bicycle'
            extra_pairs <- c ('name', 'London.Cycle.Network.Route.9')
            bbox <- get_bbox (c (-0.26,51.47,0.11,51.67))
            # This can't be run because any downloading issues generate
            # warnings. Only if downloading succeeds will the message be
            # generated ('Cannot determine return_type, maybe specify
            # explicitly?')
            #expect_message (dat <- extract_osm_objects (bbox=bbox, key=key,
            #                                            value=value,
            #                                            extra_pairs=extra_pairs))
    })

} # end if has_internet
