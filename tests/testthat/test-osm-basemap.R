context ('osm-basemap')

test_that ('bbox', {
           expect_error (osm_basemap (), 'bbox must be supplied')
           expect_error (osm_basemap (-1), 'bbox must have length = 4')
           expect_error (osm_basemap ('a'), 'bbox is not numeric')
           expect_error (osm_basemap (NULL), 'bbox is not numeric')
           expect_error (osm_basemap (NA), 'bbox is not numeric')
           expect_error (osm_basemap (c (1:3, 'a')), 'bbox is not numeric')
           expect_warning (osm_basemap (1:5), 'bbox has length > 4')
})

test_that ('structures', {
           bb <- get_bbox (1:4)
           expect_error (osm_basemap (bb, structures=NA), 
                         'structures must be a data frame')
           s <- osm_structures ()
           names (s) [1] <- 'x'
           expect_error (osm_basemap (get_bbox (1:4), structures=s),
                         'structures not in recognised format')
})

test_that ('bg', {
           bb <- get_bbox (1:4)
           expect_error (osm_basemap (bb, bg='a'), 'Invalid bg: a')
           expect_error (osm_basemap (bb, bg=NA), 'Invalid bg')
           expect_error (osm_basemap (bb, bg=NULL), 'Invalid bg')
           expect_warning (osm_basemap (bb, bg=1:2), 'bg has length > 1')
})
