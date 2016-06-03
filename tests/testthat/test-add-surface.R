context ("add-surface")

test_that ('basemap', {
           expect_error (add_osm_surface (), 
                         'map must be supplied to add_osm_surface')
           expect_error (add_osm_surface (NULL), 'map must be a ggplot2 object')
})

test_that ('obj', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox=bbox, bg="gray20")
           expect_error (add_osm_surface (map), 
                         'object must be supplied to add_osm_surface')
           expect_error (add_osm_surface (map, NULL), 
                         'obj must be a spatial object')
})

test_that ('dat', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox=bbox, bg="gray20")
           obj <- london$dat_BNR
           expect_error (add_osm_surface (map, obj), 
                         'dat must be supplied to add_osm_surface')
           expect_error (add_osm_surface (map, obj, NULL), 
                         'dat can not be NULL')
           expect_error (add_osm_surface (map, obj, 1), 
                         'dat must have at least 3 columns')
           dat <- matrix (runif (12), nrow=4)
           expect_warning (add_osm_surface (map, obj, dat), 
                           'dat has no column names')
           colnames (dat) <- c ('a', 'b', 'z')
           expect_warning (add_osm_surface (map, obj, dat), 
                           'dat should have columns of x/y')
           colnames (dat) <- c ('x', 'y', 'a')
           expect_warning (add_osm_surface (map, obj, dat), 
                           'dat should have column named z')

           x <- seq (bbox [1,1], bbox [1,2], length.out=dim (volcano)[1])
           y <- seq (bbox [2,1], bbox [2,2], length.out=dim (volcano)[2])
           xy <- cbind (rep (x, dim (volcano) [2]), rep (y, each=dim (volcano) [1]))
           z <- as.numeric (volcano)
           dat <- data.frame (x=xy [,1], y=xy [,2], z=z)
           cols <- gray (0:50 / 50)
           expect_silent (add_osm_surface (map, obj, dat=dat, cols=cols))
})
