context ("add-surface")

test_that ('basemap', {
           expect_error (add_osm_surface (), 'a non-null map must be provided')
           expect_error (add_osm_surface (NULL), 'map must be a ggplot2 object')
})

test_that ('obj', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           expect_error (add_osm_surface (map), 'obj must be provided')
           expect_error (add_osm_surface (map, NULL),
                         'obj must be a spatial object')
})

test_that ('add surface', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           obj <- london$dat_BNR
           expect_error (add_osm_surface (map, obj), 'dat can not be NULL')
           expect_error (add_osm_surface (map, obj, NULL),
                         "\'data\' must be of a vector type, was \'NULL\'")
           expect_error (add_osm_surface (map, obj, 1),
                         'dat must have at least 3 columns')
           dat <- matrix (runif (12), nrow = 4)
           expect_warning (add_osm_surface (map, obj, dat),
                           'dat has no column names')
           colnames (dat) <- c ('a', 'b', 'z')
           expect_warning (add_osm_surface (map, obj, dat),
                           'dat should have columns of x/y')
           colnames (dat) <- c ('x', 'y', 'a')
           expect_warning (add_osm_surface (map, obj, dat),
                           'dat should have column named z')

           bbdat <- get_bbox (c (-0.128, 51.502, -0.112, 51.518))
           x <- seq (bbdat [1, 1], bbdat [1, 2], length.out = dim (volcano)[1])
           y <- seq (bbdat [2, 1], bbdat [2, 2], length.out = dim (volcano)[2])
           xy <- cbind (rep (x, dim (volcano) [2]),
                        rep (y, each = dim (volcano) [1]))
           z <- as.numeric (volcano)
           dat <- data.frame (x = xy [, 1], y = xy [, 2], z = z)
           cols <- gray (0:50 / 50)

           # polygons---------------------------------
           expect_silent (map <- add_osm_surface (map, obj = london$dat_BNR,
                                                  dat = dat, cols = cols))
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           expect_silent (map <- add_osm_surface (map, obj = london$dat_BNR,
                                                  dat = dat, cols = cols,
                                                  bg = 'orange'))

           # lines------------------------------------
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           expect_silent (map <- add_osm_surface (map, obj = london$dat_H,
                                                  dat = dat, cols = cols))
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           expect_silent (map <- add_osm_surface (map, obj = london$dat_H,
                                                  dat = dat, cols = cols,
                                                  bg = 'orange'))

           # points-----------------------------------
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           expect_silent (map <- add_osm_surface (map, obj = london$dat_T,
                                                  dat = dat,
                                                  cols = cols))
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           expect_silent (map <- add_osm_surface (map, obj = london$dat_T,
                                                  dat = dat,
                                                  cols = cols, bg = 'orange'))
})
