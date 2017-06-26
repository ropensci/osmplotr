context ('add-colourbar')

test_that ('basemap object', {
           expect_error (add_colourbar (), 'map must be supplied')
           expect_error (add_colourbar (NULL), 'cols must be specified')
})

test_that ('colours', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           cols <- heat.colors (10)
           zlims <- c (1, 10)
           expect_error (add_colourbar (map),
                         'cols must be specified in add_colourbar')
           expect_silent (add_colourbar (map, colors = cols, zlims = zlims))
           expect_silent (add_colourbar (map, colours = cols, zlims = zlims))
})

test_that ('colourbar width', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           cols <- heat.colors (10)
           zlims <- c (1, 10)
           expect_warning (add_colourbar (map, cols = cols, zlims = zlims,
                                          barwidth = 1:3),
                       'Only the first two elements of barwidth will be used')
           expect_warning (add_colourbar (map, cols = cols, zlims = zlims,
                                          barwidth = "a"),
                           'barwidth must be numeric; using default value')
           # NOTE: there are grep problems in test_that for this expect_warning
           #expect_warning (add_colourbar (map, cols = cols, zlims = zlims,
           #                               barwidth = -0.1),
           #        'barwidth values not in [0,1]; default values will be used')
           expect_silent (add_colourbar (map, cols = cols, zlims = zlims,
                                         width = 0.1))
})

test_that ('colourbar length', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           cols <- heat.colors (10)
           zlims <- c (1, 10)
           expect_warning (add_colourbar (map, cols = cols, zlims = zlims,
                                          barlength = 1:3 / 10),
                       'Only the first two elements of barlength will be used')
           expect_warning (add_colourbar (map, cols = cols, zlims = zlims,
                                          barlength = "a"),
                           'barlength must be numeric; using default value')
           # NOTE: there are grep problems in test_that for this expect_warning
           #expect_warning (add_colourbar (map, cols = cols, zlims = zlims,
           #                               barlength = -0.1),
           #    'barlength values not in [0,1]; default values will be used')
           expect_silent (add_colourbar (map, cols = cols, zlims = zlims,
                                         length = 0.1))
})

test_that ('fontsize', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           cols <- heat.colors (10)
           zlims <- c (1, 10)
           expect_warning (add_colourbar (map, cols = cols, zlims = zlims,
                                          fontsize = -1),
                           'fontsize must be positive; using default value')
           expect_warning (add_colourbar (map, cols = cols, zlims = zlims,
                                          fontsize = 1:2),
                           'Only the first element of fontsize will be used')
           expect_warning (add_colourbar (map, cols = cols, zlims = zlims,
                                          fontsize = "a"),
                           'fontsize must be numeric; using default value')
           expect_silent (add_colourbar (map, cols = cols, zlims = zlims,
                                         size = 1))
})

test_that ('vertical', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           cols <- heat.colors (10)
           zlims <- c (1, 10)
           expect_warning (add_colourbar (map, cols = cols, zlims = zlims,
                                          vertical = 1:4),
                           'Only the first element of vertical will be used')
           expect_warning (add_colourbar (map, cols = cols, zlims = zlims,
                                          vertical = "a"),
                           'vertical must be logical; using default')
           expect_silent (add_colourbar (map, cols = cols, zlims = zlims,
                                          vertical = TRUE))
           expect_silent (add_colourbar (map, cols = cols, zlims = zlims,
                                          vertical = FALSE))
})

test_that ('alpha values', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           cols <- heat.colors (10)
           zlims <- c (1, 10)
           # NOTE: there are grep problems in test_that for this expect_warning
           #expect_warning (add_colourbar (map, cols = cols, zlims = zlims,
           #                               alpha = -1),
           #                'alpha not in [0,1]; using default value')
           expect_warning (add_colourbar (map, cols = cols, zlims = zlims,
                                          alpha = 1:2),
                           'Only the first element of alpha will be used')
           expect_warning (add_colourbar (map, cols = cols, zlims = zlims,
                                          alpha = "a"),
                           'alpha must be numeric; using default value')
})

test_that ('other_font_properties', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           cols <- heat.colors (10)
           zlims <- c (1, 10)
           expect_silent (add_colourbar (map, cols = cols, zlims = zlims,
                                         face = 1))
           expect_silent (add_colourbar (map, cols = cols, zlims = zlims,
                                         family = 1))
})
