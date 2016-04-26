context ('add-axes')

test_that ('basemap object', {
           expect_error (add_axes (), 'map must be supplied to add_axes')
           expect_error (add_axes (NULL), 'map must be a ggplot object')
})

test_that ('alpha values', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- plot_osm_basemap (bbox=bbox, bg="gray20")
           # NOTE: there are grep problems in test_that for this expect_warning
           #expect_warning (add_axes (map, alpha=-1), 
           #                'alpha not in [0,1]; using default of 0.4')
           expect_warning (add_axes (map, alpha=1:2),
                           'Only the first element of alpha will be used')
           expect_warning (add_axes (map, alpha="a"), 
                           'alpha must be numeric; using default value')
})

test_that ('pos', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- plot_osm_basemap (bbox=bbox, bg="gray20")
           expect_warning (add_axes (map, pos=1:3/10),
                           'Only the first two elements of pos will be used')
           expect_warning (add_axes (map, pos="a"),
                           'pos must be numeric; using default values')
           # NOTE: there are grep problems in test_that for this expect_warning
           #expect_warning (add_axes (map, pos=c(-0.1,1.2)),
           #                'pos not in [0,1]; using default values')
})

test_that ('fontsize', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- plot_osm_basemap (bbox=bbox, bg="gray20")
           expect_warning (add_axes (map, fontsize=-1), 
                           'fontsize must be positive; using default value')
           expect_warning (add_axes (map, fontsize=1:2),
                           'Only the first element of fontsize will be used')
           expect_warning (add_axes (map, fontsize="a"), 
                           'fontsize must be numeric; using default value')
})

