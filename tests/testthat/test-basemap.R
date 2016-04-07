context ("basemap")

test_that ("bbox", {
           expect_error (get_bbox (), "latlon must be supplied to bbox")
           expect_error (get_bbox ("a"), "latlon is not numeric")
           expect_error (get_bbox (1:3), "latlon must have length = 4")
})

test_that ("plot-basemap", {
           expect_error (plot_osm_basemap (), "bbox must be supplied")
           expect_error (plot_osm_basemap (bbox="a"), "bbox is not numeric")
           expect_error (plot_osm_basemap (bbox=1:3), 
                         "bbox must have length = 4")
           bbox <- get_bbox (c (-0.15, 51.5, -0.1, 51.52))
           expect_warning (plot_osm_basemap (bbox=bbox, bg=NULL),
                           "bg will be coerced to character")
           graphics.off ()
})
