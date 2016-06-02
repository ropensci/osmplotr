context ("add-groups")

test_that ('basemap', {
           expect_error (add_osm_groups (),
                         'map must be supplied to add_osm_groups')
           expect_error (add_osm_groups (NULL),
                         'map must be a ggplot2 object')
           expect_error (capture_warnings (add_osm_groups (cols=1:4)),
                         'map must be supplied to add_osm_groups')
})

test_that ('obj', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox=bbox, bg="gray20")
           expect_error (add_osm_groups (map),
                         'obj must be supplied to add_osm_groups')
           expect_error (add_osm_groups (map, obj=1),
                         'obj must be a spatial object')

})

test_that ('cols', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox=bbox, bg="gray20")
           obj <- london$dat_BNR
           expect_error (capture_warnings (add_osm_groups (map, obj)),
                         "either 'cols' or 'bg' must be minimally given")
           pts <- sp::SpatialPoints (cbind (c (-0.115, -0.13, -0.13, -0.115),
                                            c (51.505, 51.505, 51.515, 51.515)))
           expect_warning (add_osm_groups (map, obj, groups=pts, bg=1),
                           paste0 ('No group colours defined in add_osm_groups: ',
                                   'passing to add_osm_objects'))
           expect_message (add_osm_groups (map, obj, groups=list (pts), cols=1),
                       paste0 ('Plotting one group only makes sense with bg; ',
                               'defaulting to gray40'))
})

test_that ('groups', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox=bbox, bg="gray20")
           obj <- london$dat_BNR
           expect_warning (add_osm_groups (map, obj, bg=1),
                           paste0 ('No groups defined in add_osm_groups: ',
                                   'passing to add_osm_objects'))
           expect_error (capture_warnings (add_osm_groups (map, obj, NULL)),
                         'groups must be a SpatialPoints object')
           expect_error (add_osm_groups (map, obj, groups=list (london$dat_HP)),
                         'Cannot coerce groups to SpatialPoints')
           pts <- sp::SpatialPoints (cbind (c (-0.115, -0.13, -0.13, -0.115),
                                            c (51.505, 51.505, 51.515, 51.515)))
           grps <- list (pts, pts, "a")
           expect_error (add_osm_groups (map, obj, groups=grps),
                         'Cannot coerce groups to SpatialPoints')
})

test_that ('boundary', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox=bbox, bg="gray20")
           obj <- london$dat_BNR
           pts <- sp::SpatialPoints (cbind (c (-0.115, -0.13, -0.13, -0.115),
                                            c (51.505, 51.505, 51.515, 51.515)))
           grps <- list (pts, pts, pts)
           expect_silent (add_osm_groups (map, obj, grps, 1, 2, boundary=NULL))
})

test_that ('make_hull', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox=bbox, bg="gray20")
           obj <- london$dat_BNR
           pts <- sp::SpatialPoints (cbind (c (-0.115, -0.13, -0.13, -0.115),
                                            c (51.505, 51.505, 51.515, 51.515)))
           expect_warning (add_osm_groups 
                           (map, obj, pts, 1, bg=2, make_hull = 1:2),
                           'make_hull has length > number of groups')
           grps <- list (pts, pts, pts)
           expect_warning (add_osm_groups 
                           (map, obj, grps, 1, bg=2, make_hull = 1:2),
                           'make_hull should have length 1')
})

test_that ('colourmat', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox=bbox, bg="gray20")
           obj <- london$dat_BNR
           pts <- sp::SpatialPoints (cbind (c (-0.115, -0.13, -0.13, -0.115),
                                            c (51.505, 51.505, 51.515, 51.515)))
           grps <- list (pts, pts, pts)
           expect_silent (add_osm_groups (map, obj, grps, 1, 2, colmat=1))
           expect_silent (add_osm_groups (map, obj, grps, 1, 2, colmat="abc"))
           expect_silent (add_osm_groups (map, obj, grps, cols=1:2, 2))
           expect_silent (add_osm_groups (map, obj, grps, cols=1:2, 2,
                                          rotate="a"))
})

test_that ('rotate', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           map <- osm_basemap (bbox=bbox, bg="gray20")
           obj <- london$dat_BNR
           pts <- sp::SpatialPoints (cbind (c (-0.115, -0.13, -0.13, -0.115),
                                            c (51.505, 51.505, 51.515, 51.515)))
           grps <- list (pts, pts, pts)
           expect_silent (add_osm_groups (map, obj, grps, 1, 2, rotate=1))
           expect_silent (add_osm_groups (map, obj, grps, 1, 2, rotate="abc"))
})
