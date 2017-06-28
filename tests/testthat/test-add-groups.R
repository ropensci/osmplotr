context ("add-groups")

test_that ('basemap', {
           expect_error (add_osm_groups (), 'map must be supplied')
           expect_error (add_osm_groups (NULL), 'map must be a ggplot2 object')
           expect_error (add_osm_groups (cols = 1:4), 'map must be supplied')
})

bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
dat_B <- london$dat_BNR
dat_H <- london$dat_H

pts <- cbind (c (-0.115, -0.13, -0.13, -0.115),
              c (51.505, 51.505, 51.515, 51.515))
pts1 <- cbind (c (-0.115, -0.125, -0.125, -0.115),
               c (51.513, 51.513, 51.517, 51.517))
pts2 <- cbind (c (-0.111, -0.1145, -0.1145, -0.111),
               c (51.517, 51.517, 51.519, 51.519))
grps <- list (pts, pts1, pts2)

test_that ('obj', {
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           expect_error (add_osm_groups (map), 'obj must be supplied')
           expect_error (add_osm_groups (map, obj = 1),
                         'obj must be a spatial object')

})

test_that ('cols', {
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           expect_error (add_osm_groups (map, dat_B), "groups must be provided")
           expect_silent (add_osm_groups (map, dat_B, groups = pts, bg = 1))
           expect_message (add_osm_groups (map, dat_B, groups = list (pts),
                                           cols = 1),
                       paste0 ('Plotting one group only makes sense with bg; ',
                               'defaulting to gray40'))
})

test_that ('group errors', {
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           expect_error (add_osm_groups (map, dat_B, bg = 1),
                           "groups must be provided")
           expect_error (add_osm_groups (map, dat_B, NULL),
                         'groups must not be NULL')
           expect_error (add_osm_groups (map, dat_B,
                                         groups = list (dat_H)),
                         'All groups must be numeric')
           grps1 <- list (pts, pts, "a")
           expect_error (add_osm_groups (map, dat_B, groups = grps1),
                         'All groups must be numeric')
})

test_that ('groups with polygons', {
               map <- osm_basemap (bbox = bbox, bg = 'gray20')
               expect_silent (map <- add_osm_groups (map, dat_B,
                                             groups = grps,
                                             bg = 'gray40'))
               #expect_silent (print_osm_map (map))
               #dev.off (which = dev.cur ())

               map <- osm_basemap (bbox = bbox, bg = 'gray20')
               expect_silent (map <- add_osm_groups (map, dat_B,
                                             groups = grps))
})

test_that ('groups with lines', {
               map <- osm_basemap (bbox = bbox, bg = "gray20")

               map <- osm_basemap (bbox = bbox, bg = 'gray20')
               expect_silent (map <- add_osm_groups (map, dat_H,
                                             groups = grps,
                                             bg = 'gray40'))
               #expect_silent (print_osm_map (map))
               #dev.off (which = dev.cur ())

               map <- osm_basemap (bbox = bbox, bg = 'gray20')
               expect_silent (map <- add_osm_groups (map, dat_B,
                                             groups = grps))
})

test_that ('boundary', {
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           expect_silent (map <- add_osm_groups (map, dat_B, grps, 1, 2,
                                                 boundary = NULL))
})

test_that ('make_hull', {
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           expect_warning (map <- add_osm_groups (map, dat_B, pts, 1, bg = 2,
                                                  make_hull = 1:2),
                           'make_hull has length > number of groups')
           expect_warning (add_osm_groups (map, dat_B, grps, 1, bg = 2,
                                           make_hull = 1:2),
                           'make_hull should have length 1')
           expect_silent (map <- add_osm_groups (map, dat_B, grps, 1, bg = 2,
                                                 make_hull = TRUE,
                                                 border_width = 2))
})

test_that ('colourmat', {
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           expect_silent (add_osm_groups (map, dat_B, grps, 1, 2, colmat = 1))
           expect_error (add_osm_groups (map, dat_B, grps, 1, 2, colmat = "a"),
                         'colmat can not be coerced to logical')
           expect_silent (add_osm_groups (map, dat_B, grps, cols = 1:2, 2))
           expect_silent (add_osm_groups (map, dat_B, grps, cols = 1:2, 2,
                                          rotate = "a"))
})

test_that ('rotate', {
           map <- osm_basemap (bbox = bbox, bg = "gray20")
           expect_silent (add_osm_groups (map, dat_B, grps, 1, 2, rotate = 1))
           expect_silent (add_osm_groups (map, dat_B, grps, 1, 2,
                                          rotate = "abc"))
})
