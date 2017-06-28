
context ('make-osm-map')

source ("../stub.R")

test_that ('make_osm_map', {
               dat <- list (dat_BU = london$dat_BNR, dat_A = london$dat_A,
                            dat_P = london$dat_P, dat_H = london$dat_H,
                            dat_T = london$dat_T)
               structures <- osm_structures ()
               indx <- which (paste0 ('dat_',
                                      structures$suffix) %in% names (dat))
               structures <- rbind (structures [indx, ],
                                    structures [structures$structure ==
                                                'background', ])

               expect_silent (dat <- make_osm_map (osm_data = dat,
                                                   structures = structures))
})

test_that ('get_missing_osm_data', {
               structs <- osm_structures ()
               indx <- which (structs$structure == 'building')
               structs <- structs [indx, ]
               bbox <- get_bbox (c(-0.15, 51.5, -0.10, 51.52))
               stub (get_missing_osm_data, 'extract_osm_objects',
                     function (x, ...) london$dat_BNR)
               dat <- get_missing_osm_data (osm_data = list (),
                                            structures = structs, bbox = bbox,
                                            dat_prefix = 'dat_')
               expect_is (dat, 'list')
               expect_identical (names (dat), c ('indx', 'osm_data'))
               expect_is (dat$indx, 'integer')
               expect_is (dat$osm_data, 'list')
               expect_equal (length (dat$osm_data), 1)
               # TODO: Change that when london data are updated to sf
               expect_is (dat$osm_data [[1]], 'sf')
})
