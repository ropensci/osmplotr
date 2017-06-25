
context ('make-osm-map')

test_that ('make_osm_map', {
               dat <- list (dat_BU = london$dat_BNR, dat_A = london$dat_A,
                            dat_G = london$dat_G, dat_N = london$dat_N,
                            dat_P = london$dat_P, dat_H = london$dat_H,
                            dat_T = london$dat_T)
               structures <- osm_structures ()
               indx <- which (paste0 ('dat_', structures$suffix) %in% names (dat))
               structures <- rbind (structures [indx, ], 
                                    structures [structures$structure == 'background', ])

               expect_silent (dat <- make_osm_map (osm_data = dat,
                                                   structures = structures))
})
