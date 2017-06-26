context ('print-osm-map')

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("TRAVIS"), "true"))

test_that ('print_osm_map', {
               dat_B <- london$dat_BNR
               bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
               map <- osm_basemap (bbox = bbox, bg = "gray20")
               map <- add_osm_objects (map, dat_B)

               exts <- c ('eps', 'ps', 'tex', 'pdf', 'svg', 'png',
                          'jpg', 'jpeg', 'bmp', 'tiff')
               #for (e in exts)
               #{
               #    fname <- paste0 ('map.', e)
               #    expect_silent (print_osm_map (map, width = 5, height = 4,
               #                                  filename = fname,
               #                                  units = "in"))
               #    expect_true (fname %in% list.files ())
               #}
               expect_silent (print_osm_map (map, width = 5, height = 4,
                                             filename = "map.eps",
                                             units = "in"))
               expect_silent (print_osm_map (map, width = 5, height = 4,
                                             filename = "map.ps",
                                             units = "in"))
               expect_silent (print_osm_map (map, width = 5, height = 4,
                                             filename = "map.tex",
                                             units = "in"))
               expect_silent (print_osm_map (map, width = 5, height = 4,
                                             filename = "map.pdf",
                                             units = "in"))
               expect_silent (print_osm_map (map, width = 5, height = 4,
                                             filename = "map.svg",
                                             units = "in"))
               expect_silent (print_osm_map (map, width = 5, height = 4,
                                             filename = "map.png",
                                             units = "in"))
               expect_silent (print_osm_map (map, width = 5, height = 4,
                                             filename = "map.jpg",
                                             units = "in"))
               expect_silent (print_osm_map (map, width = 5, height = 4,
                                             filename = "map.jpeg",
                                             units = "in"))
               expect_silent (print_osm_map (map, width = 5, height = 4,
                                             filename = "map.bmp",
                                             units = "in"))
               expect_silent (print_osm_map (map, width = 5, height = 4,
                                             filename = "map.tiff",
                                             units = "in"))
               #if (test_all)
               #{
               #    for (e in exts)
               #        file.remove (paste0 ('map.', e))
               #}
})
