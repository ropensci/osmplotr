context ('print-osm-map')

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("TRAVIS"), "true"))

test_that ('print_osm_map', {
               dat_B <- london$dat_BNR
               bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
               map <- osm_basemap (bbox = bbox, bg = "gray20")
               map <- add_osm_objects (map, dat_B)

               #exts <- c ('eps', 'ps', 'tex', 'pdf', 'svg', 'png',
               #           'jpg', 'jpeg', 'bmp', 'tiff')
               # svg fails on travis osx with r: release
               exts <- c ('eps', 'ps', 'tex', 'pdf', 'png',
                          'jpg', 'jpeg', 'bmp', 'tiff')
               for (e in exts)
               {
                   fname <- paste0 ('map.', e)
                   expect_silent (print_osm_map (map, width = 5, height = 4,
                                                 filename = fname,
                                                 units = "in"))
                   expect_true (fname %in% list.files ())
               }
               if (test_all)
               {
                   for (e in exts)
                       file.remove (paste0 ('map.', e))
               }
})
