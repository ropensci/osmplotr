context ('extract-objects')

# test_all used to switch off tests on CRAN
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("TRAVIS"), "true") |
             identical (Sys.getenv ("APPVEYOR"), "True"))

test_that ('missing objects', {
           expect_error (extract_osm_objects (), 'key must be provided')
           expect_error (extract_osm_objects ('aaa'), 'bbox must be provided')
    })

test_that ('key missing', {
           bbox <- get_bbox (c (-0.12, 51.51, -0.11, 51.52))
           expect_error (extract_osm_objects (bbox = bbox, key = ''),
                         'key must be provided')
           expect_error (extract_osm_objects (bbox = bbox),
                         'key must be provided')
    })

if (curl::has_internet () & test_all)
{
    test_that ('invalid key', {
               bbox <- get_bbox (c (-0.12, 51.51, -0.11, 51.52))
               expect_warning (dat <- extract_osm_objects (bbox = bbox,
                                                           key = 'aaa'))
               expect_null (dat)
    })

    test_that ('valid key', {
               bbox <- get_bbox (c (-0.12, 51.515, -0.115, 51.52))
               dat <- extract_osm_objects (bbox = bbox, key = 'building')
               # Will return NULL if any warnings are generated (such as by
               # download fail)
               if (!is.null (dat))
                   expect_is (dat, 'SpatialPolygonsDataFrame')
    })

    test_that ('return_type', {
               key <- 'route'
               value <- 'bicycle'
               extra_pairs <- c ('name', 'London.Cycle.Network.Route.9')
               bbox <- get_bbox (c (-0.26, 51.47, 0.11, 51.67))
               # NOTE: any downloading issues will generate warnings here. Only
               # if downloading succeeds will the message be generated ('Cannot
               # determine return_type, maybe specify explicitly?')
               expect_message (dat <- extract_osm_objects (bbox = bbox,
                                        key = key, value = value,
                                        extra_pairs = extra_pairs))
    })
} # end if has_internet
