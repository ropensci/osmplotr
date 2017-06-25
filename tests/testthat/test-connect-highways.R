context ('connect-highways')

# test_all used to switch off tests on CRAN
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("TRAVIS"), "true") |
             identical (Sys.getenv ("APPVEYOR"), "True"))

if (curl::has_internet ()) # otherwise all of these return errors not warnings
{
    test_that ('missing objects', {
               expect_error (connect_highways (),
                             'A vector of highway names must be given')
               expect_error (connect_highways ('aaa'),
                             'A bounding box must be given')
    })

    if (test_all)
    {
        test_that ('unrecognised highways', {
                       bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
                       # No error specified because different HTML errors may
                       # also be generated
                       expect_error (connect_highways ('aaa', bbox))
                       #expect_error (connect_highways ('aaa', bbox),
                       #              'No data able to be extracted')
        })

        test_that ('highways do not connect', {
                   bbox <- get_bbox (c(-0.15, 51.5, -0.10, 51.52))
                   highways <- c ('Monmouth.St', 'Short.?s.Gardens',
                                  'Endell.St', 'Long.Acre',
                                  'Upper.Saint.Martin')
                   # Again, warnings will appear if download fails
                   expect_output (connect_highways (highways = highways,
                                                    bbox = bbox, plot = TRUE))
        })
    }
} # end if has_internet

# highway tests using internal data
load (system.file ("extdata", "hwys.rda", package = "osmplotr"))

test_that ('connect highways internal code', { # these all form cycles
               for (i in 1:3)
               {
                   expect_silent (ways0 <- flatten_highways (hwys [[1]]))
                   expect_true (!all (vapply (ways0, is.list, logical (1))))
                   expect_silent (ways <- get_highway_cycle (ways0))
                   # get_highway_cycle should add nodes:
                   expect_true (sum (vapply (ways0, length, numeric (1))) <
                                sum (vapply (ways, length, numeric (1))))
                   conmat <- get_conmat (ways)
                   expect_true (nrow (conmat) == length (ways))
                   cyc <- ggm::fundCycles (conmat) [[1]]
                   paths <- sps_through_cycle (cyc, ways)
                   expect_equal (length (paths), length (ways))
                   # paths should have fewer total nodes:
                   expect_true (sum (vapply (paths, length, numeric (1))) <
                                sum (vapply (ways, length, numeric (1))))
               }
        })
