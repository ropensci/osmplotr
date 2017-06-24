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
                   highways <- c ('Kingsway', 'Holborn', 'Farringdon.St',
                                  'Strand', 'Fleet.St', 'Aldwych')
                   # Again, warnings will differ if download fails
                   expect_warning (connect_highways (highways = highways,
                                                     bbox = bbox))
        })

        test_that ('plot', {
                   bbox <- get_bbox (c(-0.15, 51.5, -0.10, 51.52))
                   highways <- c ('Regent.St', 'Oxford.St', 'Shaftesbury')
                   # This can't fail because failed downloads will generate
                   # warnings and successful ones will generate a plot.
                   expect_warning (connect_highways (highways = highways,
                                                     bbox = bbox, plot = TRUE))
        })
    } # end if (test_all)
} # end if has_internet
