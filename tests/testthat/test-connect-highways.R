context ('connect-highways')

test_that ('missing objects', {
           if (curl::has_internet ()) # otherwise it'll be an error
           {
               expect_error (connect_highways (), 
                             'A vector of highway names must be given')
               expect_error (connect_highways ('aaa'), 
                             'A bounding box must be given')
           }
})

test_that ('unrecognised highways', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           if (curl::has_internet ()) # otherwise it'll be an error
           {
               expect_error (connect_highways ('aaa', bbox),
                             'No data able to be extracted')
           }
})

test_that ('highways do not connect', {
           bbox <- get_bbox (c(-0.15,51.5,-0.10,51.52))
           highways <- c ('Kingsway', 'Holborn', 'Farringdon.St', 'Strand',
                          'Fleet.St', 'Aldwych')
           if (curl::has_internet ()) # otherwise it'll be an error
           {
               # No warning specified here because different warnings will be
               # given if download fails
               expect_warning (connect_highways (highways=highways, bbox=bbox))
               #expect_warning (connect_highways (highways=highways, bbox=bbox),
               #                'Cycle unable to be extended through all ways')
           }
})

test_that ('plot', {
           bbox <- get_bbox (c(-0.15,51.5,-0.10,51.52))
           highways <- c ('Regent.St', 'Oxford.St', 'Shaftesbury')
           if (curl::has_internet ()) # otherwise it'll be an error
           {
               # This can't fail because failed downloads will generate warnings
               # and successful ones will generate a plot. 
               expect_output (connect_highways (highways=highways, bbox=bbox,
                                                plot=TRUE)) 
           }
})
