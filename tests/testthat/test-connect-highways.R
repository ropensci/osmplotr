context ('connect-highways')

test_that ('missing objects', {
           expect_error (connect_highways (), 
                         'A vector of highway names must be given')
           expect_error (connect_highways ('aaa'), 
                         'A bounding box must be given')
})

test_that ('unrecognised highways', {
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           expect_error (connect_highways ('aaa', bbox),
                         'No data able to be extracted')
})

test_that ('highways do not connect', {
           bbox_big <- get_bbox (c(-0.15,51.5,-0.10,51.52))
           highways <- c ('Kingsway', 'Holborn', 'Farringdon.St', 'Strand',
                          'Fleet.St', 'Aldwych')
           expect_warning (connect_highways (highways=highways, bbox=bbox_big),
                           'Cycle unable to be extended through all ways')
})

test_that ('plot', {
    bbox_big <- get_bbox (c(-0.15,51.5,-0.10,51.52))
    highways <- c ('Regent.St', 'Oxford.St', 'Shaftesbury')
    expect_message (connect_highways (highways=highways, bbox=bbox_big,
                                      plot=TRUE),
                    'Unable to download all requested data')
})
