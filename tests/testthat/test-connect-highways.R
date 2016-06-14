context ('connect-highways')

test_that ('missing objects', {
           expect_error (connect_highways (), 
                         'A vector of highway names must be given')
           expect_error (connect_highways ('aaa'), 
                         'A bounding box must be given')
           bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
           expect_error (connect_highways ('aaa', bbox),
                         'No data able to be extracted')
})
