context ('connect-highways')

test_that ('missing objects', {
           expect_error (connect_highways (), 
                         'A vector of highway names must be given')
           expect_error (connect_highways ('aaa'), 
                         'A bounding box must be given')
})
