context ('get-bbox')

test_that ('latlon', {
           expect_error (get_bbox (), 'latlon must be supplied')
           expect_error (get_bbox (-1), 'latlon must have length = 4')
           expect_error (get_bbox ('a'), 'latlon is not numeric')
           expect_error (get_bbox (NULL), 'latlon is not numeric')
           expect_error (get_bbox (NA), 'latlon is not numeric')
           expect_error (get_bbox (c (1:3, 'a')), 'latlon is not numeric')
           expect_warning (get_bbox (1:5), 'latlon has length > 4')
})
