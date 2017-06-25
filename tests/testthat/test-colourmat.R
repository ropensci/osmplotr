context ('colourmat')

test_that ('colours', {
           expect_error (colour_mat (), 'cols must be provided')
           expect_error (colour_mat (cols = -1), 'cols must have length >= 4')
           expect_error (colour_mat (cols = 'a'), 'cols must have length >= 4')
           expect_null (colour_mat (cols = NULL))
           expect_error (colour_mat (cols = letters [1:4]),
                         'Invalid colours: a')
           expect_error (colour_mat (cols = c ('red', 'blue', letters [1:4])),
                         'Invalid colours: a')
           expect_error (colour_mat (cols = rep (NA, 4)),
                         'One or more cols is NA')
           expect_silent (colour_mat (cols = 1:7))
           expect_silent (colour_mat (cols = 1:4, plot = TRUE))
           dev.off (which = dev.cur ())
})

test_that ('n', {
           expect_error (colour_mat (cols = 1:4, n = 1), 'n must be > 1')
           expect_error (colour_mat (cols = 1:4, n = 'a'), 'n must be numeric')
           expect_error (colour_mat (cols = 1:4, n = NA), 'n must be numeric')
           expect_error (colour_mat (cols = 1:4, n = NULL), 'n must be numeric')
           expect_error (colour_mat (cols = 1:4, n = 1:2), 'n must be > 1')
           expect_error (colour_mat (cols = 1:4, n = c(2, 'a')),
                         'n must be numeric')
           expect_error (colour_mat (cols = 1:4, n = c(2, NA)),
                         'n can not be NA')
})

test_that ('rotate', {
           expect_error (colour_mat (cols = 1:4, rotate = 'a'),
                         'rotate must be numeric')
           expect_error (colour_mat (cols = 1:4, rotate = NA),
                         'rotate must be numeric')
           expect_error (colour_mat (cols = 1:4, rotate = NULL),
                         'rotate must be numeric')
           expect_warning (colour_mat (cols = 1:4, rotate = 1:2),
                           'rotate has length > 1')
})
