context ('adjust-colours')

test_that ('colours', {
           expect_error (adjust_colours (), 'a non-null col must be provided')
           expect_error (adjust_colours (NULL), 'cols must be non-null')
           expect_error (adjust_colours (cols = -1), 'Invalid colour: -1')
           expect_error (adjust_colours (cols = 'a'), 'Invalid colour: a')
           expect_silent (adjust_colours (cols = NA))
           expect_silent (adjust_colours (cols = c(1:5, NA)))
           expect_silent (adjust_colours (cols = 1:5))
})

test_that ('adj', {
    expect_error (adjust_colours (1, -2), 'adj must be between -1 and 1')
    expect_error (adjust_colours (1, NA), 'adj can not be NA')
    expect_error (adjust_colours (1, NULL), 'adj can not be NULL')
    expect_error (adjust_colours (1, 'a'), 'adj can not be coerced to numeric')
})

test_that ('plot', {
    expect_error (adjust_colours (1, 0, NA), 'plot can not be NA')
    expect_error (adjust_colours (1, 0, NULL), 'plot can not be NULL')
    expect_error (adjust_colours (1, 0, 'a'),
                  'plot can not be coerced to logical')
    expect_silent (adjust_colours (1, 0, FALSE))
    expect_silent (adjust_colours (1, 0, TRUE))
})
