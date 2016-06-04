context ('adjust-colours')

test_that ('colours', {
           expect_error (adjust_colours (), 'cols must be provided')
           expect_error (adjust_colours (cols=-1), 'Invalid colours: -1')
           expect_error (adjust_colours (cols='a'), 'Invalid colours: a')
           expect_error (adjust_colours (cols=NA), 'One or more cols is NA')
           expect_error (adjust_colours (cols=c(1:5,NA)), 
                         'One or more cols is NA')
           expect_null (adjust_colours (NULL))
           expect_silent (adjust_colours (cols=1:5))
})

test_that ('adj', {
    expect_error (adjust_colours (1, -2), 'adj must be between -1 and 1')
    expect_error (adjust_colours (1, NA), 'adj is NA')
    expect_null (adjust_colours (1, NULL))
    expect_error (adjust_colours (1, 'a'), 'adj can not be coerced to numeric')
})

test_that ('plot', {
    expect_error (adjust_colours (1, 0, NA), 'plot is NA')
    expect_null (adjust_colours (1, 0, NULL))
    expect_error (adjust_colours (1, 0, 'a'), 
                  'plot can not be coerced to logical')
    expect_silent (adjust_colours (1, 0, FALSE))
    expect_silent (adjust_colours (1, 0, TRUE))
})
