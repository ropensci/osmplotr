# ---------  lengths of vectors
test_len1 <- function (a, txt)
{
    if (length (a) > 1)
    {
        warning (paste ('Only the first element of', txt, 'will be used'))
        a <- a [1]
    }
    return (a)
}

test_len2 <- function (a, txt)
{
    if (length (a) > 2)
    {
        warning (paste ('Only the first two elements of', txt, 'will be used'))
        a <- a [1:2]
    }
    return (a)
}

# ---------  object classes
test_numeric <- function (a, txt, value)
{
    if (!is.numeric (a))
    {
        if (length (value) == 1)
            vstr <- 'value'
        else
            vstr <- 'values'
        w <- simpleWarning (paste (txt, 'must be numeric; using default', vstr))
        a <- tryCatch(as.numeric (a), warning = function(c) w)
        if (is (a, "warning"))
        {
            warning (a)
            a <- value
        }
    }
    return (a)
}

test_logical <- function (a, txt, value)
{
    if (!is.logical (a))
    {
        w <- simpleWarning (paste (txt, 'must be logical; using default'))
        if (is.na (as.logical (a)))
        {
            warning (w)
            a <- value
        }
    }
    return (a)
}

# ---------  object values
test_pos <- function (a, txt, value)
{
    if (a < 0)
    {
        warning (paste (txt, 'must be positive; using default value'))
        a <- 3
    }
    return (a)
}

test_range <- function (a, txt, rng, value)
{
    if (any (a < rng [1]) | any (a > rng [2]))
    {
        if (length (value) == 1)
            vstr <- 'value'
        else
            vstr <- 'values'
        warning (paste0 (txt, ' not in [', rng [1], ',', rng [2],
                         ']; using default ', vstr))
        a <- value
    }
    return (a)
}
