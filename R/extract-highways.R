#' extract_highways
#'
#' Extracts a list of named OpenStreetMap highways. OSM data are neither
#' structured nor ordered; this routine reduces data for each given highway to a
#' minimal number of discrete and sequentially ordered segments. These segments
#' may or may not connect, yet can be connected at their nearest points with
#' get_highway_cycle().
#'
#' @param highway_names A vector of highway names passed directly to the
#' Overpass API. Wildcards and whitespaces are '.'; for other options see
#' overpass help.
#' @param bbox the bounding box for the map.  A 2-by-2 matrix of 4 elements with
#' columns of min and max values, and rows of x and y values.  
#' @return A list of highways matching 'highway_names', each element of which is
#' a list of distinct components for the given highway.
#' @return A list of 2 components:
#' \enumerate{
#'  \item obj: A data frame of sp objects
#'  \item warn: Any warnings produced in downloading the data
#' }

extract_highways <- function (highway_names, bbox)
{
    if (missing (highway_names))
        stop ('A vector of highway names must be given')
    if (missing (bbox))
        stop ('A bounding box must be given')

    # Proceeds through five stages:
    # (1) Download OSM data for highways 
    # (2) Order the individual OSM objects into a minimal number of discrete
    # sequences
    # (3) If they don't exist, add juction points to lines which geographically
    # cross. 
    # (4) Remove any segments that do not cross or touch any others

    # **** (1) Download OSM data for highways 
    #
    # Start by getting 2-letter abbreviations for each highway
    nletters <- 2
    waynames <- sapply (highway_names, function (x) 
                      tolower (substring (x, 1, nletters)))
    while (any (duplicated (waynames)))
    {
        nletters <- nletters + 1
        waynames <- sapply (highway_names, function (x) 
                          tolower (substring (x, 1, nletters)))
    }

    cat ('Downloading OSM data ...\n')
    dat <- NULL
    max_trials <- 20
    count <- 1
    notnull <- 0
    p4s <- NULL
    while (notnull < length (highway_names))
    {
        pb <- txtProgressBar (max=1, style = 3) # shows start and end positions
        notnull <- 0
        for (i in seq (highway_names))
        {
            dat <- extract_highway (name = highway_names [i], bbox=bbox)
            if (!is.null (dat))
            {
                notnull <- notnull + 1
                p4s <- proj4string (dat)
                assign (waynames [i], dat)
            } 
            setTxtProgressBar(pb, i / length (highway_names))
        }
        rm (dat)
        close (pb)
        if (notnull < length (highway_names))
            cat ('Failed to download all data, trying again (#', count,
                 '/', max_trials, ') ...\n', sep='')
        count <- count + 1
        if (count > max_trials)
            break
    }
    if (notnull < length (highway_names))
        stop ('Unable to download all requested data.')

    # ***** (2) Order the individual OSM objects into a minimal number of
    # *****     discrete sequences
    i0 <- 0 # Nodes in ordered lines are numbered sequentially from (i0+1)
    for (i in seq (highway_names))
    {
        dat <- order_lines (get (waynames [i]), i0=i0)
        assign (paste0 (waynames [i], 'o'), dat)
        i0 <- max (unlist (lapply (dat, function (x) as.numeric (rownames (x)))))
    }

    # ***** (3) If they don't exist, add juction points to lines which
    # *****     geographically cross. 
    #
    # Start by constructing list of all street objects, and also get the maximum
    # vertex number, so new junction vertices can be numbered above that. objs
    # is the final returned list of highways, each element of which is a list of
    # discrete components.
    objs <- NULL
    maxvert <- 0
    for (i in waynames)
    {
        objs [[i]] <- get (paste0 (i, 'o'))
        maxvert <- max (maxvert, unlist (lapply (objs [[i]], function (x)
                                    max (as.numeric (rownames (x))))))
    }
    maxvert <- maxvert + 1

    # The OSM objects in 'objs' then need to be modified through the addition of
    # junction points where these don't exist, requiring a double loop.
    for (i in seq (objs))
    {
        obji <- objs [[i]]
        test <- objs
        test [[i]] <- NULL
        test_flat <- do.call (c, test)
        # Check whether any of obji cross any of test_flat *and* don't already
        # exist as vertices
        for (j in seq (obji))
        {
            li <- sp::Line (obji [[j]])
            li <- sp::SpatialLines (list (Lines (list (li), ID='a'))) 
            # The following function returns default of -1 for no geometric
            # intersection; 0 where intersections exists but area *NOT* vertices
            # of li, and 2 where intersections are vertices of li.
            intersections <- sapply (test_flat, function (x) {
                        lj <- sp::Line (x)
                        lj <- sp::SpatialLines (list (Lines (list (lj), ID='a'))) 
                        int <- rgeos::gIntersection (li, lj)
                        if (!is.null (int))
                            sum (sp::coordinates (int) %in% x)
                        else
                            -1
                        })
            if (any (intersections == 0))
                for (k in which (intersections == 0))
                {
                    # Then they have to be added to objs [[i]] [[j]]. 
                    x <- test_flat [k] [[1]]
                    lj <- sp::Line (x)
                    lj <- sp::SpatialLines (list (Lines (list (lj), ID='a'))) 
                    xy <- sp::coordinates (rgeos::gIntersection (li, lj))
                    d <- sqrt ((xy [1] - obji [[j]] [,1]) ^ 2 + 
                               (xy [2] - obji [[j]] [,2]) ^ 2)
                    di <- which.min (d)
                    n <- nrow (obji [[j]])
                    rnames <- rownames (obji [[j]])
                    # xy can be closest to d1, but still either
                    # A. -------d1---xy--------------d2, or
                    # B. xy-----d1-------------------d2
                    # A. implies that |xy,d2|<|d1,d2|, and B vice-versa
                    if (di == 1)
                    {
                        d12 <- sqrt (diff (obji [[j]] [1:2,1]) ^ 2 +
                                     diff (obji [[j]] [1:2,2]) ^ 2)
                        if (d12 < d [2])
                            indx <- list (NULL, 1:n)
                        else
                            indx <- list (1, 2:n)
                    } else if (di == n)
                    {
                        d12 <- sqrt (diff (obji [[j]] [(n-1:n),1]) ^ 2 +
                                     diff (obji [[j]] [(n-1:n),2]) ^ 2)
                        if (d12 < d [n-1])
                            indx <- list (1:n, NULL)
                        else
                            indx <- list (1:(n-1), n)
                    } else if (d [di - 1] < d [di + 1])
                        indx <- list (1:(di-1), di:n)
                    else
                        indx <- list (1:di, (di+1):n)
                    objs [[i]] [[j]] <- rbind (obji [[j]] [indx [[1]], ], xy,
                                               obji [[j]] [indx [[2]], ])
                    rownames (objs [[i]] [[j]]) <- c (rnames [indx [[1]]],
                                                      maxvert,
                                                      rnames [indx [[2]]])
                    objs [[i]] [[j]] <- unique (objs [[i]] [[j]])

                    # Then add same vertex into the other elements, which requires
                    # first making an index into the list of lists that is objs
                    lens <- cumsum (sapply (test, length))
                    if (k < lens [1])
                    {
                        ni <- 1
                        nj <- k
                    } else
                    {
                        ni <- max (which (lens < k)) + 1
                        nj <- k - lens [ni - 1]
                    }
                    # Then ni needs to point into the full objs instead of test
                    ni <- seq (objs) [!seq (objs) %in% i] [ni]
                    temp <- objs [[ni]] [[nj]] 
                    # Then insert xy into temp
                    d <- sqrt ((xy [1] - temp [,1]) ^ 2 + (xy [2] - temp [,2]) ^ 2)
                    di <- which.min (d)
                    n <- nrow (temp)
                    rnames <- rownames (temp)

                    if (di == 1)
                    {
                        d12 <- sqrt (diff (obji [[j]] [1:2,1]) ^ 2 +
                                     diff (obji [[j]] [1:2,2]) ^ 2)
                        if (d12 < d [2])
                            indx <- list (NULL, 1:n)
                        else
                            indx <- list (1, 2:n)
                    } else if (di == n)
                    {
                        d12 <- sqrt (diff (obji [[j]] [(n-1:n),1]) ^ 2 +
                                     diff (obji [[j]] [(n-1:n),2]) ^ 2)
                        if (d12 < d [n-1])
                            indx <- list (1:n, NULL)
                        else
                            indx <- list (1:(n-1), n)
                    } else if (d [di - 1] < d [di + 1])
                        indx <- list (1:(di-1), di:n)
                    else
                        indx <- list (1:di, (di+1):n)
                    temp <- rbind (temp [indx [[1]],], xy, temp [indx [[2]],])
                    rownames (temp) <- c (rnames [indx [[1]]], maxvert,
                                                rnames [indx [[2]]])
                    objs [[ni]] [[nj]] <- unique (temp)
                } # end for k over which (intersections == 0)
        } # end for j over obj [[i]]
    } # end for i over all objs

    # (4) Remove any segments that do not cross or touch any others
    # ---> This is actually not a good idea!
    #removes <- NULL
    #for (i in seq (objs))
    #    for (j in seq (objs [[i]]))
    #    {
    #        objs_temp <- objs
    #        objs_temp [[i]] [[j]] <- NULL
    #        objs_temp <- do.call (rbind, do.call (c, objs_temp))
    #        indx <- array (objs [[i]] [[j]] %in% objs_temp,
    #                       dim=dim (objs [[i]] [[j]]))
    #        if (max (rowSums (indx)) < 2)
    #            removes <- rbind (c (i, j), removes)
    #    }
    ## removes is constructed backwards, so can be directly NULLed
    #if (nrow (removes) > 0)
    #    for (i in seq (nrow (removes)))
    #        objs [[removes [i,1] ]] [[removes [i,2] ]] <- NULL

    attr (objs, "crs") <- p4s

    return (objs)
}
