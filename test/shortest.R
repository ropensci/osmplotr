bbox <- c(-0.15,51.5,-0.1,51.52)

require (devtools)
require (roxygen2)
setwd ("../..")
document ("urbanplotr")
load_all ("urbanplotr")
#check ("urbanplotr") 
setwd ("./urbanplotr")

datT <- extract.osm.objects (key="natural", value="tree", bbox=bbox)

# make some convex hull polygons
require (spatstat) # for ppp and convex hulls 
require (spatialkernel) 
# for pinpoly, which returns (0,1,2) for (outside, on boundary, in) (or negative
# number for error when polygon has > 3000 points).
require (sp)
require (RColorBrewer)

# *****************************
cols <- brewer.pal (3, "Set2")
xylims <- get.xylims (datBU)
#plot.osm.basemap (xylims=xylims, file="junk.png", width=4000)
plot.osm.basemap (xylims=xylims)
add.osm.objects (datBU)
#graphics.off ()
add.osm.objects (datT, col="green", ptsize=0.2, pch=1)

# ***** (1) Get SpatialLines for Kingsway, High Holborn, and Farringdon St
highways <- c ("Kingsway", "Holborn", "Farringdon.St", "Strand", "Fleet.St",
               "Aldwych")
# Then get 2-letter abbreviations for each
nletters <- 2
waynames <- sapply (highways, function (x) 
                  tolower (substring (x, 1, nletters)))
while (any (duplicated (waynames)))
{
    nletters <- nletters + 1
    waynames <- sapply (highways, function (x) 
                      tolower (substring (x, 1, nletters)))
}

cat ("Downloading OSM data ...\n")
pb <- txtProgressBar (max=1, style = 3) # shows start and end positions
for (i in seq (highways))
{
    dat <- extract.highway (name = highways [i])
    assign (waynames [i], dat)
    setTxtProgressBar(pb, i / length (highways))
}
rm (dat)
close (pb)

xlims <- range (lapply (waynames, function (i)
                 do.call (get.xylims, list (get (i)))$xrange))
ylims <- range (lapply (waynames, function (i)
                 do.call (get.xylims, list (get (i)))$yrange))


# ***** (2) Order the lines 
i0 <- 0 # Nodes in ordered lines are numbered sequentially from (i0+1)
for (i in seq (highways))
{
    dat <- order.lines (get (waynames [i]), i0=i0)
    assign (paste (waynames [i], "o", sep=""), dat)
    i0 <- max (unlist (lapply (dat, function (x) as.numeric (rownames (x)))))
}

plot.new ()
par (mar=c(0,0,0,0))
plot (NULL, NULL, xlim=xlims, ylim=ylims, xaxt="n", yaxt="n",
      xlab="", ylab="", frame=FALSE)
for (i in waynames)
{
    ni <- paste (i, "o", sep="")
    for (j in get (ni))
        lines (j [,1], j [,2])
}

# ***** (3) If they don't exist, add juction points to lines which
# *****     geographically cross. 

# Construct list of all street objects, and also get the maximum vertex number,
# so new junction vertices can be numbered above that.
objs <- NULL
maxvert <- 0
for (i in waynames)
{
    objs [[i]] <- get (paste (i, "o", sep=""))
    maxvert <- max (maxvert, unlist (lapply (objs [[i]], function (x)
                                max (as.numeric (rownames (x))))))
}
maxvert <- maxvert + 1

for (i in seq (objs))
{
    obji <- objs [[i]]
    test <- objs
    test [[i]] <- NULL
    test.flat <- do.call (c, test)
    # Check whether any of obji cross any of test.flat *and* don't already exist
    # as vertices
    for (j in seq (obji))
    {
        li <- sp::Line (obji [[j]])
        li <- sp::SpatialLines (list (Lines (list (li), ID="a"))) 
        # The following function returns default of -1 for no geometric
        # intersection; 0 where intersection exists but is *NOT* a vertex of li,
        # and 2 where intersection is a vertex of li.
        intersections <- sapply (test.flat, function (x) {
                    lj <- sp::Line (x)
                    lj <- sp::SpatialLines (list (Lines (list (lj), ID="a"))) 
                    int <- rgeos::gIntersection (li, lj)
                    if (!is.null (int))
                        sum (coordinates (int) %in% x)
                    else
                        -1
                    })
        if (any (intersections == 0))
        {
            # Then they have to be added to objs [[i]] [[j]]. 
            stopifnot (length (which (intersections == 0)) == 1)
            x <- test.flat [which (intersections == 0)] [[1]]
            lj <- sp::Line (x)
            lj <- sp::SpatialLines (list (Lines (list (lj), ID="a"))) 
            xy <- coordinates (rgeos::gIntersection (li, lj))
            d <- sqrt ((xy [1] - obji [[j]] [,1]) ^ 2 + 
                       (xy [2] - obji [[j]] [,2]) ^ 2)
            di <- which.min (d)
            n <- nrow (obji [[j]])
            rnames <- rownames (obji [[j]])

            if (d [di-1] < d [di+1])
            {
                objs [[i]] [[j]] <- rbind (obji [[j]] [1:(di-1),], xy, 
                                     obji [[j]] [di:n,])
                rownames (objs [[i]] [[j]]) <- c (rnames [1:(di-1)], maxvert,
                                            rnames [di:n])
            } else
            {
                objs [[i]] [[j]] <- rbind (obji [[j]] [1:di,], xy, 
                                     obji [[j]] [(di+1):n,])
                rownames (objs [[i]] [[j]]) <- c (rnames [1:di], maxvert,
                                            rnames [(di+1):n])
            }

            # Then add same vertex into the other element, which requires first
            # making an index into the list of lists that is objs
            n <- which (intersections == 0)
            lens <- cumsum (sapply (test, length))
            ni <- max (which (lens < n)) + 1
            nj <- n - lens [ni - 1]
            # Then ni needs to point into the full objs instead of test
            ni <- seq (objs) [!seq (objs) %in% i] [ni]
            temp <- objs [[ni]] [[nj]] 
            # Then insert xy into temp
            d <- sqrt ((xy [1] - temp [,1]) ^ 2 + (xy [2] - temp [,2]) ^ 2)
            di <- which.min (d)
            n <- nrow (temp)
            rnames <- rownames (temp)

            if (d [di-1] < d [di+1])
            {
                temp <- rbind (temp [1:(di-1),], xy, temp [di:n,])
                rownames (temp) <- c (rnames [1:(di-1)], maxvert,
                                            rnames [di:n])
            } else
            {
                temp <- rbind (temp [1:di,], xy, temp [(di+1):n,])
                rownames (temp) <- c (rnames [1:di], maxvert,
                                            rnames [(di+1):n])
            }
            objs [[ni]] [[nj]] <- temp
        } # end if any (intersections) == 0
    } # end for j over obj [[i]]
} # end for i over all objs


# ***** (4) Fill a connectivity matrix between all highways

conmat <- array (FALSE, dim=rep (length (waynames), 2))
for (i in seq (objs))
{
    test <- do.call (rbind, objs [[i]])
    ref <- objs
    ref [[i]] <- NULL
    indx <- (1:length (objs)) [!(1:length (objs)) %in% i]
    # Then find which lines from ref intersect with test:
    ni <- unlist (lapply (ref, function (x) {
                  xflat <- do.call (rbind, x)
                  n <- array (xflat %in% test, dim=dim (xflat))
                  # NOTE: If there is more than one common vertex, only the
                  # first is taken. TODO: Check alternatives!
                  n <- which (rowSums (n) == 2) [1]
                  rownames (xflat) [n]
                 }))
    indx2 <- indx [which (!is.na (ni))]
    conmat [i, indx2] <- conmat [indx2, i] <- TRUE
}
# And use that to extract the longest cycle:
cycles <- ggm::fundCycles (conmat)
ci <- which.max (sapply (cycles, nrow))
cyc <- cycles [[ci]]


# ***** (5) Finally calculate the shortest paths along each step of the cycle

cyc.len <- nrow (cyc)
cyc <- rbind (cyc, cyc [1,])
paths <- list ()
for (i in seq (cyc.len))
{
    w0 <- cyc [i,2] # the current way
    wf <- cyc [i,1] # the "from" way
    wt <- cyc [i+1,2] # the "to" way

    # Store objs [[w0]] as an igraph
    from <- unlist (lapply (objs [[w0]], function (x) 
                                       rownames (x)[1:(nrow(x)-1)]))
    to <- unlist (lapply (objs [[w0]], function (x) 
                                       rownames (x)[2:nrow(x)]))
    g <- igraph::graph_from_edgelist (cbind (from, to), directed=FALSE)

    # Then find nodes in w0 which join to wf and wt
    street <- do.call (rbind, objs [[w0]])
    ends <- c (objs [[wf]], objs [[wt]])
    ends <- do.call (rbind, ends)
    n <- which (rowSums (array (street %in% ends, dim=dim (street))) == 2)
    # n is index into street of lat-lon pairs duplicated in ends
    xy <- street [n,] # coordinates of nn
    n <- rownames (street) [n] # names of nn within the igraph
    stopifnot (length (n) > 1)

    # If there are more than 2 nodes present in the ends, then extract the
    # *longest* of these shortest paths:
    if (length (n) > 2)
    {
        # Then reduce to the *longest* of the shortest paths
        maxlen <- 0
        ij <- NULL
        nc <- combn (n, 2)
        for (j in seq (n))
        {
            sp <- suppressWarnings (igraph::shortest_paths 
                                     (g, nc [1,j], nc [2,j])$vpath [[1]])
            if (length (sp) > maxlen)
            {
                maxlen <- length (sp)
                ij <- combn (1:3, 2) [,j]
            }
        }
        n <- n [ij]
    }
    # Then length (n) == 2, and so the shortest path can be extracted:
    sp <- suppressWarnings (igraph::shortest_paths (g, n [1], n [2])$vpath [[1]])

    # Then get xy of sp from "street", which is the flat version of objs [[w0]].
    # Note that junctions will have duplicates of same node, but match only
    # returns the first match, so that's okay.
    indx <- match (names (sp), rownames (street))
    stopifnot (all (!is.na (indx)))
    path <- street [indx,]

    lines (path [,1], path [,2], lwd=3, col=rainbow (cyc.len) [i])
    paths [[i]] <- path
}

# Finally connect all paths together to make a single path, which involves first
# checking whether any of the paths need to be flipped 
for (i in 1:(length (paths) - 1))
{
    n <- which (rowSums (array (paths [[i]] %in% paths [[i+1]], 
                                dim=dim (paths [[i]]))) == 2)
    # n is the index into paths [[i]] of nodes occuring in paths [[i+1]]. This
    # obviously should be nrow (paths [[i]]), so:
    if (n == 1)
        paths [[i]] <- apply (t (paths [[i]]), 1, rev) # flip
}

paths <- do.call (rbind, paths)
lines (paths [,1], paths [,2], lwd=5, col="orange", lty=2)
