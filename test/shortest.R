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
kw <- extract.highway (name="Kingsway")
ho <- extract.highway (name="Holborn") # includes high holborn
fa <- extract.highway (name="Farringdon.St")
st <- extract.highway (name="Strand")
fl <- extract.highway (name="Fleet.St")
al <- extract.highway (name="Aldwych")

streets <- c ("kw", "ho", "fa", "st", "fl", "al")
xlims <- range (lapply (streets, function (i)
                 do.call (get.xylims, list (get (i)))$xrange))
ylims <- range (lapply (streets, function (i)
                 do.call (get.xylims, list (get (i)))$yrange))


# ***** (2) Order the lines 
kwo <- order.lines (kw, i0=0) 
n <- max (unlist (lapply (kwo, function (x) as.numeric (rownames (x)))))
hoo <- order.lines (ho, i0=n)
n <- max (unlist (lapply (hoo, function (x) as.numeric (rownames (x)))))
fao <- order.lines (fa, i0=n)
n <- max (unlist (lapply (fao, function (x) as.numeric (rownames (x)))))
sto <- order.lines (st, i0=n)
n <- max (unlist (lapply (sto, function (x) as.numeric (rownames (x)))))
flo <- order.lines (fl, i0=n)
n <- max (unlist (lapply (flo, function (x) as.numeric (rownames (x)))))
alo <- order.lines (al, i0=n)

plot.new ()
par (mar=c(0,0,0,0))
plot (NULL, NULL, xlim=xlims, ylim=ylims, xaxt="n", yaxt="n",
      xlab="", ylab="", frame=FALSE)
for (i in streets)
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
for (i in streets)
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




# Then fill a connectivity matrix between all streets:
conmat <- array (FALSE, dim=rep (length (streets), 2))
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
# And use that to extrac the largest cycle:
cycles <- ggm::fundCycles (conmat)
ci <- which.max (sapply (cycles, nrow))
cyc <- cycles [[ci]]

# Then calculate shortest paths along each step of the cycle.  
path <- NULL
i <- cyc [1,2]
fi <- cyc [1,1]
ti <- cyc [2,2]
# Store objs [[i]] as an igraph
from <- unlist (lapply (objs [[i]], function (x) 
                                   rownames (x)[1:(nrow(x)-1)]))
to <- unlist (lapply (objs [[i]], function (x) 
                                   rownames (x)[2:nrow(x)]))
g <- igraph::graph_from_edgelist (cbind (from, to), directed=FALSE)

# Then find nodes in i which join to fi and ti
street <- do.call (rbind, objs [[i]])
ends <- c (objs [[fi]], objs [[ti]])
ends <- do.call (rbind, ends)
n <- which (rowSums (array (street %in% ends, dim=dim (street))) == 2)
xy <- street [n,]
n <- rownames (street) [n]
stopifnot (length (n) > 1)

# Then find shortest path between n
if (length (n) > 2)
{
    # Then reduce to the *longest* of the shortest paths
    maxlen <- 0
    ij <- NULL
    nc <- combn (n, 2)
    for (i in seq (n))
    {
        sp <- suppressWarnings (igraph::shortest_paths 
                                 (g, nc [1,i], nc [2,i])$vpath [[1]])
        if (length (sp) > maxlen)
        {
            maxlen <- length (sp)
            ij <- c (i, j)
        }
    }
    n <- n [ij]
}
sp <- suppressWarnings (igraph::shortest_paths (g, n [1], n [2])$vpath [[1]])

# Then get xy of sp from obj:
path <- NULL
for (i in 1:length (sp))
{
    indx <- which (rownames (street) == names (sp)[i]) [1]
    # [1] because xy will have more than 2 elements at junction nodes
    xy <- street [indx,]
    path <- rbind (path, xy [1:2])
}
lines (path[,1], path[,2], col="red", lwd=3)
