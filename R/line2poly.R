#' osm_line2poly
#'
#' Converts \code{sf::sfc_LINSTRING} objects to polygons by connecting end
#' points around the given bounding box. This is particularly useful for
#' plotting water and land delineated by coastlines. Coastlines in OpenStreetMap
#' are lines, not polygons, and so there is no directly way to plot ocean water
#' distinct from land. This function enables that by connecting the end points
#' of coastline \code{LINESTRING} objects to form closed polygons.
#'
#' This is a tricky problem for a number of reasons, and the current implementation
#' may not be correct, although it does successfully deal with a few tough situations.
#' Some of the issues are: an osm coastline query returns a mixture of "ways" and polygons.
#' Polygons correspond to islands, but not all islands are polygons. A "way" is a connected
#' set of points with the land on the left. A piece of coastline in a bounding box
#' may consist of multiple ways, which need to be connected together to create
#' a polygon. Also, ways extend outside the query bounding box, and may join
#' other ways that enter the bounding box (e.g ends of a peninsula). The degree
#' to which this happens depends on the scale of the bounding box. Coastlines may
#' enter at any bounding box edge and exit at any other, including the one they
#' entered from. 
#'
#' @param obj A Simple Features (\code{sf}) data frame of lines, typically as
#' returned by \code{\link{extract_osm_objects}}, or by
#' \code{osmdata::osmdata_sf}.
#' @param bbox bounding box (Latitude-longitude range) to be plotted.  A 2-by-2
#' matrix of 4 elements with columns of min and max values, and rows of x and y
#' values. Can also be an object of class \code{sf}, for example as returned
#' from \code{extract_osm_objects} or the \code{osmdata} package, in which case
#' the bounding box will be extracted from the object coordinates.
#' @return A list of three Simple Features (\code{sf}) data frames, labelled sea
#' islands and land.
#' @export
#'
#' @examples
#' # This example uses the \code{osmdata} package to extract data from 
#' # a named bounding box
#' \dontrun{
#' library (magrittr)
#' library (osmdata)
#' bb <- osmdata::getbb ("melbourne, australia")
#' coast <- extract_osm_objects (bbox = bb, key = "natural", value = "coastline",
#'                               return_type = "line")
#' coast <- osm_line2poly (coast, bbox = bb)
#' # The following map then colours in just the ocean:
#' map <- osm_basemap (bbox = bb) %>%
#'     add_osm_objects (coast$sea, col = "lightsteelblue") %>%
#'     print_osm_map ()
#' }
osm_line2poly <- function (obj, bbox)
{
  if (!is (obj$geometry, "sfc_LINESTRING"))
    stop ("obj must be class 'sf' with fields of class 'sfc_LINESTRING'")
  
  if (nrow(obj) == 0)
    stop("obj is empty - check osm query results")
  g <- obj$geom
  
  ## Clip the lines
  clipOne <- function(out, bbox)
  {
    # Then reduce out to only that portion within bb, plus one point either side
    indx <-  (out [, 1] >= bbox [1, 1] & out [, 1] <= bbox [1, 2] &
                out [, 2] >= bbox [2, 1] & out [, 2] <= bbox [2, 2])
    ## Need to deal with curves that join outside the bbox.
    ## Need to dilate the indx vector by 1 (max with left and right shifted 
    ## versions of itself)
    indx <- as.logical(pmax(indx, c(indx[-1], FALSE), c(FALSE, indx[-length(indx)])))
    out <- out [indx, ]
    return(out)
  }
  
  ## These geometries can contain several coastline "ways" that
  ## need to be linked together. There may be multiple sets of these (think)
  ## peninsulas being crossed by bounding box.
  ## There can also be ways that link to form polygons, and they
  ## need to be filtered separately.
  
  ## Note that really strange things can happen, depending on the scale.
  ## It is possible for parts of a coastline to be connected outside
  ## the bounding box, which means we need to be extra careful when 
  ## clipping.
  ## box before attempting to connect them. Very likely to lead to other problems
  
  ## Look for the ones that aren't loops first, then deal with the loops
  ##
  ## We identify link points using the rownames from osm as the key.
  
  ## Get the first and last rowname from each line segment
  HdTl <- t(sapply(g, function(X)rownames(X)[c(1, nrow(X))]))
  
  ## Use this one later for recalling the complete line geometry
  HdTl.orig <- HdTl
  m2 <- match(HdTl[,2], HdTl[,1])
  m1 <- match(HdTl[,1], HdTl[,2])
  
  lookupWays <- function(L) {
    gg <- g[rownames(L)]
    gg <- do.call(rbind, lapply(gg, as.matrix))
    rr <- duplicated(rownames(gg))
    gg <- gg[!rr,]
    return(gg)
  }
  ## NA in m2 indicates the end of a chain.
  ## NA in m1 indicates the start of a chain
  startidx <- which(is.na(m1))
  if (length(startidx) >= 1) {
    ## Need to test this with disconnected bits
    linkorders <- lapply(startidx, unrollRec, V=m2)
    linkorders <- lapply(linkorders, na.omit)
    links <- lapply(linkorders, function(X)HdTl[X,,drop=FALSE])
    HdTl <- HdTl[-unlist(linkorders), ,drop=FALSE]
    links <- lapply(links, lookupWays)
  }
  
  ## Now we deal with loops
  ## Keep extracting loops until nothing left
  ToBecomePolygons <- list()
  lidx <- 1
  while (nrow(HdTl) > 0) {
    m2 <- match(HdTl[,2], HdTl[,1])
    l1 <- unrollRecLoop(1, m2)
    ToBecomePolygons[[lidx]] <- HdTl[l1,]
    lidx <- lidx+1
    HdTl <- HdTl[-l1,]
  }
  ToBecomePolygons <- lapply(ToBecomePolygons, lookupWays)
  ToBecomePolygons <- lapply(ToBecomePolygons, make_sf, g)
  ToBecomePolygons <- do.call(rbind, ToBecomePolygons)
  ## Don't need to clip the polygons against the bounding box - they'll
  ## already be inside, otherwise they wouldn't have been registered as polygons.
  ## Even if they aren't inside, we should be able to fill them
  ##
  
  #  if (length (g) > 0)
  #    message ("Not all line segments align continuously.")
  
  # polygon formed by expanding bbox through combination of increasing or
  # decreasing longitudes or latitudes. Requires explicit combinations for
  # each case. Note that bb is arranged thus:
  #   bb [1, 1]           bb [1, 2]
  #       |                   |
  #       O-------------------O  <- bb [2, 2]
  #       |                   |
  #       |                   |
  #       O-------------------O  <- bb [2, 1]
  #
  # The three required combinations for starting at -lon are then like this,
  # where double lines are the bbox, and single lines the expansion
  #
  # out[1,1],bb[2,2]      out[n,1],bb[2,2]
  #   |---O====================O---|
  #   |   ||                  ||   |
  # 1-|   ||                  ||   |-N
  #   |   ||                  ||   |
  #   |---O====================O---|
  # out[1,1],bb[2,1]      out[n,1],bb[2,1]
  #
  #
  # out[1,1],bb[2,2]      bb[1,2],bb[2,2]
  #   |---O====================O
  #   |   ||                  ||
  # 1-|   ||                  ||
  #   |   ||                  ||
  #   |   O====================O
  #   |             |          |
  #   |-------------N----------|
  # out[1,1],out[n,2]     bb[1,2],out[n,2]
  #
  #
  # out[1,1],out[n,2]     bb[1,2],out[n,2]
  #   |------------N-----------|
  #   |            |           |
  #   |   O====================O
  #   |   ||                  ||
  # 1-|   ||                  ||
  #   |   ||                  ||
  #   |---O====================O
  # out[1,1],bb[2,1]      bb[1,2],bb[2,1]
  #
  # And N can be outside the same edge as 1!
  #
  # ... explicit cases for each of the other 3 starting points (+lon, +/- lat)
  # then follow by extension.
  
  ## for use with the bbox checks
  compass <- c("W", "S", "E", "N")
  dim(compass) <- c(2,2)
  
  directions <- 1:4
  names(directions) <- c("N", "E", "S", "W")
  
  bbxcornersRH <- c("NE", "SE", "SW", "NW")
  bbxcoords <- rbind(c(bbox[1, 2], bbox[2, 2]),
                     c(bbox[1, 2], bbox[2, 1]),
                     c(bbox[1, 1], bbox[2, 1]),
                     c(bbox[1, 1], bbox[2, 2])
  )
  rownames(bbxcoords) <- bbxcornersRH
  classifyPtDir <- function(P, bbox)
  {
    ## return whether a point is N,S,E,W of bounding box - i.e. which edge
    ## Could be a pathological corner case, which we'll ignore for now.
    
    lt <- P < bbox[,"min"]
    gt <- P > bbox[,"max"]
    
    td <- cbind(lt, gt)
    return(directions[compass[td]])
  }
  makePoly <- function(out, bbox)
  {
    ## Turn this into a function because we might have several curves to close
    p1 <- p2 <- NULL
    n <- nrow (out)
    
    FstPt <- out[1,]
    LstPt <- out[n,]
    
    ## Figure out which edges of the BB we cross (N,S,E,W)
    FstPtD <- classifyPtDir(FstPt, bbox)
    LstPtD <- classifyPtDir(LstPt, bbox)
    ## We need these to generate a list of corners going clockwise and anticlockwise.
    ## Remember that the corner i is clockwise from edge i.
    
    ## Modify the bbox by extending the corners. Just line up the extremes, much as done for clipping
    
    bb <- bbox # makes following code slightly easier to read
    
    bb["x", "min"] <- min(c(bb["x", "min"], out[,1]))
    bb["x", "max"] <- max(c(bb["x", "max"], out[,1]))
    bb["y", "min"] <- min(c(bb["y", "min"], out[,2]))
    bb["y", "max"] <- max(c(bb["y", "max"], out[,2]))
    
    bb21 <- bb[2, 1]
    bb12 <- bb[1, 2]
    bb22 <- bb[2, 2]
    bb11 <- bb[1, 1]
    
    ext.corners <- rbind(c(bb12, bb22),
                         c(bb12, bb21),
                         c(bb11, bb21),
                         c(bb11, bb22))
    
    Wrp <- function(idxs)
    {
      (idxs - 1) %% 4 + 1
    }
    
    ## create lists of corners in each direction
    if (LstPtD==FstPtD) {
      ## Special rules if loop from one edge
      ## Need to check whether Lst is clockwise from Fst or not,
      ## as the intersection edge doesn't tell us.
      
      vFL <- LstPt - FstPt
      vEdge <- ext.corners[FstPtD, ] - ext.corners[Wrp(FstPtD-1),]
      DP <- sign(sum(vFL * vEdge))
      if (DP < 0) {
        ## Anticlockwise coast (relative to bb corners)
        CWIdx <- c(Wrp(LstPtD-1), LstPtD )
        CCWIdx <- (LstPtD-1):(LstPtD - 4)
        CCWIdx <- Wrp(CCWIdx)
        CCWIdx <- CCWIdx[1:which.max(CCWIdx==FstPtD)]
      } else {
        CWIdx <- LstPtD:(LstPtD + 4)
        CWIdx <- Wrp(CWIdx)
        CWIdx <- CWIdx[1:which.max(CWIdx==Wrp(FstPtD-1))]
        CCWIdx <- c(LstPtD, Wrp(LstPtD - 1))
      }
      
    } else {
      CWIdx <- LstPtD:(LstPtD + 4)
      CWIdx <- Wrp(CWIdx)
      CWIdx <- CWIdx[1:which.max(CWIdx==FstPtD)]
      
      CCWIdx <- (LstPtD-1):(LstPtD - 4)
      CCWIdx <- Wrp(CCWIdx)
      CCWIdx <- CCWIdx[1:which.max(CCWIdx==FstPtD)]
    }
    
    p1 <- rbind(out, ext.corners[CWIdx, ], out[1, ])
    p2 <- rbind(out, ext.corners[CCWIdx, ], out[1, ])
    
    return(list(p1=make_sf(p1, g), p2=make_sf(p2, g)))
  }
  
  if (length(links) >= 1) {
    links <- lapply(links, clipOne, bbox=bbox)
    linkpoly <- lapply(links, makePoly, bbox=bbox)
    p1 <- lapply(linkpoly, "[[", "p1")
    p2 <- lapply(linkpoly, "[[", "p2")
    
  } else {
    warning("No open curves found - check for polygons")
  }
  
  res <- NULL
  if (!is.null (p1) & !is.null (p2)) {
    res <- list (sea=do.call(rbind, p1), land=do.call(rbind, p2))
  }
  if (length(ToBecomePolygons) >= 1) {
    res$islands <- ToBecomePolygons
  }
  return (res)
}

## For reordering the ways
unrollRecLoop <- function(firstpos, V)
{
  ## Recursive version - not for polygons/loops
  idx <- V[firstpos[length(firstpos)]]
  if (V[idx] %in% firstpos) return(c(firstpos, idx))
  else return(Recall(c(firstpos, idx), V))
}

unrollRec <- function(firstpos, V)
{
  ## Recursive version - not for polygons/loops
  idx <- V[firstpos[length(firstpos)]]
  # if (is.na(idx)) return(firstpos) # check this one
  if (is.na(V[idx])) return(c(firstpos, idx))
  else return(Recall(c(firstpos, idx), V))
}

# The df bits directly adapted from same fn in osmdata/get-osmdata.R in
# simplified form; the initial "sfg" and "sfc" bits also cribbed from osmdata
make_sf <- function (x, g)
{
    x <- list (x)
    class (x) <- c ("XY", "POLYGON", "sfg")

    x <- list (x)
    attr (x, "n_empty") <- 0
    class (x) <- c ("sfc_POLYGON", "sfc")
    attr (x, "precision") <- 0.0
    attr (x, "bbox") <- attr (g, "bbox")
    attr (x, "crs") <- attr (g, "crs")

    df <- data.frame (row.names = "1")
    df [["geometry"]] <- x
    attr (df, "sf_column") <- "geometry"
    f <- factor(rep(NA_character_, length.out = ncol(df) - 1),
                levels = c ("constant", "aggregate", "identity"))
    names(f) <- names(df)[-ncol (df)]
    attr(df, "agr") <- f
    class(df) <- c("sf", class(df))
    return (df)
}
