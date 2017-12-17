library(purrr)
library(sf)
samedirection <- function(poly, lines) {
  p <- st_cast(poly, "POINT")
  l <- st_cast(lines, "POINT")
  idx <- na.omit(match(l, p))
  ## If the idx values are increasing, then order of points is the same, and thus polygon
  ## direction is the same
  return(all(idx == cummax(idx)))
}

sequential_dif <- function(lp, seaset) {
  ## Reduce doesn't work?
  for (i in 1:length(seaset)) {
    lp <- st_difference(lp, seaset[i])
  }
  return(lp)
}


#' Determin which polygons intersect, compute those intersections, return them along with the isolated ones
#'
#' @param polygons 
#' Complex because we don't want to produce nothing at the end. Thus can't chuck all the pairs in.
#' Only option I can come up with is to perform an intersection, then check. Heuristics would probably
#' make things faster, like starting with bigger polygons. May happen by default. Could also optimise
#' a bit by performing graph-clustering first, then doing this within each cluster, but I suspect that 
#' multiple clusters are rare, due to the way the bounding box is usually selected.
#' @return the land polygons
#' @export
#'
#' @examples

doIntersect <- function(polygons) {
  finalP <- polygons
  while(TRUE) {
    ii <- st_intersects(finalP, finalP)
    iil <- lengths(ii)
    if (all(iil==1)) {
      ## only self intersections
      break
    }
    ## Pick an intersecting pair, compute the intersection, repeat
    pairA <- which.max(iil > 1)
    pairB <- ii[[pairA]]
    pairB <- pairB[which.max(pairB != pairA)]
    
    tempPolys <- finalP[-c(pairA, pairB)]
    ip <- st_intersection(finalP[pairA], finalP[pairB])
    finalP <- c(tempPolys, ip)
  }

  return(finalP)
}

#' Create land polygons for plotting
#'
#' @param osmd result of a coastline osm query
#' @param bbox the box associated with the query
#'
#' @return a simple feature collection
#' @export
#'
#' @examples
sf_land <- function(osmd, bbox) {
  ## osmd is a coastline query, and could contain
  ## both lines and polygons.
  ## Lines need to be merged and converted to polygons.
  ## Both need to be clipped against the bounding box
  bbxcoords <- rbind(c(bbox[1, 2], bbox[2, 2]),
                     c(bbox[1, 2], bbox[2, 1]),
                     c(bbox[1, 1], bbox[2, 1]),
                     c(bbox[1, 1], bbox[2, 2])
  )
  bbxcoords <- rbind(bbxcoords, bbxcoords[1,])
  bbx <- st_sfc(st_cast(st_multipoint(bbxcoords), "POLYGON"), crs=st_crs(osmd$osm_lines))
  ## merge lines
  if (!is.null(osmd$osm_lines)) {
    m1 <- osmd$osm_lines
    if (nrow(m1) > 1) {
      m1 <- st_cast(st_line_merge(st_union(m1)), "LINESTRING")
    }
    m2 <- st_polygonize(m1)
    k <- st_dimension(m2)
    ## initialise some empty geometries, like a NULL
    closed <- res <- islands <- st_sfc(st_geometrycollection(), crs=st_crs(osmd$osm_lines))
    if (!(all(st_is_empty(m2)))) {
      ## Cast to polygon - warn=FALSE stops complaints when there aren't any polygons
      islands <- st_cast(m2, warn=FALSE)
    }
    closed <- c(closed, islands, st_geometry(osmd$osm_polygons))
    ## closed is now the combined polygons and islands
    if (!all(st_is_empty(closed))) {
      closed <- st_intersection(closed, bbx)
    }
    ## collect the ones remaining as lines - keep a copy to compare point
    ## order
    m1lines <- m1[is.na(k)]
    ## clips off the outside parts of line
    ## orders of points preserved here (i.e still left handed)
    bbxl <- st_cast(bbx, "LINESTRING")
    if (length(m1lines) > 0) {
      m1clipped <- st_intersection(bbx, m1lines)
      ## union with the lines in the polygon
      m1 <- st_union(m1clipped, bbxl, by_feature=TRUE)
      ## sf polygons at this stage are clockwise - want to detect
      ## if the coastline parts are reversed.
      ## Not sure if there are pathological cases where we can end up
      ## with polygons that have holes. Maybe unlikely given that
      ## this is for coastline.
      polys <- st_cast(st_polygonize(m1))
      ## now to figure out which polygons are sea
      ## Need to sort out whether points are in the
      ## same order as they were when retrieved from osm.
      ##
      ## Original osmdata will have land on the left.
      ## Good approach will be to remove the bounding box from each polygon and
      ## check the overlaps.
      polysM <- st_difference(st_cast(polys, "LINESTRING"), bbxl)
      k <- st_intersects(polysM, m1clipped)
      ## Ones with the same orientation correspond to ocean polygons
      same <- purrr::imap_lgl(k, ~samedirection(polysM[.y], m1clipped[.x]))
      landpolys <- polys[!same]
      seapolys <- polys[same]
      ## Now need to take difference  between each land polygon and all sea polygons (sequentially)
      ## Then union the land polygons
      # landpolys.dif <- purrr::map(1:length(landpolys), ~sequential_dif(landpolys[.x], seapolys))
      # landpolys <- st_union(do.call(c, landpolys.dif))
      
      # figure out which land polygons intersect, and take the intersection of them
      res <- doIntersect(landpolys)
    }
    res <- c(res, closed)
    return(res)
  } else {
    ## no lines to process - should test this more carefully
    return(st_intersection(st_geometry(osmd$osm_polygons), bbx))
  }
}
