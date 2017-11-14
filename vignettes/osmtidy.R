## utility functions for osm-tidyverse
## ---- osmbasemap ----
library(osmplotr)
check_bbox_arg <- function (bbox)
{
  if (missing (bbox))
    stop ('bbox must be provided')
  if (is (bbox, 'sf')) # sf obj submitted to osm_basemap
  {
    if (is (bbox$geometry, "sfc_LINESTRING") |
        is (bbox$geometry, "sfc_POINT"))
      xy <- do.call (rbind, bbox$geometry)
    else if (is (bbox$geometry, "sfc_POLYGON"))
      xy <- do.call (rbind, lapply (bbox$geometry, function (i) i [[1]]))
    else if (is (bbox$geometry, "sfc_MULTIPOLYGON") |
             is (bbox$geometry, "sfc_MULTILINESTRING"))
      xy <- do.call (rbind, lapply (bbox$geometry,
                                    function (i) i [[1]] [[1]]))
    bbox <- t (apply (xy, 2, range))
    rownames (bbox) <- c ("x", "y")
    colnames (bbox) <- c ("min", "max")
  }
  if (!is.numeric (bbox))
    stop ('bbox is not numeric')
  if (length (bbox) < 4)
    stop ('bbox must have length = 4')
  if (length (bbox) > 4)
  {
    warning ('bbox has length > 4; only first 4 elements will be used')
    bbox <- matrix (bbox [1:4], 2, 2)
  }
  
  return (bbox)
}

osm_basemap2 <- function (bbox, structures, bg = 'gray20')
{
  # ---------------  sanity checks and warnings  ---------------
  bbox <- check_bbox_arg (bbox)
  if (!missing (structures))
  {
    check_structures_arg (structures)
    bg <- structure$cols [which (structures$structure == 'background')]
  }
  #check_col_arg (bg)
  if (length (bg) > 1)
  {
    warning ('bg has length > 1; only first element will be used')
    bg <- bg [1]
  }
  # ---------------  end sanity checks and warnings  ---------------
  
  map_theme <- set_map_theme2 (bg = bg)
  
  lon <- lat <- NA
  map <- ggplot2::ggplot () + map_theme +
    ## xlim stuff appears to need to be added with coord_sf at the end
    ## This causes warnings from the sf geom.
    #ggplot2::coord_map (xlim = range (bbox[1, ]),
    #                    ylim = range (bbox[2, ])) +
    ## don't need this as the sf geom deals with it.
    # ggplot2::aes(x = lon, y = lat)
    ggplot2::scale_x_continuous (expand = c(0, 0)) +
    ggplot2::scale_y_continuous (expand = c(0, 0))
  
  return (map)
}

set_map_theme2 <- function (bg)
{
  theme <- ggplot2::theme_minimal ()
  theme$panel.background <- ggplot2::element_rect (fill = bg, size = 0)
  theme$line <- ggplot2::element_blank ()
  theme$axis.text <- ggplot2::element_blank ()
  theme$axis.title <- ggplot2::element_blank ()
  theme$plot.margin <- ggplot2::margin (rep (ggplot2::unit (0, 'null'), 4))
  theme$plot.margin <- ggplot2::margin (rep (ggplot2::unit (-0.5, 'line'), 4))
  theme$legend.position <- 'none'
  theme$axis.ticks.length <- ggplot2::unit (0, 'null')
  
  return (theme)
}

## ---- SpatialGroupsFns ----
## better naming strategy needed
#' Add a group column to an osm sf data frame
#'
#' @details Names from the region list are used to construct factor levels. A 
#' background label is included. No checking to ensure regions don't overlap
#' @param osmdata 
#' @param regions a named list of sf polygons.
#' @param colname the output column name
#' 
#' @return the input dataframe with an extra column named colname
#' @export
#'
#' @examples
add_osm_group_column <- function(osmdata, regions, 
                                 membership=c("within", "overlaps"), 
                                 colname="SpatialGroups", 
                                 backgroundfactor="bg") {
  membership <- match.arg(membership)
  if (length(names(regions)) == 0) {
    stop("Regions must be a named list")
  }
  rlist <- do.call(sf::st_sfc, list(regions, crs=sf::st_crs(osmdata)))
  w <- sf::st_within(osmdata, rlist, sparse=FALSE)
  if (membership == "overlaps") {
    # also accepting objects that cross the boundary
    w <- w | sf::st_overlaps(osmdata, rlist, sparse=FALSE)
  }
  bg <- !apply(w, MARGIN=1, any)
  w <- cbind(bg, w)
  colnames(w) <- c(backgroundfactor, names(regions))
  
  osmdata[[colname]] <- factor(w %*% (1:ncol(w)), labels=colnames(w))
  return(osmdata)
}


#' split into groups, producing new data frame with objects broken at
#' group boundaries
#'
#' @param osmdata 
#' @param regions 
#' @param colname 
#' @param backgroundfactor 
#'
#' @details Probably fragile if groups are empty. Also if there are extra columns
#' in the input osmdata.
#' 
#' @return modified sf dataframe
#' @export
#'
#' @examples
split_osm_objects <- function(osmdata, regions, 
                               colname="SpatialGroups", 
                               backgroundfactor="bg") {
  if (length(names(regions)) == 0) {
    stop("Regions must be a named list")
  }
  rlist <- do.call(sf::st_sfc, list(regions, crs=sf::st_crs(osmdata)))
  w <- sf::st_within(osmdata, rlist, sparse=FALSE)
  tobreak <- sf::st_overlaps(osmdata, rlist, sparse=FALSE)

  bg.nosplit <- !apply(w|tobreak, MARGIN=1, any)
  
  breakbygroup <- lapply(1:length(regions), function(idx) {
    osmdata[ tobreak[,idx], ]
  })
  insidegroups <- lapply(1:length(regions), function(idx) {
    a <- st_intersection(breakbygroup[[idx]], rlist[idx])
    a[[colname]] <- names(regions)[idx]
    a
  })

  outsidegroups <- lapply(1:length(regions), function(idx) {
    a <- st_difference(breakbygroup[[idx]], st_geometry(insidegroups[[idx]]))
    a[[colname]] <- backgroundfactor
    a
  })
  insidegroups <- do.call(rbind, insidegroups)
  outsidegroups <- do.call(rbind, outsidegroups)
  ## group membership for the ones not being split
  w <- cbind(bg.nosplit, w)
  ## keep the rows that weren't split
  k <- rowSums(w) > 0
  
  colnames(w) <- c(backgroundfactor, names(regions))
  w <- w[k,]
  m1 <- as.character(factor(w %*% (1:ncol(w)), labels=colnames(w)))

  results.not.split <- osmdata[k,]
  results.not.split[[colname]] <- m1
  ## Now the objects that were split
  results.split <- rbind(insidegroups, outsidegroups)
  return(rbind(results.not.split, results.split))
}
## ---- SpatialGroupsEx1 ----
library(osmplotr)
library(ggplot2)
library(dplyr)
library(sf)
data(london)
dat_B <- rbind (london$dat_BNR, london$dat_BR)

bbox <- get_bbox (c(-0.13, 51.51, -0.11, 51.52))

pts <- cbind (c (-0.115, -0.125, -0.125, -0.115),
              c (51.513, 51.513, 51.517, 51.517))

#  Turn our corners into a polygon
pts.sf <- sf::st_polygon(list(rbind(pts, pts[1,])))

## generate the grouping column, and an area column for fun
datB_g1 <- add_osm_group_column(dat_B, list(G1=pts.sf))
datB_g1 <- mutate(datB_g1, AREA=st_area(geometry))

lon.map1 <- osm_basemap2 (bbox = bbox,
                          bg = 'gray20')
lon.map1+geom_sf(data=datB_g1, aes(fill=SpatialGroups), colour=NA) + 
  scale_fill_manual(values=c("gray40", "orange")) +
  coord_sf(xlim=range(bbox[1, ]), ylim=range (bbox[2, ])) +
  theme(panel.grid.major = element_line(colour = 'transparent')) 

## ---- SpatialGroupsEx2 ----
lon.map1+geom_sf(data=datB_g1, aes(fill=SpatialGroups, alpha=sqrt(as.numeric(AREA))), colour=NA) + 
  scale_fill_manual(values=c("gray40", "orange")) +
  coord_sf(xlim=range(bbox[1, ]), ylim=range (bbox[2, ])) +
  theme(panel.grid.major = element_line(colour = 'transparent')) 

## ---- SpatialGroupsEx3 ----
## one liner for this
datB_g1 <- mutate(datB_g1, fgArea = sqrt(as.numeric(AREA)))
datB_g1 <- within(datB_g1, {
  fgArea[SpatialGroups=="bg"] <- 1
})

lon.map1 + geom_sf(data=datB_g1, aes(fill=SpatialGroups, alpha=fgArea), colour=NA) + 
  scale_fill_manual(values=c("gray40", "orange")) +
  coord_sf(xlim=range(bbox[1, ]), ylim=range (bbox[2, ])) +
  theme(panel.grid.major = element_line(colour = 'transparent')) 

## ---- SpatialGroupsEx4 ----

lon.map1 +
  geom_sf(data=filter(datB_g1, SpatialGroups=="G1"), aes(alpha=fgArea), fill="orange", colour=NA) + 
  geom_sf(data=filter(datB_g1, SpatialGroups=="bg"), fill="NA", colour="gray50", size=0.1) + 
  coord_sf(xlim=range(bbox[1, ]), ylim=range (bbox[2, ])) +
   theme(panel.grid.major = element_line(colour = 'transparent')) 

## ---- MultiSpatialGroups ----
pts2 <- cbind (c (-0.111, -0.1145, -0.1145, -0.111),
               c (51.517, 51.517, 51.519, 51.519))
pts2.sf <- sf::st_polygon(list(rbind(pts2, pts2[1,])))

datB_g2 <- add_osm_group_column(dat_B, list(G1=pts.sf, G2=pts2.sf), membership="overlap")

datB_g2 <- mutate(datB_g2, AREA=st_area(geometry), 
                  fgArea=sqrt(as.numeric(AREA)))

## ---- MultiSpatialGroupsEx1 ----

lon.map2 <- osm_basemap2 (bbox = bbox,
                          bg = 'gray20')
lon.map2+geom_sf(data=datB_g2, aes(fill=SpatialGroups), colour=NA) + 
  scale_fill_manual(values=c("gray40", "orange", "tomato")) +
  coord_sf(xlim=range(bbox[1, ]), ylim=range (bbox[2, ])) +
  theme(panel.grid.major = element_line(colour = 'transparent')) 
## ---- MultiSpatialGroupsEx2 ----
lon.map2 +
  geom_sf(data=filter(datB_g2, SpatialGroups!="bg"), aes(alpha=fgArea, fill=SpatialGroups), colour=NA) + 
  scale_fill_manual(values=c("orange", "tomato")) +
  geom_sf(data=filter(datB_g2, SpatialGroups=="bg"), fill="NA", colour="gray60", size=0.1) + 
  coord_sf(xlim=range(bbox[1, ]), ylim=range (bbox[2, ])) +
  theme(panel.grid.major = element_line(colour = 'transparent')) 

## ---- MultiSpatialGroupsEx3 ----
lon.map2 +
  scale_size(range=c(0.1, 1)) + 
  geom_sf(data=filter(datB_g2, SpatialGroups!="bg"), 
          aes(alpha=fgArea, colour=SpatialGroups, size=fgArea), fill=NA) + 
  scale_colour_manual(values=c("orange", "tomato")) +
  geom_sf(data=filter(datB_g2, SpatialGroups=="bg"), fill="NA", colour="gray60", size=0.1) + 
  coord_sf(xlim=range(bbox[1, ]), ylim=range (bbox[2, ])) +
  theme(panel.grid.major = element_line(colour = 'transparent')) 

## ---- HighPrecGroupsA ----

datB_s2 <- split_osm_objects(dat_B, list(G1=pts.sf, G2=pts2.sf))

lon.map2 +
  scale_colour_manual(values=c("gray40", "orange", "tomato")) +
  geom_sf(data=datB_s2, aes(colour=SpatialGroups), fill=NA) + 
  coord_sf(xlim=range(bbox[1, ]), ylim=range (bbox[2, ])) +
  theme(panel.grid.major = element_line(colour = 'transparent')) 
  