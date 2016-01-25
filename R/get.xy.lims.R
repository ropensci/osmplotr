#' get.xylims
#'
#' Extracts lat-lon limits from a set of polygons returned by get.osm.polygons()
#'
#' @param poly a polygon object returned by get.osm.polygons ()
#' @param aspect1 
#' @return list of lat-lon ranges

get.xylims <- function (poly, aspect1=TRUE)
{
  xrange <- range (unlist (lapply (slot (poly, "polygons"),  
                    function (x) min (slot (x, "labpt") [1]))))
  yrange <- range (unlist (lapply (slot (poly, "polygons"),  
                    function (x) min (slot (x, "labpt") [2]))))
  if (aspect1) {
    if (diff (xrange) > diff (yrange)) {
      r <- diff (xrange) / diff (yrange)
      yrange [1] <- yrange [1] - r * diff (yrange) / 2
      yrange [2] <- yrange [2] + r * diff (yrange) / 2
    } else {
      r <- diff (yrange) / diff (xrange)
      xrange [1] <- xrange [1] - r * diff (xrange) / 2
      xrange [2] <- xrange [2] + r * diff (xrange) / 2
    }
  }
  return (list (x=xrange, y=yrange))
}

