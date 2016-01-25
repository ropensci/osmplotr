#' get.osm.data
#'
#' Downloads OSM data for selected object type from the MapQuest api.
#'
#' @param type: currently recognises "building", "waterway", "natural", "grass",
#' "park", "amenity", "shop", "boundary", and "highway". Others will be simply
#' passed to "way[type=*]", which may or may not work.
#' @param bbox = the bounding box within which all objects of the given type
#' should be downloaded. Default is central London (-0.15,51.5,-0.1,51.52).
#' @return The contents of

get.osm.data <- function (type="building", bbox=c(-0.15,51.5,-0.1,51.52))
{
    stopifnot (is.numeric (bbox))
    stopifnot (length (bbox) == 4)
    if (bbox [3] < bbox [1])
        bbox <- bbox [c (3, 2, 1, 4)]
    if (bbox [4] <- bbox [2])
        bbox <- bbox [c (1, 4, 3, 2)]
    bbox <- paste ("[bbox=", bbox [1], ",", bbox [2], ",",
                   bbox[3], ",", bbox [4], "]", sep="")

    url.base <- "http://open.mapquestapi.com/xapi/api/0.6/"
    if (type == "building") 
    {
        dat <- getURL (paste (url.base, "way[building=*]", bbox, sep=""))
    } else if (type == "waterway") 
    {
        dat <- getURL (paste (url.base, "way[waterway=*]", bbox, sep=""))
    } else if (type == "natural") 
    {
        dat <- getURL (paste (url.base, "way[natural=water]", bbox, sep=""))
    } else if (type == "grass") 
    {
        dat <- getURL (paste (url.base, "way[landuse=grass]", bbox, sep=""))
    } else if (type == "park") 
    {
        dat <- getURL (paste (url.base, "way[leisure=park]", bbox, sep=""))
    } else if (type == "amenity") 
    {
        dat <- getURL (paste (url.base, "way[amenity=*]", bbox, sep=""))
    } else if (type == "shop") 
    {
        dat <- getURL (paste (url.base, "way[shop=*]", bbox, sep=""))
    } else if (type == "boundary") 
    {
        dat <- getURL (paste (url.base, "way[boundary=*]", bbox, sep=""))
    } else if (type == "highway") 
    {
        dat <- getURL (paste (url.base, "way[highway=*]", bbox, sep=""))
    } else 
    {
        url.str <- paste (url.base, "way[", type, "=*]", bbox, sep="")
        try ( dat <- getURL (url.str) )
    }

    if (is.null (dat))
        warning ("No data downloaded.")

    return (dat)
}

