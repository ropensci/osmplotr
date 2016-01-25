#' get.colours
#'
#' Returns a list of sample colours for all object types listed in
#' get.osm.structures () which may be used to plot an OSM map. These are the
#' default values used in "make.osm.map".
#'
#' @return list of colours

get.colours <- function ()
{
    structs <- get.suffixes ()
    col.bg <- "gray20"
    # All colours are repeats of these three:
    colBU <- "gray40"
    col.grass <- rgb (100, 120, 100, maxColorValue=255)
    col.grass <- "gray30"
    col.water <- rgb (77, 77, 92, maxColorValue=255)

    colA <- colBU
    colW <- col.water
    colG <- colN <- colP <- col.grass
    colH <- "black"
    colBO <- "white"

    return (list (colBU=colBU, colA=colA, colW=colW, colG=colG, colN=colN,
                  colP=colP, colH=colH, colBO=colBO))
}
