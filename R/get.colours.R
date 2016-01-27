#' get.colours
#'
#' Returns a list of sample colours for all object types listed in get.suffixes
#' which may be used to plot an OSM map. These are the default values used in
#' "make.osm.map".
#'
#' @return list of colours

get.colours <- function (structs=get.suffixes ())
{
    # TODO: Expand this to offer more colour schemes
    structs <- cbind (structs, NA)
    names (structs)[3] <- "col"

    # ---------- Set the colours ----------
    col.bg <- "gray20"
    col.grass <- rgb (100, 120, 100, maxColorValue=255)
    #col.grass <- "gray30"
    col.water <- rgb (77, 77, 92, maxColorValue=255)
    colBU <- "gray40" # buildings and water
    # -------------------------------------

    # Then just apply them
    structs$col [structs$dat.types == "building"] <- colBU
    structs$col [structs$dat.types == "amenity"] <- colBU
    structs$col [structs$dat.types == "waterway"] <- col.water
    structs$col [structs$dat.types == "grass"] <- col.grass
    structs$col [structs$dat.types == "natural"] <- col.grass
    structs$col [structs$dat.types == "park"] <- col.grass
    structs$col [structs$dat.types == "highway"] <- "black"
    structs$col [structs$dat.types == "boundary"] <- "white"

    return (structs$col)
}
