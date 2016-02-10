#' osm_structures
#'
#' For the given vector of structure types returns a data.frame containing two
#' columns of corresponding OpenStreetMap key-value pairs, one column of
#' unambiguous suffices to be appended to the objects returned by get_osm_data
#' (), and one column specifying colours. This data frame may be subsequently
#' modified as desired, and ultimately passed to make_osm_map to automate map
#' production.
#'
#' @param structures The vector of types of structures (defaults listed in
#' ?extract_osm_objects).
#' @param col_scheme Colour scheme for the plot (current options include "dark"
#' and "light")
#' @return List of structures, corresponding prefixes, and colours.

osm_structures <- function (structures = c ("building", "amenity", "waterway",
                         "grass", "natural", "park", "highway", "boundary",
                         "tree"), col_scheme="dark")
{
    # Set up key-value pairs:
    keys <- structures
    values <- rep ("", length (keys))
    if (any (structures == "grass"))
    {
        keys [structures == "grass"] <- "landuse"
        values [structures == "grass"] <- "grass"
    }
    if (any (structures == "park"))
    {
        keys [structures == "park"] <- "leisure"
        values [structures == "park"] <- "park"
    }
    if (any (structures == "tree"))
    {
        keys [structures == "tree"] <- "natural"
        values [structures == "tree"] <- "tree"
    }
    if (any (structures == "water"))
    {
        keys [structures == "water"] <- "natural"
        values [structures == "water"] <- "water"
    }

    # Get 2-letter prefixes for naming data objects
    lettrs <- sapply (structures, function (x) toupper (substr (x, 1, 1)))
    # matches returns a list of matches for each letter, with the subsequent
    # indx marking any replications.
    matches <- sapply (lettrs, function (x) which (lettrs %in% x))
    indx <- which (sapply (matches, function (x) length (x) > 1))
    if (length (indx) > 0) 
        for (i in 1:length (indx)) 
        {
            indx_i <- matches [[indx [i]]] # Only need to extract first element
            lettrs [indx_i] <- toupper (substr (structures [indx_i], 1, 2))
        }

    # Color scheme:
    if (col_scheme == "dark")
    {
        col_bg <- "gray20"
        col_green <- rgb (100, 120, 100, 255, maxColorValue=255)
        col_green_bright <- rgb (100, 160, 100, 255, maxColorValue=255)
        col_blue <- rgb (100, 100, 120, 255, maxColorValue=255)
        col_gray1 <- rgb (100, 100, 100, 255, maxColorValue=255)
        col_gray2 <- rgb (120, 120, 120, 255, maxColorValue=255)
        col_white <- rgb (200, 200, 200, 255, maxColorValue=255)
        col_black <- rgb (0, 0, 0, 255, maxColorValue=255)
        cols <- rep (col_bg, length (structures))
        cols [structures == "building"] <- col_gray1
        cols [structures == "amenity"] <- col_gray2
        cols [structures == "waterway"] <- col_blue
        cols [structures == "natural"] <- col_green
        cols [structures == "park"] <- col_green
        cols [structures == "tree"] <- col_green_bright
        cols [structures == "grass"] <- col_green_bright
        cols [structures == "highway"] <- col_black
        cols [structures == "boundary"] <- col_white
    } else if (col_scheme == "light")
    {
        col_bg <- "gray95"
        col_green <- rgb (200, 220, 200, 255, maxColorValue=255)
        col_green_bright <- rgb (200, 255, 200, 255, maxColorValue=255)
        col_blue <- rgb (200, 200, 220, 255, maxColorValue=255)
        col_gray1 <- rgb (200, 200, 200, 255, maxColorValue=255)
        col_gray2 <- rgb (220, 220, 220, 255, maxColorValue=255)
        col_white <- rgb (255, 255, 255, 255, maxColorValue=255)
        col_black <- rgb (150, 150, 150, 255, maxColorValue=255)
        cols <- rep (col_bg, length (structures))
        cols [structures == "building"] <- col_gray1
        cols [structures == "amenity"] <- col_gray2
        cols [structures == "waterway"] <- col_blue
        cols [structures == "natural"] <- col_green
        cols [structures == "park"] <- col_green
        cols [structures == "tree"] <- col_green_bright
        cols [structures == "grass"] <- col_green_bright
        cols [structures == "highway"] <- col_black
        cols [structures == "boundary"] <- col_white
    }
    # Then add row to designate background colour (this has to be done prior to
    # data.frame construction, because cols are converted there to factors):
    structures <- c (structures, "background")
    keys <- c (keys, "")
    values <- c (values, "")
    lettrs <- c (lettrs, "")
    cols <- c (cols, col_bg)
    
    dat <- data.frame (cbind (structures, keys, values, lettrs, cols))
    row.names (dat) <- NULL
    names (dat) <- c ("structures", "key", "value", "letters", "cols")
    return (dat)
}

