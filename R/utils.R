#' Re-project on to Lambert Azimuthal equal-area projection, centred at center
#' of actual objects.
#' @noRd
reproj_equal_area <- function (x) {

    xy <- sf::st_coordinates (x)
    xy <- round (apply (xy, 2, mean))

    crs <- sf::st_crs (paste0 (
        "+proj=laea +lat_0=",
        xy [1],
        " +lon_0=",
        xy [2],
        " +x_0=4321000 +y_0=3210000 +ellps=GRS80 ",
        "+towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
    ))

    sf::st_transform (x, crs = crs)
}
