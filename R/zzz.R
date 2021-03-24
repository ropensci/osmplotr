.onAttach <- function(libname, pkgname) {           # nolint
    msg <- paste0 ("Data (c) OpenStreetMap contributors, ",
                   "ODbL 1.0. http://www.openstreetmap.org/copyright")
    packageStartupMessage(msg)
}

.onLoad <- function(libname, pkgname) {         # nolint
    packageStartupMessage ("NOTE: The 'sf' package should be loaded to ",
                           "ensure that 'sf' methods work as expected.")
}
