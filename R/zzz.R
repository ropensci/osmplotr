.onAttach <- function(libname, pkgname) {           # nolint
    msg <- paste0 ("Data (c) OpenStreetMap contributors, ",
                   "ODbL 1.0. http://www.openstreetmap.org/copyright")
    packageStartupMessage(msg)
}

.onLoad <- function(libname, pkgname) {         # nolint
    if (!requireNamespace ("sf")) {

        packageStartupMessage ("NOTE: 'sf' package must be loaded for 'sf' ",
                               "methods to work")
    }
}
