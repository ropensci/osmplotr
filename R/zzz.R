.onAttach <- function(libname, pkgname) {
    msg <- paste0 ('Data (c) OpenStreetMap contributors, ',
                   'ODbL 1.0. http://www.openstreetmap.org/copyright')
    if (!requireNamespace ("sf"))
        msg <- paste0 (msg, "\nNOTE: 'sf' package must be loaded for 'sf' methods ",
                       "to work")
    packageStartupMessage(msg)
}
