#' make.osm.map
#'
#' Makes an entire OSM map for the given bbox by downloading and saving all
#' data. Polygons are saved as "polyX", where "X" is a suffix returned from
#' get.osm.structures().  These lists of polygons are intended for re-use and
#' are thus *NOT* deleted.  This function can take quite some time to execute
#' the two primary steps of downloading and processing the raw OSM data.  For
#' this reason, this function is not intended to be actually run; rather it
#' serves to demonstrate how all functions may be combined to generate a map.
#'
#' @param filename = name of plot file; default=NULL plots to screen device (low
#' quality and likely slow)
#' @param bbox = the bounding box for the map. Default is central London
#' (-0.15,51.5,-0.1,51.52).  
#' @param roads = TRUE (default) plot lines for roads (ways)
#' @param save.raw.data (default=FALSE) if true, also saves the raw data
#' structures as "datX" with suffixes as for "polyX" described above.
#' @param cols = a list of colours matching all object types listed in
#' get.suffixes (); defaults to values returned by get.colours ()
#' @return nothing (generates graphics device of specified type; progress is
#' dumped to screen, including time taken).

make.osm.map <- function (filedump=NULL, bbox=c(-0.15,51.5,-0.1,51.52), roads=TRUE,
                     save.raw.data=FALSE, cols=get.colours ())
{
    # Extend any submitted colours to required length of 8 types returned from
    # get.suffixes ():
    while (length (cols) < dim (get.suffixes ()) [1])
        cols <- rep (cols, 2)
    cols <- cols [1:dim (get.suffixes ()) [1]]

    osm.structs <- get.osm.structs ()
    st0 <- Sys.time ()
    cat ("Downloading raw data ...\n", rep ("-", 50), "\n", sep="")
    fnames <- NULL
    for (i in 1:dim (osm.structs) [1]) {
        cat ("Dowloading ", toString (osm.structs$dat.types [i]), 
             " \t... ", sep="")
        dat <- get.osm.data (type=toString (osm.structs$dat.types [i]),
                             bbox=bbox)
        st <- timetaken (st0)
        cat (" done; time = ", st, "\n", sep="")
        fname <- paste ("dat", toString (osm.structs$letters [i]), sep="")
        fnames <- c (fnames, fname) # used in subsequent processing stage
        assign (fname, dat)
        if (save.raw.data)
            save (list=c(fname), file=fname)
    }

    cat ("\nProcessing raw data ...\n", rep ("-", 50), "\n", sep="")
    st0 <- Sys.time ()
    for (i in 1:dim (osm.structs) [1]) {
        cat ("Processing ", toString (osm.structs$dat.types [i]), " \t... ", sep="")
        dat <- get.polys (get (fnames [i]), type=toString (osm.structs$dat.types [i]))
        st <- timetaken (st0)
        cat (" done; time = ", st, "\n", sep="")
        fname <- paste ("poly", toString (osm.structs$letters [i]), sep="")
        assign (fname, dat)
        save (list=c(fname), file=fname)
    }
    for (i in 1:dim (osm.structs) [1]) {
        fname <- paste ("poly", toString (osm.structs$letters [i]), sep="")
        load (fname)
    }

    xylims <- get.xylims (polyBU, aspect1=FALSE)

    # Streets are plotted at base, so first polygons are default, to be re-plotted
    # later.
    plot.polys (polyN, xylims, filedump=filedump, col=cols [["colN"]])
    if (roads)
    {
        add.lines (polyH, col=cols [["colH"]]) # highways   
        add.lines (polyBO, col=cols [["colBO"]]) # boundaries
    }
    # Then amenities, grass and parks:
    add.polys (polyA, col=cols [["colA"]])
    add.polys (polyG, col=cols [["colG"]])
    add.polys (polyP, col=cols [["colP"]])
    # Then buildings:
    add.polys (polyBU, col=cols [["colBU"]])
    # Water has to be plotted last to go over the top of grass and parks   
    add.polys (polyW, col=cols [["colW"]])
    add.polys (polyN, col=cols [["colN"]])
    if (!is.null (filename)) 
        junk <- dev.off (which=dev.cur())

    st <- timetaken (st0)
    cat ("Time taken = ", st, "\n", sep="")
}
