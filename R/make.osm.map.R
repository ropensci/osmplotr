#' make.osm.map
#'
#' Makes an entire OSM map for the given bbox by downloading all data.  This
#' function can take quite some time (tens of minutes) to execute the two
#' primary steps of downloading and processing the raw OSM data.  For this
#' reason, this function is not intended to be actually run; rather it serves to
#' demonstrate how all functions may be combined to generate a map.
#'
#' @param filename = name of plot file; default=NULL plots to screen device (low
#' quality and likely slow)
#' @param bbox = the bounding box for the map. Default is central London
#' (-0.15,51.5,-0.1,51.52).  
#' @param roads = TRUE (default) plot lines for roads (ways)
#' @param cols = a list of colours matching all object types listed in
#' get.suffixes (); defaults to values returned by get.colours ()
#' @param remove.data = TRUE. To save working memory, data for each type of
#' structure are temporarily saved to disk and removed from memory. If
#' remove.data = FALSE, saved objects are *NOT* removed at end.
#' @return nothing (generates graphics device of specified type; progress is
#' dumped to screen, including time taken).
#' @examples
#' # These illustrate the key steps of the function:
#' bbox <- c (-0.15, 51.5, -0.1, 51.52)
#' datBU <- extract.osm.objects (bbox=bbox, key="building")
#' plot.osm.basemap (xylims=get.xylims (datBU))
#' add.osm.object (datBU, col="red")

make.osm.map <- function (filename=NULL, bbox=c(-0.15,51.5,-0.1,51.52), roads=TRUE,
                          save.raw.data=FALSE, cols=get.colours (),
                          remove.data=TRUE)
{
    # Extend any submitted colours to required length of 8 types returned from
    # get.suffixes ():
    while (length (cols) < dim (get.suffixes ()) [1])
        cols <- rep (cols, 2)
    cols <- cols [1:dim (get.suffixes ()) [1]]

    osm.structs <- get.suffixes ()
    ns <- dim (osm.structs) [1]
    struct.list <- NULL

    cat ("Downloading and extracting OSM data for ", ns, " structures ...\n")
    pb <- txtProgressBar (max=1, style = 3) # shows start and end positions
    t0 <- proc.time ()
    for (i in 1:ns) {
        dat <- extract.osm.objects (key=toString (osm.structs$dat.types [i]),
                                   bbox=bbox)
        fname <- paste ("dat", toString (osm.structs$letters [i]), sep="")
        assign (fname, dat)
        save (list=c(fname), file=fname)
        struct.list <- c (struct.list, fname)
        rm (list=c(fname))
        setTxtProgressBar(pb, i / ns)
    }
    close (pb)
    cat ("That took ", (proc.time () - t0)[3], "s\n", sep="")

    if ("datBU" %in% struct.list & file.exists ("datBU")) 
    {
        load ("datBU")
        xylims <- get.xylims (datBU)
        rm ("datBU")
    } else if ("datH" %in% struct.list & file.exists ("datH"))
    {
        load ("datH")
        xylims <- get.xylims (datH)
        rm ("datH")
    } else 
        stop ("don't know what structure to use to calculate xylims.")

    plot.osm.basemap (xylims=xylims, filename=filename)
    # The plot order is determined by the following indx, starting with
    # highways, although boundaries are removed here from the struct.list
    struct.list <- struct.list [struct.list != "datBO"]
    indx <- NULL
    if (roads)
        indx <- c (indx, which (struct.list %in% c ("datH", "datBO")))
    # Then anemities, grass, and parks:
    indx <- c (indx, which (struct.list %in% c ("datA", "datG", "datP")))
    #  buildings
    indx <- c (indx, which (struct.list == "datBU"))
    # and finally water
    indx <- c (indx, which (struct.list %in% c ("datW", "datN")))
    suffix <- osm.structs$letters [indx]
    types <- paste (osm.structs$dat.types [indx])
    for (i in seq (suffix))
    {
        fname <- paste ("dat", suffix [i], sep="")
        load (fname)
        add.osm.objects (get (fname), col=cols [osm.structs$dat.types == types [i]]) 
        rm (list=c(fname))
    }

    if (remove.data)
        for (i in struct.list)
            file.remove (i)

    if (!is.null (filename)) 
        junk <- dev.off (which=dev.cur())
}
