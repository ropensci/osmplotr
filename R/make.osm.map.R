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

make.osm.map <- function (filename=NULL, bbox=c(-0.15,51.5,-0.1,51.52), roads=TRUE,
                          save.raw.data=FALSE, cols=get.colours (),
                          remove.data=FALSE)
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
    # Streets are plotted at base, so all other obbjects overlay them
    if (roads)
    {
        if (file.exists ("datH"))
        {
            load ("datH")
            add.osm.objects (datH, col=cols [osm.structs$dat.types == "highway"]) 
            rm ("datH")
        }
        if (file.exists ("datBO"))
        {
            load ("datBO")
            add.osm.objects (datBO, col=cols [osm.structs$dat.types == "boundary"]) 
            rm ("datBO")
        }
    }
    # Then amenities, grass and parks:
    if (file.exists ("datA"))
    {
        load ("datA")
        add.osm.objects (datA, col=cols [osm.structs$dat.types == "amenity"]) 
        rm ("datA")
    }
    if (file.exists ("datG"))
    {
        load ("datG")
        add.osm.objects (datG, col=cols [osm.structs$dat.types == "grass"]) 
        rm ("datG")
    }
    if (file.exists ("datP"))
    {
        load ("datP")
        add.osm.objects (datP, col=cols [osm.structs$dat.types == "park"]) 
        rm ("datP")
    }
    # Then buildings:
    if (file.exists ("datBU"))
    {
        load ("datBU")
        add.osm.objects (datBU, col=cols [osm.structs$dat.types == "building"]) 
        rm ("datBU")
    }
    # Water has to be plotted last to go over the top of grass and parks   
    if (file.exists ("datW"))
    {
        load ("datW")
        add.osm.objects (datW, col=cols [osm.structs$day.types == "water"]) 
        rm ("datW")
    }
    if (file.exists ("datN"))
    {
        load ("datN")
        add.osm.objects (datN, col=cols [osm.structs$day.types == "natural"]) 
        rm ("datN")
    }
    if (!is.null (filename)) 
        junk <- dev.off (which=dev.cur())

    if (remove.data)
    {
        # TODO: Remove data
    }
}
