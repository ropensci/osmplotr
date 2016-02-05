#' make_osm_map
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
#' @param bbox = the bounding box for the map.  Must be a vector of 4 elements
#' (xmin, ymin, xmax, ymax).  Default is a small part of central London.
#' @param roads = TRUE (default) plot lines for roads (ways)
#' @param cols = a list of colours matching all object types listed in
#' get.suffixes (); defaults to values returned by get.colours ()
#' @param remove_data = TRUE. To save working memory, data for each type of
#' structure are temporarily saved to disk and removed from memory. If
#' remove_data = FALSE, saved objects are *NOT* removed at end.
#' @return nothing (generates graphics device of specified type; progress is
#' dumped to screen, including time taken).
#' @examples
#' # These illustrate the key steps of the function:
#' \dontrun{
#'  bbox <- c (-0.15, 51.51, -0.14, 51.52)
#'  datBU <- extract_osm_objects (bbox=bbox, key="building")
#'  datH <- extract_osm_objects (bbox=bbox, key="highway")
#'  datG <- extract_osm_objects (bbox=bbox, key="grass")
#'  datP <- extract_osm_objects (bbox=bbox, key="park")
#'  datW <- extract_osm_objects (bbox=bbox, key="water")
#' }
#' plot_osm_basemap (xylims=get_xylims (datBU))
#' add_osm_objects (datH, col="white")
#' add_osm_objects (datBU, col="orange")
#' add_osm_objects (datG, col="lawngreen")
#' add_osm_objects (datP, col="lawngreen")

make_osm_map <- function (filename=NULL, bbox=c(-0.15,51.5,-0.1,51.52), roads=TRUE,
                          cols=get_colours (), remove_data=TRUE)
{
    # Extend any submitted colours to required length of 8 types returned from
    # get_suffixes ():
    if (length (cols) < dim (get_suffixes ()) [1])
        cols <- rep (cols, dim (get_suffixes ()) [1])

    osm_structs <- get_suffixes ()
    ns <- dim (osm_structs) [1]
    struct_list <- NULL

    cat ("Downloading and extracting OSM data for ", ns, " structures ...\n")
    pb <- txtProgressBar (max=1, style = 3) # shows start and end positions
    t0 <- proc.time ()
    for (i in 1:ns) {
        dat <- extract_osm_objects (key=toString (osm_structs$dat.types [i]),
                                   bbox=bbox)
        fname <- paste ("dat", toString (osm_structs$letters [i]), sep="")
        assign (fname, dat)
        save (list=c(fname), file=fname)
        struct_list <- c (struct_list, fname)
        rm (list=c(fname))
        setTxtProgressBar(pb, i / ns)
    }
    close (pb)
    cat ("That took ", (proc.time () - t0)[3], "s\n", sep="")

    if ("datBU" %in% struct_list & file.exists ("datBU")) 
    {
        load ("datBU")
        xylims <- get_xylims (datBU)
        rm ("datBU")
    } else if ("datH" %in% struct_list & file.exists ("datH"))
    {
        load ("datH")
        xylims <- get_xylims (datH)
        rm ("datH")
    } else 
        stop ("don't know what structure to use to calculate xylims.")

    plot_osm_basemap (xylims=xylims, filename=filename)
    # The plot order is determined by the following indx, starting with
    # highways, although boundaries are removed here from the struct_list
    struct_list <- struct_list [struct_list != "datBO"]
    indx <- NULL
    if (roads)
        indx <- c (indx, which (struct_list %in% c ("datH", "datBO")))
    # Then anemities, grass, and parks:
    indx <- c (indx, which (struct_list %in% c ("datA", "datG", "datP")))
    #  buildings
    indx <- c (indx, which (struct_list == "datBU"))
    # and finally water
    indx <- c (indx, which (struct_list %in% c ("datW", "datN")))
    suffix <- osm_structs$letters [indx]
    types <- paste (osm_structs$dat.types [indx])
    for (i in seq (suffix))
    {
        fname <- paste ("dat", suffix [i], sep="")
        load (fname)
        add_osm_objects (get (fname), col=cols [osm_structs$dat.types == types [i]]) 
        rm (list=c(fname))
    }

    if (remove_data)
        for (i in struct_list)
            file.remove (i)

    if (!is.null (filename)) 
        junk <- dev.off (which=dev.cur())
}
