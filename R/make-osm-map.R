#' make_osm_map
#'
#' Makes an entire OSM map for the given bbox by downloading all data.  This
#' function can take quite some time (tens of minutes) to execute the two
#' primary steps of downloading and processing the raw OSM data.  For this
#' reason, this function is not intended to be actually run; rather it serves to
#' demonstrate how all functions may be combined to generate a map.
#'
#' @param filename Name of plot file; default=NULL plots to screen device (low
#' quality and likely slow)
#' @param bbox The bounding box for the map.  Must be a vector of 4 elements
#' (xmin, ymin, xmax, ymax).  Default is a small part of central London
#' (-0.15,51.5,-0.1,51.52).  
#' @param structs A data.frame specifying types of OSM structures as returned
#' from osm_structures, and potentially modified to alter lists of structures to
#' be plotted, and their associated colours. The order of structs determines the
#' plot order of objects.
#' @param remove_data To save working memory, data for each type of structure
#' are temporarily saved to disk and removed from memory. If remove_data =
#' FALSE, saved objects are *NOT* removed from disk at end.
#' @return nothing (generates graphics device of specified type; progress is
#' dumped to screen, including time taken).
#' @examples
#' # An example of the resultant map can be reproduced using the "london" data:
#' \dontrun{
#'  data (london)
#'  structs <- osm_structures (col_scheme="dark")
#'  xylims <- get_xylims (london$dat_B)
#'  plot_osm_basemap (xylims=xylims, structures=structs)
#'  col <- as.character (structs$cols [which (structs$structures == "highway")])
#'  add_osm_objects (london$dat_H, col=col)
#'  col <- as.character (structs$cols [which (structs$structures == "building")])
#'  add_osm_objects (london$dat_B, col=col)
#'  col <- as.character (structs$cols [which (structs$structures == "amenity")])
#'  add_osm_objects (london$dat_A, col=col)
#'  col <- as.character (structs$cols [which (structs$structures == "park")])
#'  add_osm_objects (london$dat_P, col=col)
#'  add_osm_objects (london$dat_N, col=col)
#'  col <- as.character (structs$cols [which (structs$structures == "grass")])
#'  add_osm_objects (london$dat_G, col=col)
#' }

make_osm_map <- function (filename=NULL, bbox=c(-0.15,51.5,-0.1,51.52), 
                          structs=osm_structures (), remove_data=TRUE)
{
    ns <- nrow (structs)
    struct_list <- NULL

    cat ("Downloading and extracting OSM data for", ns, "structures ...\n")
    pb <- txtProgressBar (max=1, style = 3) # shows start and end positions
    t0 <- proc.time ()
    for (i in 1:ns) {
        dat <- extract_osm_objects (key=toString (structs$key [i]),
                                    value=toString (structs$value [i]),
                                   bbox=bbox)
        fname <- paste ("dat_", toString (structs$letters [i]), sep="")
        assign (fname, dat)
        save (list=c(fname), file=fname)
        struct_list <- c (struct_list, fname)
        rm (list=c(fname))
        setTxtProgressBar(pb, i / ns)
    }
    close (pb)
    cat ("That took ", (proc.time () - t0)[3], "s\n", sep="")

    if ("dat_BU" %in% struct_list & file.exists ("datBU")) 
    {
        load ("dat_BU")
        xylims <- get_xylims (datBU)
        rm ("dat_BU")
    } else if ("dat_H" %in% struct_list & file.exists ("datH"))
    {
        load ("dat_H")
        xylims <- get_xylims (datH)
        rm ("dat_H")
    } else 
        stop ("don't know what structure to use to calculate xylims.")

    plot_osm_basemap (xylims=xylims, filename=filename)
    for (i in seq (nrow (structs)))
    {
        fname <- paste ("dat_", structs$letters [i], sep="")
        data (get (fname))
        add_osm_objects (get (fname), col=as.character (structs$cols [i]))
        rm (list=c(fname))
    }

    if (remove_data)
        for (i in struct_list)
            file.remove (i)

    if (!is.null (filename)) 
        junk <- dev.off (which=dev.cur())
}
