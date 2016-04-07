context ("add-objects")

test_that ("no basemap", {
           graphics.off ()
           expect_error (add_osm_objects (),
                     "add_osm_objects can only be called after plot_osm_basemap")
           expect_error (add_osm_objects (london$dat_H),
                     "add_osm_objects can only be called after plot_osm_basemap")
})

test_that ("no objects", {
           plot.new ()
           expect_error (add_osm_objects (), 
                         "no object passed to add_osm_objects")
           expect_error (add_osm_objects ("a"), "obj is not a spatial class")
           graphics.off ()
})
