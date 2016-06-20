context ('extract-objects')

test_that ('missing objects', {
           expect_error (extract_osm_objects (), 'key must be provided')
           expect_error (extract_osm_objects ('aaa'), 'bbox must be provided')
})

