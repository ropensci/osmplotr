test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage"))

test_that ("missing objects", {
    expect_error (extract_osm_objects (), "key can not be NULL")
    expect_error (
        extract_osm_objects (key = "aaa"),
        "bbox must be provided"
    )
})

test_that ("key missing", {
    bbox <- get_bbox (c (-0.12, 51.51, -0.11, 51.52))
    expect_error (
        extract_osm_objects (bbox = bbox, key = NULL),
        "key can not be NULL"
    )
    expect_error (
        extract_osm_objects (bbox = bbox, key = NA),
        "key can not be NA"
    )
    expect_error (
        extract_osm_objects (bbox = bbox),
        "key can not be NULL"
    )
})

skip_if (!curl::has_internet ())
skip_if (!test_all)

test_that ("invalid key", {
    bbox <- get_bbox (c (-0.1265, 51.5145, -0.1263, 51.5147))
    expect_warning (
        suppressMessages (
            httptest2::with_mock_dir ("extract-invalid-key", {
                extract_osm_objects (bbox = bbox, key = "aaa")
            })
        ),
        "No valid data returned"
    )
})

test_that ("valid key", {
    bbox <- get_bbox (c (-0.1265, 51.5145, -0.1263, 51.5147))
    dat <- httptest2::with_mock_dir ("extract-buildings", {
        extract_osm_objects (bbox = bbox, key = "building")
    })
    expect_is (dat, "sf")

    expect_warning (
        dat <- httptest2::with_mock_dir ("extract-buildings-sp", {
            extract_osm_objects (
                bbox = bbox, key = "building",
                sf = FALSE
            )
        }),
        "\\'osmdata\\_sp \\(\\)\\' is deprecated"
    )
    expect_is (dat, "SpatialPolygonsDataFrame")
})

test_that ("extra_pairs", {
    key <- "building"
    value <- "yes"
    extra_pairs <- c ("amenity", "place_of_worship")
    bbox <- get_bbox (c (-0.1274, 51.5140, -0.1270, 51.5144))
    dat <- httptest2::with_mock_dir ("extract-bldg-extra-pair", {
        extract_osm_objects (
            bbox = bbox, key = key,
            value = value,
            extra_pairs = extra_pairs
        )
    })
    expect_true (nrow (dat) > 0)
    expect_is (dat, "sf")
})

test_that ("sp objects", {
    key <- "building"
    value <- "yes"
    extra_pairs <- c ("amenity", "place_of_worship")
    bbox <- get_bbox (c (-0.1274, 51.5140, -0.1270, 51.5144))
    expect_warning (
        dat <- httptest2::with_mock_dir ("extract-bldg-extra-pair-sp", {
            extract_osm_objects (
                bbox = bbox, key = key,
                value = value,
                extra_pairs = extra_pairs,
                sf = FALSE
            )
        }),
        "\\'osmdata\\_sp \\(\\)\\' is deprecated"
    )
    expect_true (nrow (dat) > 0)
    expect_is (dat, "Spatial")
})
