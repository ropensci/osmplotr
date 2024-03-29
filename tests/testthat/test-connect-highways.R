context ("connect-highways")

# test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
#             identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))
test_all <- identical (Sys.getenv ("MPADGE_LOCAL"), "true")

source ("../stub.R")

if (curl::has_internet ()) { # otherwise all of these return errors not warnings

    test_that ("missing objects", {
        expect_error (
            connect_highways (),
            "A vector of highway names must be given"
        )
        expect_error (
            connect_highways ("aaa"),
            "A bounding box must be given"
        )
    })

    if (test_all) {

        test_that ("unrecognised highways", {
            bbox <- get_bbox (c (-0.13, 51.5, -0.11, 51.52))
            # No error specified because different HTML errors may
            # also be generated
            expect_error (connect_highways ("aaa", bbox))
            # expect_error (connect_highways ("aaa", bbox),
            #              "No data able to be extracted")
        })

        bbox <- get_bbox (c (-0.15, 51.5, -0.10, 51.52))
        highways <- c ("Monmouth.St", "Short.?s.Gardens")

        test_that ("extract_highways", {
            ways <- extract_highways (
                highway_names = highways,
                bbox = bbox
            )
            expect_is (ways, "list")
            expect_true (length (ways) > 1) # some might fail
            nms <- abbreviate_hwy_names (highways)
            expect_true (any (nms %in% names (ways)))
        })
    }
} # end if has_internet

# highway tests using internal data
load (system.file ("extdata", "hwys.rda", package = "osmplotr"))

test_that ("connect highways internal code", { # these all form cycles
    for (i in 1:3) {

        expect_silent (ways0 <- connect_single_ways (hwys [[1]]))
        expect_true (all (vapply (ways0, is.list, logical (1))))
        expect_silent (ways <- get_highway_cycle (ways0))
        # get_highway_cycle should add nodes:
        n0 <- sum (vapply (ways0, function (i) {
            nrow (do.call (rbind, i))
        }, numeric (1)))
        n <- sum (vapply (ways, function (i) {
            nrow (do.call (rbind, i))
        }, numeric (1)))
        expect_true (n > n0)

        conmat <- get_conmat (ways)
        expect_true (nrow (conmat) == length (ways))
        cyc <- ggm::fundCycles (conmat) [[1]]
        paths <- sps_through_cycle (ways, cyc)
        # paths should have fewer total nodes:
        expect_true (nrow (paths) < n0)
    }
})
