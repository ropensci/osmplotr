# CRAN notes for osmplotr_v0.3.6 re-submission

This is a re-submission of a previously archived package. The submission may
generate one single note regarding installed size of ~6MB, which is due to the
vignettes. These produce many graphical files illustrating the package's
functionality. Every effort has been made to reduce this as much as possible,
including reducing the resolution of these images to the coarsest practicable
scale. Halving the resolution again (from current 72 to 36 dpi) only decreases
the final package size by around 200 kB.

This re-submission also rectifies several issues pointed out in an email from
7th Feb 2025:

- Return values have been documented in all functions
- Most `\dontrun` statements have been reduced, and converted to `donttest`. A
few nevertheless remain, as the example code makes external API calls to
download data. The following functions retain `dontrun` statements only around
code which makes external API calls:
  - `add_osm_groups()`
  - `adjust_colours()`,
  - `add_osm_objects()`
  - `extract_osm_objects()`
  - `osm_line2poly()`
  - `connect_highways()`.


## Test environments

Other than the above, this submission generates NO notes on:

- Linux (via github actions): R-release, R-oldrelease
- Windows (via github actions): R-release, R-oldrelease, R-devel
- win-builder: R-oldrelease, R-release, R-devel
