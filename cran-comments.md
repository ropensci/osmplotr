# CRAN notes for osmplotr_v0.3.0 submission

The single note regarding installed size of ~6MB is due to the vignettes. These
produce many graphical files illustrating the package's functionality. Halving
the current resolution of these images (from 72 to 36 dpi) only decreases the
final package size by around 200 kB - it really is the sheer number of them
required, rather than their sizes.

## Test environments

This submission generates NO notes on:
* Linux (via Travis-ci): R-oldrel, R-release, R-devel
* OSX (via Travis-ci): R-oldrel R-release
* Windows Visual Studio 2015 x64 (via appveyor)
* win-builder: R-oldrelease, R-release, R-devel

Package also checked using both local memory sanitzer and `rocker/r-devel-san`
with clean results. 
