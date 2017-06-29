# CRAN notes for osmplotr_v0.3.0 submission

The single note regarding installed size is due to the vignettes. These produce
many graphical files representing the package's functionality. I have
endeavoured to make these files as small as possible, but the 'doc' fold
nevertheless remains ~5MB.

## Test environments

This submission generates NO notes on:
* Linux (via Travis-ci): R-oldrel, R-release, R-devel
* OSX (via Travis-ci): R-oldrel R-release
* Windows Visual Studio 2015 x64 (via appveyor)
* win-builder: R-oldrelease, R-release, R-devel

Package also checked using both local memory sanitzer and `rocker/r-devel-san`
with clean results. 
