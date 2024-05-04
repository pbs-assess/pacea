# Create an object of depth values, based on grid26.
# These are currently just working note to keep track of ideas, jump to the end
#  to see latest thinking.

load_all()
library(PBSdata)   # devtools::install_github("pbs-software/pbs-data/PBSdata")
library(PBSmapping)

grid26   # sf object

vignette('PBSdata-UG', package = "PBSdata")
# Page 5 says:
# Figure 2. Data frame of sea floor topography (Smith and Sandwell, 1997) downloaded from
# TOPEX and reformatted for use by makeTopography in PBSmapping .
# and gives example code. Also
# ?bctopo
# gives the Smith and Sandwell 1997 reference.

# Probably better to use more recent data, and process it once (PBSdata already
# processed values).

# Philina suggested marmap. Looking at first vignette it talks about raster and
# sp packages (but not sf).

# Search on Canadian Hydrographic Service gives S102and111.zip from
#  https://www.charts.gc.ca/data-gestion/index-eng.html
#  S102/ contains .h5 files. Can read these in using the rhdf5 package
#  https://www.bioconductor.org/packages/release/bioc/vignettes/rhdf5/inst/doc/rhdf5.html
# install.packages("BiocManager")
# BiocManager::install("rhdf5")

library(rhdf5)    # vignette at
# https://www.bioconductor.org/packages/release/bioc/vignettes/rhdf5/inst/doc/rhdf5.html
a <- H5Fopen("S102_S111sample/S102/102CA0024600N07100W.h5")

# CHS website also says they supply data to GEBCO, which is what Kelsey had
# mentioned:
# https://www.charts.gc.ca/data-gestion/index-eng.html    links to
# https://www.gebco.net/
# OR they also have direct data from OpenData or the Federal Geospatial Platform
# (Govt employees only, but that's fine for us here).

# See the options from
# https://www.charts.gc.ca/data-gestion/index-eng.html
#  in the Bathymetry tab. OpenData link looks best.

# Kelsey going to help me. Cole made suggestions, added to Issue #48.

# From:
# https://open.canada.ca/data/en/dataset/e6e11b99-f0cc-44f7-f5eb-3b995fb1637e/resource/34d89346-527b-4723-9140-f4782cc384fb
# canada_west_coast_DEM_original.gdb. zip - 7 Gb file, taking an hour to download.
