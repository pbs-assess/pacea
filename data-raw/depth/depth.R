# Create an object of depth values, based on grid26.

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
