# Code to do the calculations; alpi-surry.R contains functions (that need to be
# sourced first). This is the rest of the code in Surry and King (2015).

# Code to run the functions on real data
# This is a source file named ALPI_example.r
# Written for R 3.1.2
############################################################
# Code to run the ALPI functions on datafiles already downloaded
# from the UCAR website for 2011 - 2015. Will produce
# comma-delimited (csv) files containing the monthly and
# seasonal areas of the Aleutian Low, annual ALPI values, and
# coordinates for the center of the Aleutian Low for all the years
# included.
############################################################
# Source the ALPI functions
source("alpi-surry.r")
############################################################
# These are the files for each year (change path as necessary)
file2011 <- "./data/ds010.1.20101200.20110331.nc"
file2012 <- "./data/ds010.1.20111200.20120331.nc"
file2013 <- "./data/ds010.1.20121200.20130331.nc"
file2014 <- "./data/ds010.1.20131200.20140331.nc"
file2015 <- "./data/ds010.1.20141200.20150331.nc"
############################################################
# This is a vector containing all the years
year <- c(2011,2012,2013,2014,2015)
# This is the 1950-1997 mean
longterm <- 5.523704e+12
############################################################
# Calculate areas, indices, and coordinates
#2011
data2011 <- get.alpi.nc(file2011)
raster2011 <- sapply(data2011,slp.idw,cs=40)
vals2011 <- sapply(raster2011,month.val)
alpa2011 <- alpa(vals2011)
alpi2011 <- c(alpa2011,alpi.val(vals2011,lt.mean=longterm))
coords2011 <- seasonal.ctr(raster2011)
#
#2012
data2012 <- get.alpi.nc(file2012)
raster2012 <- sapply(data2012,slp.idw,cs=40)
vals2012 <- sapply(raster2012,month.val)
alpa2012 <- alpa(vals2012)
alpi2012 <- c(alpa2012,alpi.val(vals2012,lt.mean=longterm))
coords2012 <- seasonal.ctr(raster2012)
#
#2013
data2013 <- get.alpi.nc(file2013)
raster2013 <- sapply(data2013,slp.idw,cs=40)
vals2013 <- sapply(raster2013,month.val)
alpa2013 <- alpa(vals2013)
alpi2013 <- c(alpa2013,alpi.val(vals2013,lt.mean=longterm))
coords2013 <- seasonal.ctr(raster2013)
#
#2014
data2014 <- get.alpi.nc(file2014)
raster2014 <- sapply(data2014,slp.idw,cs=40)
vals2014 <- sapply(raster2014,month.val)
alpa2014 <- alpa(vals2014)
alpi2014 <- c(alpa2014,alpi.val(vals2014,lt.mean=longterm))
coords2014 <- seasonal.ctr(raster2014)
#
#2015
data2015 <- get.alpi.nc(file2015)
raster2015 <- sapply(data2015,slp.idw,cs=40)
vals2015 <- sapply(raster2015,month.val)
alpa2015 <- alpa(vals2015)
alpi2015 <- c(alpa2015,alpi.val(vals2015,lt.mean=longterm))
coords2015 <- seasonal.ctr(raster2015)
#
############################################################
#Save ALPI results to a text file in the current working directory
alpi.results <- data.frame(
year,rbind(alpi2011,alpi2012,alpi2013,alpi2014,alpi2015),
row.names = NULL)
write.table(alpi.results,
file="alpi2011-2015.csv",row.names=F,sep=",",quote=F)
#
#Save coordinates to a text file in the current working directory
coords.results <- rbind(
t(coords2011),
t(coords2012),
t(coords2013),
t(coords2014),
t(coords2015))
coords.results <- data.frame(
t(matrix(unlist(strsplit(dimnames(coords.results)[[1]]," ")),nrow=2)),
coords.results)
rownames(coords.results) <- NULL
colnames(coords.results) <- c("month","year","long","lat","x","y")
write.table(coords.results, file="coords2011-
2015.csv",row.names=F,sep=",",quote=F)
