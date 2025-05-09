# ROUTINE - point2rast
# interpolates coordinate point data from dataframe, matrix, or spatial object (sf, sp)
#  to raster (terra)
# arguments: data = spatial object point data, dataframe, or matrix
#            spatobj = spatial object or extent for raster grid 
#            loc = vector of coordinate names in dataframe or matrix of coordinate values; optional where coordinates are implicit in data (e.g. sf)
#            cellsize = vector specifying grid cell resolution
#            nnmax = max nearest neighbour values to interpolate from
#            as = output as SpatRaster or SpatVector
# 
# output: SpatRaster or SpatVector
#  
# requires: terra, gstat
# 

point2rast <- function(data, spatobj, loc = c("x", "y"), cellsize, nnmax = 4, 
                       as = c("SpatRast","SpatVect")) {
  
  # WARNING: cellsize error
  stopifnot("must provide cellsize value" = exists("cellsize"))
  stopifnot("must specify valid value for 'as'" = as %in% c("SpatRast", "SpatVect"))
  
  # if data are entered in as single vector, convert to matrix
  if(!length(dim(data))) { data <- as.matrix(data) }
  
  # if dataframe or matrix
  if(!any(sapply(c("sf", "Spatial"), function(cl) is(data, cl)))) {
    
    # loc must be specified as vector of column names or matrix/dataframe of coordinates
    # if loc is a vector specifying column names
    if(!length(dim(loc))){
      
      stopifnot("loc vector must of be of length==2 " = length(loc)==2)
      stopifnot("loc names not found in data" = loc %in% names(data))
      
      coords <- setNames(as.data.frame(data[, loc]), c("x", "y"))
      
      tdat <- as.data.frame(data[, -which(colnames(data) %in% loc), drop=F])
      
    } else { # if loc is a matrix of coordinates
      
      stopifnot("loc data must be a matrix or dataframe of two columns" = 
                  length(dim(loc)) == 2 & dim(loc)[2] == 2)
      stopifnot("loc data is not of equal length to data" = nrow(data) == nrow(loc))
      
      coords <- setNames(as.data.frame(loc), c("x", "y"))
      
      tdat <- as.data.frame(data[, !colnames(data) %in% colnames(loc)])
    }
  }
  
  # if data are spatial, can get loc from the geometry/coordinates 
  #   TEST OUT SPATIAL DATA THAT ARE !POINTS (E.G. LINE, POLYGON)
  #   also test SpatVector file
  
  #   TEST TO CHECK THAT crs ARE THE SAME BETWEEN DATA AND POLYGON
  
  if(is(data, "Spatial")) {
    
    coords <- setNames(as.data.frame(data)[,c("coords.x1", "coords.x2")], c("x", "y"))
    
    tdat <- as.data.frame(data)[, names(data), drop=F]    
  }
  if(is(data, "sf")) {
    
    coords <- setNames(as.data.frame(matrix(unlist(data$geometry), ncol=2, byrow=T)), c("x", "y"))
    
    tdat <- as.data.frame(data)[, -which(names(data) == "geometry")]
  }
  
  # test if coordinates fall within extent of spatobj
  tbb <- ext(spatobj)
  if(!any(coords$x >= tbb$xmin & coords$x <= tbb$xmax & 
          coords$y >= tbb$ymin & coords$y <= tbb$ymax)) {
    warning("loc coordinates within spatobj extent = 0; check crs or extent of spatobj")
  }
  
  # create empty raster based on spatial object
  r <- rast(ext(spatobj), res = c(cellsize), crs = crs(spatobj))
  
  ## WARNING for number of cells; check resolution units
  
  ## nn interpolation
  nn.pred <- apply(tdat, 2, FUN = nnfit, r=r, loc=loc, coords=coords, nnmax=nnmax)
  
  xyz <- cbind(as.data.frame(suppressWarnings(crds(r))), nn.pred)
  
  if(as[1]=="SpatRast"){
    spat <- terra::rast(xyz, type="xyz", crs = crs(r))
  } 
  if(as[1]=="SpatVect"){
    spat <- terra::vect(xyz, geom = c("x", "y"), crs = crs(r))
  } 
  
  return(spat)
}

# SUBROUTINE
# nearest neighbour model for point2rast sub routine 
nnfit <- function(x, r, loc, coords, nnmax) {
  xdat <- na.omit(data.frame(xvar = as.vector(x), coords))
  
  f <- paste0("xvar", " ~ 1")
  lf <- paste0("~", paste(loc, collapse = "+"))
  
  gs <- gstat(formula = xvar~1, locations = ~x+y, data = xdat, nmax = nnmax, set=list(idp = 0))
  nn <- terra::interpolate(r, gs, debug.level=0)
  return(as.vector(nn$var1.pred))
}

#####
# ROUTINE
# Cross validation function 
# arguments: data = dataframe
#            loc = vector of coordinate names in dataframe
#            nnmax = max nearest neighbour values to interpolate from

rmse.kcross <- function(data, var, loc = c("x", "y"), nnmax = 4) {
  
  # coordinates from data
  c <- data[, loc, drop=F]
  
  # data column
  d <- data[, var, drop=F]
  
  # remove NAs - interpolation can't handle missing values
  kf_dat <- cbind(c,d) |> na.omit()
  
  # assign each value to training data, 5 validation iterations
  kf <- sample(1:5, nrow(kf_dat), replace=TRUE)
  
  # store results
  out <- rep(NA, 5)
  
  # null model variance 
  null <- RMSE(mean(kf_dat[, var]), kf_dat[, var])
  
  # formulas for gstat model
  f <- paste0(var, " ~ 1")
  lf <- paste0("~", paste(loc, collapse = "+"))
  
  for (k in 1:5) {
    test <- kf_dat[kf == k, ]
    train <- kf_dat[kf != k, ]
    gs <- gstat(formula=formula(f), locations=formula(lf), data=train, nmax=nnmax, set=list(idp=0))
    p <- predict(gs, test, debug.level=0)
    out[k] <- RMSE(test[, var], p$var1.pred)
  }
  
  # return list of results
  lout <- list(null_rmse = null, kcross_rmse = out, performance = perf(out, h0 = null))
  return(lout)
}

# SUBROUTINE - null model
# Root-mean-squared error
#  RMSE for cross validation to test intepolation sd against data sd - interpolation should be lower than data sd
#  performance = 0, interpolation has same sd as data; performance = 1, interpolation has no error
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

# SUBROUTINE - performance function
#  performance function for interpolation model fitting 
perf <- function(trmse, h0 = null) {
  round(1 - (mean(trmse) / h0), 3)
}

