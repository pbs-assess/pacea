#' Points to raster nearest neighbour interpolation
#'
#' @param data Vector, dataframe, or spatial (class: 'Spatial' or 'sf') input data to use for interpolation
#' @param spatobj Spatial object used to create extent for output raster
#' @param loc Vector of coordinate variable names or matrix of coordinates
#' @param cellsize Output raster cell size
#' @param nnmax Nearest neighbour maximum neighbours used for interpolation
#' @param as Output as raster 'SpatRast' or vector 'SpatVect'
#'
#' @return A terra SpatRaster or SpatVector object 
#' 
#' @author Travis Tai
#' @import sf
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(x = runif(5, 0, 10), y = runif(5, 0, 10), var = rnorm(5))
#' extent <- st_bbox(c(xmin = 0, ymin = 0, xmax= 10, ymax = 10),crs = NA)
#' output <- point2rast(dat, extent, loc = c("x","y"), cellsize = 0.5, nnmax = 2, as = "SpatRast")
#' output
#' plot(output)
#' }
point2rast <- function(data, spatobj, loc = c("x", "y"), cellsize, nnmax = 4, 
                       as = c("SpatRast","SpatVect")) {
  
  stopifnot("must provide cellsize value" = exists("cellsize"))
  stopifnot("must specify valid value for 'as'" = as %in% c("SpatRast", "SpatVect"))
  
  if(!length(dim(data))) { data <- as.matrix(data) }
  
  if(!any(sapply(c("sf", "Spatial"), function(cl) is(data, cl)))) {
    
    if(!length(dim(loc))){
      
      stopifnot("loc vector must of be of length==2 " = length(loc)==2)
      stopifnot("loc names not found in data" = loc %in% names(data))
      
      coords <- setNames(as.data.frame(data[, loc]), c("x", "y"))
      
      tdat <- as.data.frame(data[, -which(colnames(data) %in% loc), drop=F])
      
    } else { 
      
      stopifnot("loc data must be a matrix or dataframe of two columns" = 
                  length(dim(loc)) == 2 & dim(loc)[2] == 2)
      stopifnot("loc data is not of equal length to data" = nrow(data) == nrow(loc))
      
      coords <- setNames(as.data.frame(loc), c("x", "y"))
      
      tdat <- as.data.frame(data[, !colnames(data) %in% colnames(loc)])
    }
  }
  
  if(is(data, "Spatial")) {
    coords <- setNames(as.data.frame(data)[,c("coords.x1", "coords.x2")], c("x", "y"))
    tdat <- as.data.frame(data)[, names(data), drop=F]    
  }
  
  if(is(data, "sf")) {
    coords <- setNames(as.data.frame(matrix(unlist(data$geometry), ncol=2, byrow=T)), c("x", "y"))
    tdat <- as.data.frame(data)[, -which(names(data) == "geometry")]
  }
  
  tbb <- terra::ext(spatobj)
  if(!any(coords$x >= tbb$xmin & coords$x <= tbb$xmax & 
          coords$y >= tbb$ymin & coords$y <= tbb$ymax)) {
    warning("loc coordinates within spatobj extent = 0; check crs or extent of spatobj")
  }
  
  terror <- try(terra::crs(spatobj), silent=T)
  if("try-error" %in% class(terror)) {
    r <- terra::rast(terra::ext(spatobj), res = c(cellsize))
  } else {
    r <- terra::rast(terra::ext(spatobj), res = c(cellsize), crs = terra::crs(spatobj))
  }
  
  nn.pred <- apply(tdat, 2, FUN = nnfit, r=r, loc=loc, coords=coords, nnmax=nnmax)
  xyz <- cbind(as.data.frame(suppressWarnings(crds(r))), nn.pred)
  
  if(as[1]=="SpatRast"){
    spat <- terra::rast(xyz, type="xyz", crs = terra::crs(r))
  } 
  if(as[1]=="SpatVect"){
    spat <- terra::vect(xyz, geom = c("x", "y"), crs = terra::crs(r))
  } 
  
  return(spat)
}


nnfit <- function(x, r, loc, coords, nnmax) {
  xdat <- na.omit(data.frame(xvar = as.vector(x), coords))
  
  f <- paste0("xvar", " ~ 1")
  lf <- paste0("~", paste(loc, collapse = "+"))
  
  gs <- gstat::gstat(formula = xvar~1, locations = ~x+y, data = xdat, nmax = nnmax, set=list(idp = 0))
  nn <- terra::interpolate(r, gs, debug.level=0)
  return(as.vector(nn$var1.pred))
}