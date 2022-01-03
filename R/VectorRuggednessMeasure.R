#' @title Vector Ruggedness Measure using 'terra' 
#' @description Implementation of the vector ruggedness measure (Sappington et al, 2007) using R package 'terra' (Hijmans et al, 2021)
#' 
#' @param x Elevation raster of class 'SpatRaster' 
#' @param n A positive integer, if cores > 1, a cluster is created and used for calculations where appropriate (from 'parallel')
#' @param s Scale of window, must be an odd number (typically 3 or 5)
#' @param filepath A character string of the file path to store intermediate rasters and save the output raster
#' @param outname Name of the output raster (for best results, should include *.tif)
#'
#' @note 
#' Key improvements over 'spatialEco::vrm()' include the implementation of 'terra' over 'raster' to leverage multiple threads,
#' creation of intermediate raster files in the specified filepath which are deleted upon completion (prevents memory issues arising from 'Temp' files),
#' and progress bars from 'terra'. This R implementation more closely mirrors Sappington's ArcGIS/Python script and is particularly useful for larger rasters.
#'
#' @author Robert Ritson <robert.ritson@idfg.idaho.gov>
#'
#' @references
#' Sappington, J.M., K.M. Longshore, D.B. Thomson (2007). Quantifying Landscape Ruggedness for Animal Habitat Analysis: 
#'   A case Study Using Bighorn Sheep in the Mojave Desert. Journal of Wildlife Management. 71(5):1419-1426
#' Robert J. Hijmans (2021). terra: Spatial Data Analysis. R package version 1.4-11. https://CRAN.R-project.org/package=terra
#' Evans JS (2021). _spatialEco_. R package version 1.3-6, <URL: https://github.com/jeffreyevans/spatialEco>.
#'
#' @examples
#'  require('terra')
#'  elev <- terra::rast('C:/Path/to/DEM/raster/file')
#'  vrmTerra3 <- terra_vrm(elev, n=1, s=3, filepath='C:/Path/to/store/result', outname='vrmTerra3.tif')
#'
#' @export terra_vrm
terra_vrm <- function(x, n = 1, s = 3, filepath = "",outname = "vrm.tif") {
  if(length(s) > 2) stop( "Specified window exceeds 2 dimensions")   
  scale.factor <- round(s^2, 0)
  f = matrix(1,s,s) #focal window
  
  print('Setting file names...')
  sout <- paste0(filepath,"/sloperad.tif")
  aout <- paste0(filepath,"/aspectrad.tif")
  xyout <- paste0(filepath,"/xyrast.tif")
  zout <- paste0(filepath,"/zout.tif")
  xout <- paste0(filepath,"/xout.tif")
  xfinal <- paste0(filepath,"/xoutfinal.tif")
  yout <- paste0(filepath,"/yout.tif")
  yfinal <- paste0(filepath,"/youtfinal.tif")
  xsumout <- paste0(filepath,"/xsumout.tif")
  ysumout <- paste0(filepath,"/ysumout.tif")
  zsumout <- paste0(filepath,"/zsumout.tif")
  resout <- paste0(filepath,"/resout.tif")
  
  print('Calculating slope (radians)')
  slp <- terra::terrain(x, v="slope", unit="radians", neighbors=8, filename = sout,overwrite=T) 
  
  print('Calculating aspect (radians)')
  asp <- terra::terrain(x, v="aspect", unit="radians", neighbors=8, filename = aout,overwrite=T) 	
  
  print('Calculating xy raster...')
  xyraster <- terra::app(slp, fun = function(i){sin(i)}, cores = n, filename = xyout,overwrite=T)

  print('Calculating z raster...')
  zraster <- terra::app(slp, fun = function(i){cos(i)}, cores = n, filename = zout,overwrite=T)
  rm(slp)

  print('Calculating x raster...')
  xraster1 <- terra::app(asp, fun = function(i){sin(i)}, cores = n, filename = xout, overwrite=T) 
  xraster2 <- xraster1 * xyraster
  terra::writeRaster(xraster2,xfinal)
  rm(xraster1)
  
  print('Calculating y raster...')
  yraster1 <- terra::app(asp, fun = function(i){cos(i)}, cores = n, filename = yout, overwrite=T) 
  yraster2 <- yraster1 * xyraster
  terra::writeRaster(yraster2,yfinal)
  rm(yraster1,xyraster,asp)
  do.call(file.remove,list(aout,xyout))
  
  print('Calculating x Sum...')
  x.sum <- terra::focal(xraster2, w = f, fun=sum, na.rm = T, filename = xsumout, overwrite=T) 
  rm(xraster2)
  do.call(file.remove,list(xout,xfinal))
  
  print('Calculating y Sum...')
  y.sum <- terra::focal(yraster2, w = f, fun=sum, na.rm = T, filename = ysumout, overwrite=T)
  rm(yraster2)
  do.call(file.remove,list(yout,yfinal))
  
  print('Calculating z Sum...')
  z.sum <- terra::focal(zraster, w = f, fun=sum, na.rm = T, filename = zsumout, overwrite=T)
  rm(zraster)
  file.remove(zout)
  
  print('Calculate resultant raster...')
  r.fun <- function(x, y, z) {sqrt((x^2) + (y^2) + (z^2))}
  sum_sds <- terra::sds(x.sum, y.sum, z.sum)
  r <- terra::lapp(sum_sds, fun = r.fun, filename = resout,overwrite=T)
  rm(sum_sds,x.sum,y.sum,z.sum)
  do.call(file.remove,list(xsumout,ysumout,zsumout))
  
  print('Calculate Ruggedness raster...')
  vrm.fun <- function(r){1 - (r/scale.factor)}
  vrm <- terra::app(r, fun = vrm.fun, cores = n, filename = paste0(filepath,"/",outname), overwrite=T)
  rm(r)
  file.remove(resout)
  terra::tmpFiles(remove = T)
  return(vrm)
}
