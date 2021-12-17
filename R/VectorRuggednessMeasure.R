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
  
  print('Setting file names...')
  wopt<-list(gdal=c("COMPRESS=NONE", "of=COG"), datatype='INT1U')
  sout <- paste0(filepath,"/sloperad.tif")
  aout <- paste0(filepath,"/aspectrad.tif")
  xyout <- paste0(filepath,"/xyrast.tif")
  zout <- paste0(filepath,"/zout.tif")
  xout <- paste0(filepath,"/xout.tif")
  yout <- paste0(filepath,"/yout.tif")
  xsumout <- paste0(filepath,"/xsumout.tif")
  ysumout <- paste0(filepath,"/ysumout.tif")
  zsumout <- paste0(filepath,"/zsumout.tif")
  vrmout <- paste0(filepath,"/vrmout.tif")
  
  print('Calculating slope (radians)')
  slp <- terra::terrain(x, v="slope", unit="radians", neighbors=8, filename = sout) 
  
  print('Calculating aspect (radians)')
  asp <- terra::terrain(x, v="aspect", unit="radians", neighbors=8, filename = aout) 	
  
  print('Calculating xy raster...')
  sin.slp <- terra::app(slp, fun = sin, cores = n, filename = xyout, wopt = wopt)
  
  print('Calculating z raster...')
  cos.slp <- terra::app(slp, fun = cos, cores = n, filename = zout, wopt = wopt)
  rm(slp)
  file.remove(sout)
  
  print('Calculating x raster...')
  sin.asp <- terra::app(asp, fun = sin, cores = n, filename = xout, wopt = wopt) * sin.slp
  
  print('Calculating y raster...')
  cos.asp <- terra::app(asp, fun = cos, cores = n, filename = yout, wopt = wopt) * sin.slp
  rm(asp,sin.slp)
  do.call(file.remove,list(aout,xyout))
  
  print('Calculating x Sum...')
  x.sum <- terra::focal(sin.asp, w = s, fun="sum", filename = xsumout, wopt = wopt) 
  rm(sin.asp)
  file.remove(yout)
  
  print('Calculating y Sum...')
  y.sum <- terra::focal(cos.asp, w = s, fun="sum", filename = ysumout, wopt = wopt)
  rm(cos.asp)
  file.remove(xout)
  
  print('Calculating z Sum...')
  z.sum <- terra::focal(cos.slp, w = s, fun="sum", filename = zsumout, wopt = wopt)
  rm(cos.slp)
  file.remove(zout)
  
  print('Calculating vrm...')
  vrm.fun <- function(x, y, z) {sqrt((x^2) + (y^2) + (z^2))}
  sum_sds <- terra::sds(x.sum, y.sum, z.sum)
  r <- terra::lapp(sum_sds, fun = vrm.fun, cores = n, filename = vrmout, wopt = wopt)
  rm(sum_sds,x.sum,y.sum,z.sum)
  do.call(file.remove,list(xsumout,ysumout,zsumout))
  
  print('Calculating final vrm...')
  vrm.fun.final <- function(r) 1 - (r / scale.factor) 
  vrm.final <- terra::app(r, fun = vrm.fun.final, scale.factor, cores = n, filename = paste0(filepath,"/",outname), wopt = wopt)
  
  rm(r)
  file.remove(vrmout)
  terra::tmpFiles(remove = T)
  return(vrm.final)
}
