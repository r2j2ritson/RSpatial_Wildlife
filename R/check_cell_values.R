# Check cell values of raster (chunked, to avoid memory limits)
# Comapre cell values of two rasters using 'terra' by chunks, returns FALSE if at least one cell value changed
# Useful for cross-referencing projections or resampled rasters and quickly determing if changes occured, idicating potential mapping anomalies
check_cell_values_chunk <- function(r1,r2){
  n <- terra::nrow(r1) #number of rows in raster
  f <- get_integer_factors(n) #integer factorization of number of rows in raster
  chnk <- f[round(length(f)/2)] #determine optimal number of chunks (middle integer factor)
  start <-  seq(1,n-chnk,by=chnk) #last iteration goes to end of raster
  for(i in 1:length(start)){
    v1 <- terra::values(r1,row=start[i],nrows=chnk)
    v2 <- terra::values(r2,row=start[i],nrows=chnk)
    rc <- identical(v1,v2)
    if(isFALSE(rc)){return(rc)} #stop if a FALSE is encountered, raster values are not identical
  }
  return(rc)
}

# simple function for integer factorization
get_integer_factors <- function(n){
  out <- NULL
  for(i in 1:n){
    if((n %% i)==0){
      out <- if(is.null(out)){i}else{c(out,i)}
    }
  }
  return(out)
}
