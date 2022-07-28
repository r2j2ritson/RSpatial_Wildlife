# Check alignment of cells between two rasters
# grabs coordinates of an equivalent cell in each raster and measures distance between the points in meters (with 'terra' and 'sf')
# Useful for cross-checking projections and resmapling of rasters
check_alignment_cell <- function(r1,r2,cell = 1){
  x <- terra::xyFromCell(r1,cell = cell)
  crds_1 <- as.numeric(sprintf("%.10f",x)) #to the nano-meter
  c1 <- sf::st_point(crds_1)
  
  y <- terra::xyFromCell(r2,cell = cell)
  crds_2 <- as.numeric(sprintf("%.10f",y)) #to the nano-meter
  c2 <- sf::st_point(crds_2)
  
  diff <- sf::st_distance(c1,c2) #reported in meters
  return(diff)
}
