##------- IDFG-Fish R --------##
# R translation of ArcMap 'fishnet' tool, 
# customized for random sampling of reservoirs for gillnets
# By: Robert Ritson, 3/11/2022
##---------------------------###
## fishnetR function ###
# Description - randomly sample reservoirs for gillnet sampling
# shp - 'sf' object of reservoir shape
# cell_size - numeric, how large grid cells should be in meters
# n - numeric, how many grid cells to randomly sample

# Returns shape file ('sf' object) of the randomly selected grid cells
# Forthcoming: Cell size and sample size rules based on reservoir area, writing shapefile to a filepath

fishnetR <- function(shp,cell_size,n,stratify=TRUE){
  if(!(class(shp) %in% c('sf','sfc','sfc_POLYGON'))){stop("Shape must be of class of `sf`")}
  
  ## Number of samples and cell size rules (forthcoming)
  #if(is.null(n)){
  #  n <- ifelse(sf::st_area(shp) < xx, n1,
  #              ifelse(sf::st_area(shp) >= xx & sf::st_area(shp) <= xy, n2,
  #                     ifelse(sf::st_area(shp) >= xz, n3)))
  #}
  #if(is.null(cell_size)){
  #  cell_size <- ifelse(n == n1, yy,
  #                      ifelse(n == n2,yx,
  #                             ifelse(n == n3,yz)))
  #}
  require(dplyr)
  
  grid <- sf::st_make_grid(shp, cellsize = cell_size, what = "polygons", square = T) %>% 
  terra::vect(.) %>%
  terra::crop(.,terra::vect(shp)) %>% 
  sf::st_as_sf(.) %>%
  dplyr::mutate(strata = rep(c(1:n),each=round(nrow(grid)/n))[1:nrow(grid)],
                id = seq(1,nrow(grid), by=1)) %>%
  sf::as_Spatial(.)
  #Stratify Random Sample
  if(stratify){
    i=1
    repeat{
    i= i+1
    set.seed(i)
    strat_rand_samp <- spatialEco::stratified.random(grid,strata = 'strata', n=1, reps = 1, replace = F) %>% sf::st_as_sf(.)
    if(any(sf::st_relate(strat_rand_samp, pattern = "F***1****",sparse=F)==T)==F){
      break
      }
    }
    return(strat_rand_samp)
  
  #Simple Random Sample
  }else{
    randsamp <- sample.int(length(grid_clip), size = n, replace=F)
    locs <- grid[randsamp,]
    sf::st_write(locs,filepath) #write grids to filepath (forthcoming)
    return(locs)
    }
}

### Example
##NOT RUN
#require(dplyr)
#sim_resv <- sf::st_point(x = c(-71.06, -71.06), dim = "XYZ") %>%
#  sf::st_sfc(crs = 32619) %>%
#  sf::st_buffer(units::set_units(8.04672, km)) #create simulated reservoir (a circle)

#plot(sim_resv)

#set.seed(1)

#devtools::source_url("https://github.com/r2j2ritson/RSpatial_Wildlife/blob/main/R/fishnetR.R?raw=TRUE")

#fishnet_sample <- fishnetR(shp = sim_resv, cell_size = 1000, n = 5)

#plot(fishnet_sample,add = T)
##END NOT RUN

## Call from GitHub
#install.packages('devtools')
#devtools::source_url("https://github.com/r2j2ritson/RSpatial_Wildlife/blob/main/R/fishnetR.R?raw=TRUE")
