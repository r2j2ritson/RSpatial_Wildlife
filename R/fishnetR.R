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

fishnetR <- function(shp,cell_size,n,stratify=TRUE,seed=1){
  #if(!(class(shp) %in% c('sf','sfc','sfc_POLYGON'))){stop("Shape must be of class of `sf`")}
  
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
  
  # Create grid (fishnet)
  grid <- sf::st_make_grid(shp, cellsize = cell_size, what = "polygons", square = T) %>% 
    terra::vect(.) %>%
    terra::crop(.,terra::vect(shp)) %>% 
    sf::st_as_sf(.) %>%
    sf::st_cast(.,"POLYGON") %>%
    dplyr::mutate(id = seq(1,nrow(.), by=1))
  
  #Calculate Stratas
  stratas <- rep(c(1:n),each=floor(nrow(grid)/n))
  
  # randomly assign remainder
  extra <- sample.int(n,nrow(grid)%%n,F)
  stratas <- sort(c(stratas,extra))
  
  # Set stratas
  grid <- dplyr::mutate(grid,strata = as.factor(stratas))
    
  #Stratify Random Sample
  set.seed(seed)
  if(stratify){
    i=sample(1:1000,1)
    repeat{
      i= i+1
      set.seed(i)
      #Begin essential code from spatialEco::stratified_random()
      spx <- sf::st_drop_geometry(grid)
      if(!inherits(spx[,"strata"], "factor")) 
        spx[,"strata"] <- factor(spx[,"strata"]) 
      spx$REP <- NA
      reps=1
      nn=1
      results <- list()
      for(j in levels(spx[, "strata"])) {
        d <- spx[spx[,"strata"] == j,]
        d$rowname <- rownames(d)
        if(nrow(d) > n) {  	
          for (i in 1:reps) {	
            s <- lapply(1, function(ij) {
              d[sample(1:nrow(d), nn),]})
            s[[1]]$REP <- i
            results[[paste(j,i,sep="_")]] <-s[[1]] 
          }
        } else {
          d$REP <- 1
          results[[paste(j,i,sep="_")]] <- d
        }
      }
      results
      results <- do.call(rbind, results)
      replace=F
      if(!replace){
        if(any(duplicated(results$rowname))){ #previous spatialEco code wiped out all results when sampling without replacement
          results <- results[-which(duplicated(results$rowname)),]
        }
      }
      results <- stats::na.omit(results[,c("rowname","REP")])
      results <- merge(grid, results, by.y="rowname", by.x = 'row.names', 
                       all.x = FALSE, all.y = TRUE)
      strat_rand_samp <- results %>% 
        sf::st_as_sf(.)
      if(any(sf::st_relate(strat_rand_samp, pattern = "F***1****",sparse=F)==T)==F && nrow(strat_rand_samp)==21){ #make sure random selections from each strata do not touch
        break
      }
    }
    return(list(strat_rand_samp = strat_rand_samp, grid = grid)) #returns list containing the random sample shapes as well as the base 'fishnet' grid (croped to shape)
    
    #Simple Random Sample
  }else{
    randsamp <- sample.int(nrow(grid), size = n, replace=F) 
    locs <- grid[randsamp,]
    #sf::st_write(locs,filepath) #write grids to filepath (forthcoming)
    return(list(locs = locs, grid = grid) #returns list containing the random sample shapes as well as the base 'fishnet' grid (croped to shape)
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
