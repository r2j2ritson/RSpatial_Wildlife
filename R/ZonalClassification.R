#' @title Zonal Classification of Rasters by Polygons
#' @description A proxy for ESRI's ArcGIS 'Zonal Histogram' tool using R package 'exactextractr'. 
#' Appends coverage areas of raster classification categories to polygon 'sf' data frame.
#' 
#' @param poly an 'sf' object containing a 'MULTIPOLYGON' or 'POLYGON'
#' @param rast a 'RasterLayer' object containing the classified raster of interest
#' @param lut an object containing the raster attribute table (typically read from a *.dbf) with the columns "VALUE" (raster value) and "ReClass" (classification)
#' @param rm_ls Optional. A vector of column names in 'poly' to overwrite.
#'
#' @note
#' Particularly useful for sf 'MULTIPOLYGONS' since this function implements 'purrr::imap_dfr()' to summarise and rename the output of 'exactextractr::exact_extract()'.
#' The result is similar to what would be expected of the output from 'Zonal Histogram' in ESRI ArcGIS, but without the limitation on the number of records.
#' 
#' @author Robert Ritson <robert.ritson@idfg.idaho.gov>
#'
#' @references
#' Lionel Henry and Hadley Wickham (2020). purrr: Functional Programming Tools. R package version 0.3.4.
#'     https://CRAN.R-project.org/package=purrr.
#' Daniel Baston (2021). exactextractr: Fast Extraction from Raster Datasets using Polygons. R package version 0.7.1.
#'     https://CRAN.R-project.org/package=exactextractr
#' Pebesma, E., 2018. Simple Features for R: Standardized Support for Spatial Vector Data. The R Journal 10 (1), 439-446,
#'     https://doi.org/10.32614/RJ-2018-009
#' Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2021). dplyr: A Grammar of Data Manipulation. R package version
#'     1.0.7. https://CRAN.R-project.org/package=dplyr
#' Matt Dowle and Arun Srinivasan (2021). data.table: Extension of `data.frame`. R package version 1.14.2.
#'     https://CRAN.R-project.org/package=data.table
#' ESRI 2011. ArcGIS Desktop: Release 10. Redlands, CA: Environmental Systems Research Institute.
#'
#' @examples
#'  ###NOT RUN:
#'  ## Load requried packages and objects
#'  instaload(c('dplyr','purrr','data.table','foreign','raster','sf','exactextractr'),update=F)
#'  targetPoly <- sf::st_read('C:/File path to polygons')
#'  classRast <- raster::raster('C:/File path to classified raster')
#'  classes <- foreign::read_dbf('C:/File path to classified raster *.dbf')[,c('VALUE','ReClass')] #Rename columns if neccessary
#'  
#'  ## Execute Zonal Classification
#'  targetPoly_class <- zonal_class(poly=targetPoly, rast=classRast, lut = classes)
#'  ###END NOT RUN
#'
#' @export zonal_class
zonal_class <- function(poly,rast,lut,rm_ls = NULL){
  poly <- sf::st_as_sf(poly,sf_column_name = "Shape")
  poly <- poly[,!(colnames(poly) %in% rm_ls)]
  temp <- exactextractr::exact_extract(rast,poly,coverage_area = T)
  out <- do.call(cbind,purrr::imap_dfr(temp, 
  function(temp){
    out <- temp %>%
      dplyr::left_join(lut,by=c("value"='VALUE')) %>% 
      dplyr::select('ReClass','coverage_area') %>% 
      dplyr::group_by(ReClass) %>%
      dplyr::summarise(Total_m2 = round(sum(coverage_area),3)) %>%
      data.table::transpose() %>%
      `colnames<-`(paste0(.[1,])) %>%
      dplyr::slice(-1) 
    out[,subset(as.character(levels(lut)), !(as.character(levels(lut)) %in% colnames(out)))] <- 0
    out[,as.character(levels(lut))] <- as.numeric(out[,as.character(levels(lut))])
    return(out)
  }))
  final <- cbind(as.data.frame(poly),out)
  return(final)
}
