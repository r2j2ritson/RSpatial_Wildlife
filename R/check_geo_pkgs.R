# Check geographic R packages 
## 'rgeos', 'rgdal', 'maptools', 'sp', and 'raster' are being phased out
## check any current packages in your R library with these dependencies
check_geo_pkgs <- function(){
  suppressMessages({
    devtools::source_url("https://github.com/r2j2ritson/RSpatial_Wildlife/blob/main/R/instaload.R?raw=TRUE")
    instaload(c('packagefinder','dplyr'))
    pkg_list <- c('rgdal', 'rgeos', 'maptools','sp','raster')
    field <- c("Imports","Depends")
    out <- NULL
    for(p in pkg_list) {
      for(f in field){
        tmp <- packagefinder::exploreFields(p, f)
        class(tmp) <- NULL
        tmp <- tmp %>%
          as.data.frame(.) %>%
          dplyr::mutate(Field = f,
                        Pkg = p) %>%
          dplyr::select(NAME,Field,Pkg) 
        out <- rbind(out,tmp)
      }
    }
  })
  suppressWarnings({
    out <- out %>%
      tidyr::pivot_wider(.,names_from = Field,values_from = Pkg,values_fill = list(NA)) %>%
      dplyr::rowwise(.) %>%
      dplyr::mutate(any_rgdal = ifelse(any(stringr::str_detect(c(Imports,Depends),"rgdal")),1,0),
                    any_rgeos = ifelse(any(stringr::str_detect(c(Imports,Depends),"rgeos")),1,0),
                    any_maptools = ifelse(any(stringr::str_detect(c(Imports,Depends),"maptools")),1,0),
                    any_sp = ifelse(any(stringr::str_detect(c(Imports,Depends),"sp")),1,0),
                    any_raster = ifelse(any(stringr::str_detect(c(Imports,Depends),"raster")),1,0)) %>%
      dplyr::mutate(score_1 = sum(any_rgdal,any_rgeos,any_maptools,na.rm = T),
                    score_2 = sum(any_sp,any_raster,na.rm = T),
                    score = as.numeric(paste0(score_1,".",score_2))) %>%
      dplyr::select(NAME,Imports,Depends,score) %>%
      dplyr::arrange(desc(score)) %>%
      dplyr::select(-score) %>%
      as.data.frame(.) %>%
      dplyr::filter(NAME %in% installed.packages()[,"Package"])
  })
  return(out)
}
#pkgs <- check_geo_pkgs()
