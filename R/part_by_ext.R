# Parition list of rasters by shared extent
part_by_ext <- function(rlist) {
  out <- NULL
  for(i in 1:length(rlist)){
    r <- terra::rast(rlist[i]) %>% terra::ext(.) %>% as.list(.) %>% as.data.frame(.) %>%
      dplyr::mutate(lyr = basename(rlist[i]),
                    ext = paste(round(xmin),round(xmax),round(ymin),round(ymax),collapse = ",")) %>%
      dplyr::select(lyr,ext)
    out <- rbind(out,r)
  }
  sel <- out %>% dplyr::group_by(ext) %>% dplyr::summarise(n=n(),lyr_list = list(lyr)) %>% dplyr::ungroup(.) %>% as.data.frame(.)
  return(sel)
}
