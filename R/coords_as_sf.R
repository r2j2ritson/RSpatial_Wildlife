## Coerce a data frame with coordinates to an 'sf' object
coords_as_sf <- function(df,x,y,crs){
df_sf <- df %>% dplyr::mutate(xx = as.numeric(df[[x]]), yy = as.numeric(df[[y]])) %>%
dplyr::rowwise(.) %>% 
dplyr::mutate(geometry = list(sf::st_point(c(xx,yy)))) %>%
as.data.frame(.) %>% dplyr::select(-xx,-yy) %>% sf::st_as_sf(.,crs=crs)
return(df_sf)
}
#End Script#
