#Covariates:  Distancies urban and Agriculture pixels (Mapbiomas LU)
# source: https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_10/lulc/coverage/brazil_coverage_2024.tif

library(tidyverse)
library(terra)
library(raster)
library(sf)

########## preciso ajeitar centroid, a função. 
#o worflow que pensei é stackar e extrair no mach do
# nome com uma nova coluna que vou criar usando paste(lu+first_y_t)

########## finishe
sf_use_s2(FALSE)
terraOptions(tempdir = here::here("D:/temp"))
#terraOptions()$tempdir

PA<-read_sf("Outputs/PA_br.gpkg")

centroid_PA5880<-PA %>% 
  st_transform(., "EPSG:5880") %>% 
  st_centroid() %>% 
  dplyr::select(new_code)

LU<-list.files("DATA/Mapbiomas/LU", 
               full.names = TRUE)
#rast(LU[1]) %>% crs() #4326

n_LU<-c("LU_1990", "LU_1995", "LU_2000", 
        "LU_2005", "LU_2010", "LU_2015")

agg_dist_extract<-function(x){
  x_agg <- aggregate(
    x,
    fact = 2,   #~60 m pixels
    fun = max,
    na.rm = TRUE)
  
  dist_x<-distance(x_agg)
  
  extracting_x <- terra::extract(
    dist_x,
    vect(centroid_PA5880),
    bind = TRUE)
  return(extracting_x)
  beepr::beep()
}


# Urban ----

urban_px<-list()
for (i in seq_along(LU)) {
  r_i<-rast(LU[[i]])
  r_urb <- ifel(r_i==24, 1, NA)
  r_urb_proj <- project(r_urb, "EPSG:5880") 
  urban_px[[n_LU]] <- r_urb_proj
}
beepr::beep()

# Agriculture ----

agr_class<-c(14, 15, 18, 19, 39, 20, 40, 62, 41, 
             36, 46, 47, 35, 48, 9, 21)
agr_px<-list()
for (i in seq_along(LU)) {
  r_i<-rast(LU[[i]])
  r_agr <- ifel(r_i %in% agr_class, 1, NA)
  r_agr_proj <- project(r_agr, "EPSG:5880") 
  agr_px[[n_LU]] <- r_agr_proj
}
beepr::beep()
