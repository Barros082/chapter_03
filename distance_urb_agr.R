#Covariates:  Distancies urban and Agriculture pixels (Mapbiomas LU)
# source: https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_10/lulc/coverage/brazil_coverage_2024.tif

library(tidyverse)
library(terra)
library(raster)
library(sf)

sf_use_s2(FALSE)
#terraOptions(tempdir = here::here("D:/temp"))
#terraOptions()$tempdir

PA<-read_sf("Outputs/PA_br.gpkg")

LU<-list.files("DATA/Mapbiomas/LU", 
               full.names = TRUE)
#rast(LU[1]) %>% crs() #4326

n_LU<-c("LU_1990", "LU_1995", "LU_2000", 
        "LU_2005", "LU_2010", "LU_2015")

centroids <- PA %>% 
  st_transform("EPSG:5880") %>% 
  st_centroid() %>% 
  dplyr::select(new_code, first_year_t) %>% 
  mutate(year_match = case_when(
    first_year_t %in% c(1991, 0) ~ "LU_1990",
    first_year_t == 1996 ~ "LU_1995",
    first_year_t == 2001 ~ "LU_2000",
    first_year_t == 2006 ~ "LU_2005",
    first_year_t == 2011 ~ "LU_2010",
    first_year_t == 2016 ~ "LU_2015"
  ))

urban_px<-list()
for (i in seq_along(LU)) {
  r_i<-rast(LU[[i]])
  nm_lu<-n_LU[i]
  r_urb <- ifel(r_i==24, 1, NA)
  r_urb_proj <- project(
    r_urb, "EPSG:5880",
    filename = paste0("Outputs/temporarios/temp_proj_", 
                      nm_lu, ".tif"),
    overwrite = TRUE,
    wopt = list(gdal = c("COMPRESS=LZW"))) 
  
  urban_px[[nm_lu]] <- r_urb_proj
}

#urban_px[[n_LU[1]]]<-rast("Outputs/temporarios/temp_proj_LU_1990.tif")
#urban_px[[n_LU[2]]]<-rast("Outputs/temporarios/temp_proj_LU_1995.tif")
#urban_px[[n_LU[3]]]<-rast("Outputs/temporarios/temp_proj_LU_2000.tif")
#urban_px[[n_LU[4]]]<-rast("Outputs/temporarios/temp_proj_LU_2005.tif")
#urban_px[[n_LU[5]]]<-rast("Outputs/temporarios/temp_proj_LU_2010.tif")
#urban_px[[n_LU[6]]]<-rast("Outputs/temporarios/temp_proj_LU_2015.tif")
#for (i in seq_along(urban_px)) {
#  names(urban_px[[i]]) <- names(urban_px)[i]
#}

dist_rasters <-list()
for (i in seq_along(urban_px)) {
  r_i <- urban_px[[i]]
  nm_r_i <- names(urban_px)[i]
  r_i_agg <- aggregate(r_i, fact = 2, 
                     fun = max, na.rm = TRUE)
  #dist_rasters[[nm_r_i]]<-
  distance(r_i_agg, 
           filename = paste0("Outputs/temporarios/temp_dist_", 
                             nm_r_i, ".tif"),
           overwrite = TRUE)
  message(nm_r_i, " - Finished")
  rm(r_i, r_i_agg)
  gc()
  }

dist_list <- list()
for (yr in unique(centroids$year_match)) {
  pts <- centroids %>% 
    dplyr::filter(year_match == yr)
  
  dist_list[[yr]] <- terra::extract(
    dist_rasters[[yr]],
    vect(pts),
    bind = TRUE
    )}
dist_urb_px <- do.call(rbind, dist_list)



#garbage ----

agg_dist_extract<-function(x, centr_i){
  x_agg <- aggregate(
    x,
    fact = 2,   #~60 m pixels
    fun = max,
    na.rm = TRUE)
  
  dist_x<-distance(x_agg)
  
  extracting_x <- terra::extract(
    dist_x,
    vect(centr_i),
    bind = TRUE)
  beepr::beep()
  return(extracting_x)
}


# Urban ---

urban_px<-list()
for (i in seq_along(LU)) {
  r_i<-rast(LU[[i]])
  nm_lu<-n_LU[i]
  r_urb <- ifel(r_i==24, 1, NA)
  r_urb_proj <- project(
    r_urb, "EPSG:5880",
    filename = paste0("Outputs/temporarios/temp_proj_", 
                      nm_lu, ".tif"),
    overwrite = TRUE,
    wopt = list(gdal = c("COMPRESS=LZW"))) 
  
  urban_px[[nm_lu]] <- r_urb_proj
}
beepr::beep()

dist_urb<-list()
for (i in seq_along(centroid_PA5880)) {
  centr_i<-centroid_PA5880[[i]]
  
  if(centr_i$first_year_t%in%c(1991, 0)){
    K=urban_px[["LU_1990"]]}
  if(centr_i$first_year_t==1996){
    K=urban_px[["LU_1995"]]}
  if(centr_i$first_year_t==2001){
    K=urban_px[["LU_2000"]]}
  if(centr_i$first_year_t==2006){
    K=urban_px[["LU_2005"]]}
  if(centr_i$first_year_t==2011){
    K=urban_px[["LU_2010"]]}
  if(centr_i$first_year_t==2016){
    K=urban_px[["LU_2015"]]}
  
  dist_urb[[i]]<-agg_dist_extract(K, centr_i)
}
dist_urb_px<-do.call(rbind, dist_urb)




# Agriculture ---

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
