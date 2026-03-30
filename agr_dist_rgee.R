# Distance agriculture pixels with GEE
# source: Mapbiomas, collection 10
# tutorial about GEE functions --> https://developers.google.com/earth-engine/apidocs/ee-image-reproject?hl=pt-br

# Starting GEE
library(reticulate)
Sys.setenv(
  RETICULATE_PYTHON = "C:/miniconda3/python.exe",
  EARTHENGINE_CONFIG_DIR = "C:/miniconda3/ee_config")
library(rgee)
ee_Initialize(user = "arthur.barros.ar@gmail.com", 
              drive = TRUE)
ee_check()

# Starting task

lu_mapbiomas<-ee$Image("projects/mapbiomas-public/assets/brazil/lulc/collection10_1/mapbiomas_brazil_collection10_1_coverage_v1")
#test1<-lu_mapbiomas$bandNames()
#test1$getInfo()
bd_yr_name<-c(
  "classification_1990",
  "classification_1995",
  "classification_2000",
  "classification_2005",
  "classification_2010",
  "classification_2015")

lu_filtered<-lu_mapbiomas$select(bd_yr_name)
bands_agric_px<-lu_filtered$bandNames()
#bands_agric_px$getInfo()

agric_px <- ee$ImageCollection(
  bands_agric_px$map(
    ee_utils_pyfunc(function(w) {
      w<-ee$String(w)
      w_bands<-lu_filtered$select(w)
  
      w_class<-w_bands$
        remap(
          from = c(14, 15, 18, 19, 39, 20, 40,
                   62, 41, 36, 46, 47, 35, 48, 
                   9, 21),
          to = rep(1, 16), defaultValue = 0)$
        rename(w)
      return(w_class)
      })))$toBands()

agric_px$bandNames()$getInfo()
new_bands_agric_px<-agric_px$bandNames()

dist_agr_px <- ee$ImageCollection(
  new_bands_agric_px$map(
    ee_utils_pyfunc(function(x) {
      x <- ee$String(x)
      x_bands <- agric_px$select(x)
      
      x_dist <- x_bands$
        fastDistanceTransform(neighborhood = 1000)$
        sqrt()$
        multiply(30)$
        rename(x)
      return(x_dist)
    })))$toBands()
dist_agr_px$bandNames()$getInfo()


library(tidyverse)
library(sf)
sf_use_s2(FALSE)

centroids_ee <- read_sf("Outputs/PA_br.gpkg") %>% #crs:4674 (mapbiomas standard)
  st_transform("EPSG:5880") %>% 
  st_centroid() %>% 
  dplyr::select(new_code, first_year_t) %>% 
  mutate(year_match = case_when(
    first_year_t %in% c(1991, 0) ~ "0_0_classification_1990",
    first_year_t == 1996 ~ "1_1_classification_1995",
    first_year_t == 2001 ~ "2_2_classification_2000",
    first_year_t == 2006 ~ "3_3_classification_2005",
    first_year_t == 2011 ~ "4_4_classification_2010",
    first_year_t == 2016 ~ "5_5_classification_2015"
  )) %>% 
  sf_as_ee()


centroids_agr_dist<-centroids_ee$map(
  ee_utils_pyfunc(function(i){
    band_name<-ee$String(i$get("year_match"))
    
    value<-dist_agr_px$
      select(band_name)$
      reduceRegion(
        reducer = ee$Reducer$first(),
        geometry = i$geometry(),
        scale = 30)
    dist_value <- value$get(band_name)
    i$set("dist", dist_value)
  }))

task <- ee_table_to_drive(
  collection = centroids_agr_dist,
  description = "dist_agric_centroids",
  fileFormat = "CSV"
)

#ee_user_info()
task$start()

#after it already, i will manually download it. 
dist_to_join<-read_csv("Outputs/dist_agric_centroids.csv") %>% glimpse

PA_br_agr<-read_sf("Outputs/PA_br.gpkg") %>% 
  left_join(dist_to_join %>% select(new_code, dist),
            by="new_code") %>% 
  rename(dist_agr=dist) %>% 
  glimpse

write_sf(PA_br_agr, "Outputs/PA_br_agr.gpkg")