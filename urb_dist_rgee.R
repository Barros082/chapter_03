# Distance urban pixels with GEE
# source: Mapbiomas, collection 10
# tutorial about GEE functions --> https://developers.google.com/earth-engine/apidocs/ee-image-reproject?hl=pt-br

# Starting GEE
library(reticulate)
#install_miniconda("C:/miniconda3")
#conda_create("rgee_env", packages = "python=3.10")
use_python("C:/miniconda3/envs/rgee_env/python.exe",
           required = TRUE)
#py_install(c("earthengine-api==1.6.11", "numpy",
#             "pandas"), pip = TRUE)

library(rgee)
ee_Initialize(user = "arthur.barros.ar@gmail.com", 
              drive = TRUE)
ee_check()

# Starting task

lu_mapbiomas<-ee$Image("projects/mapbiomas-public/assets/brazil/lulc/collection10_1/mapbiomas_brazil_collection10_1_coverage_v1")
#clname<-lu_mapbiomas$bandNames()
#clname$getInfo()
bd_yr_name<-c(
  "classification_1990",
  "classification_1995",
  "classification_2000",
  "classification_2005",
  "classification_2010",
  "classification_2015", 
  "classification_2020", 
  "classification_2021",
  "classification_2022",
  "classification_2023", 
  "classification_2024")

lu_filtered<-lu_mapbiomas$select(bd_yr_name)
#test1<-lu_filtered$bandNames()
#test1$getInfo()

urban_px<-lu_filtered$eq(24)$selfMask()
#test3<-urban_px$bandNames()
#test3$getInfo()

bands_urban_px<-urban_px$bandNames()
#bands_urban_px$getInfo()

dist_urb_px <- ee$ImageCollection(
  bands_urban_px$map(
    ee_utils_pyfunc(function(x) {
      x <- ee$String(x)
      x_bands <- urban_px$select(x)
      
      x_dist <- x_bands$
        fastDistanceTransform(neighborhood = 1000)$
        sqrt()$
        multiply(30)$
        rename(x)
      # notes¹: ngb=1000 is the same of looking for a urban pixel in a radios of 30x1000=30000 m or 30 km 
      # notes²: we need to sqrt because "a métrica de distância padrão retorna a distância ao quadrado."
      # notes³: 1 pixel = 30 meters ~ we need multiply the pixel distant after sqrt to 30 to get the distance values in meters
      return(x_dist)
    })))$toBands()
#test5<-dist_urb_px$bandNames()
#test5$getInfo()


library(tidyverse)
library(sf)
sf_use_s2(FALSE)

centroids_ee <- read_sf("Outputs/PA_br.gpkg") %>% #crs:4674 (mapbiomas standard)
  #fixing first_year_treat column
  mutate(first_year_t=if_else(first_year_t==0, 
                              cria_ano, first_year_t)) %>%
  st_transform("EPSG:5880") %>% 
  st_centroid() %>% 
  dplyr::select(new_code, first_year_t) %>% 
  mutate(year_match = case_when(
    first_year_t == 1991 ~ "0_classification_1990",
    first_year_t == 1996 ~ "1_classification_1995",
    first_year_t == 2001 ~ "2_classification_2000",
    first_year_t == 2006 ~ "3_classification_2005",
    first_year_t == 2011 ~ "4_classification_2010",
    first_year_t == 2016 ~ "5_classification_2015", 
    first_year_t == 2021 ~ "6_classification_2020",
    first_year_t == 2022 ~ "7_classification_2021",
    first_year_t == 2023 ~ "8_classification_2022",
    first_year_t == 2024 ~ "9_classification_2023",
    first_year_t == 2025 ~ "10_classification_2024"
  )) %>% 
  sf_as_ee()


centroids_urb_dist<-centroids_ee$map(
  ee_utils_pyfunc(function(i){
    band_name<-ee$String(i$get("year_match"))
  
    value<-dist_urb_px$
      select(band_name)$
      reduceRegion(
        reducer = ee$Reducer$first(),
        geometry = i$geometry(),
        scale = 30)
    dist_value <- value$get(band_name)
    i$set("dist", dist_value)
}))

task <- ee_table_to_drive(
  collection = centroids_urb_dist,
  description = "dist_urban_centroids",
  fileFormat = "CSV"
)

#ee_user_info()
task$start()

#after it already, i will manually download it. 
dist_to_join<-read_csv("Outputs/dist_urban_centroids.csv") %>% glimpse

PA_br_urb<-read_sf("Outputs/PA_br.gpkg") %>% 
  left_join(dist_to_join %>% select(new_code, dist),
            by="new_code") %>% 
  rename(dist_urb=dist) %>% 
  glimpse

write_sf(PA_br_urb, "Outputs/PA_br_urb.gpkg")
