# Covariates to 1995


## distance to urban and agriculture pixels ----
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

lu_mapbiomas<-ee$Image("projects/mapbiomas-public/assets/brazil/lulc/collection10_1/mapbiomas_brazil_collection10_1_coverage_v1")

lu_95<-lu_mapbiomas$select("classification_1995")

urban_px<-lu_95$eq(24)$selfMask()
agr_px<-lu_95$
  remap(
    from = c(14, 15, 18, 19, 39, 20, 40,
             62, 41, 36, 46, 47, 35, 48, 
             9, 21),
    to = rep(1, 16), defaultValue = 0)$
  rename("classification_1995")

dist_urb<-urban_px$
  fastDistanceTransform(neighborhood = 1000)$
  sqrt()$
  multiply(30)$
  rename("classification_1995")

dist_agr<-agr_px$
  fastDistanceTransform(neighborhood = 1000)$
  sqrt()$
  multiply(30)$
  rename("classification_1995")


library(tidyverse)
library(sf)
sf_use_s2(FALSE)

centroids_ee<-read_sf("Outputs/matching_did/PA_br.gpkg") %>% #crs:4674 (mapbiomas standard)
  #fixing first_year_treat column
  st_transform("EPSG:5880") %>% 
  st_centroid() %>%
  dplyr::select(new_code) %>% 
  sf_as_ee()

centroids_dist <- centroids_ee$map(
  ee_utils_pyfunc(function(i){
    urb_val <- dist_urb$reduceRegion(
      reducer = ee$Reducer$first(),
      geometry = i$geometry(),
      scale = 30
    )$get("classification_1995")
    agr_val <- dist_agr$reduceRegion(
      reducer = ee$Reducer$first(),
      geometry = i$geometry(),
      scale = 30
    )$get("classification_1995")
    
    i$set(list(
      dist_urb = urb_val,
      dist_agr = agr_val
    ))
  }))

task <- ee_table_to_drive(
  collection = centroids_dist,
  description = "dist_centroids",
  fileFormat = "CSV"
)

#ee_user_info()
task$start()

dist_to_join<-read_csv("Outputs/matching_did/dist_centroids.csv") %>% 
  select(new_code, contains("dist_")) %>% 
  rename(dist95_agr=dist_agr, 
         dist95_urb=dist_urb) %>% glimpse

saveRDS(dist_to_join, "Outputs/matching_did/PA_br_dist95_urb_agr.rds")

#### Distance to energy lines ----
# Source: Mapbiomas (https://brasil.mapbiomas.org/dados-de-infraestrutura/)

library(tidyverse)
library(sf)        
sf::sf_use_s2(F)

#### Testing roads and railway data 
list_shp_infr<-list.files("DATA/Mapbiomas", 
                          pattern = ".shp",
                          full.names = T)
shp_infr<-lapply(list_shp_infr, function(x){
  x_done<-read_sf(x) %>% 
    st_transform(., "EPSG:5880")
  
  print(colnames(x_done))
  return(x_done)
})
# roads --> without year 
# railway tracks --> years btw 1884-1996 
### tehre are several NA in year. So i opted to exlude it.
# energy lines --> OK
summary(as.factor(shp_infr[[2]]$Ano_Opera))

eng_ln95<-shp_infr[[2]] %>% 
  filter(Ano_Opera>=1 & Ano_Opera<=1995) %>%
  st_union()

PA_dist95_eng_line<-read_sf("Outputs/matching_did/PA_br.gpkg") %>% 
  st_transform(., "EPSG:5880") %>% 
  select(new_code) %>% 
  mutate(dist_energy=as.numeric(st_distance(
    .,
    eng_ln95, 
    which ="Euclidean",
    by_element = F))) %>% 
  st_drop_geometry() %>% 
  glimpse
  
#### Population data ----
# source: https://zenodo.org/records/11179644
library(terra)
library(raster)
library(exactextractr)

pop_data<-list.files("DATA/POP", full.names = T)
pop95<-raspop_data[2]
#crs(rast(pop95))#4326

PA_pop95<-read_sf("Outputs/matching_did/PA_br.gpkg") %>% 
  st_transform(., "EPSG:4326") %>% 
  dplyr::select(new_code) %>% 
  mutate(Pop95=exact_extract(rast(pop95), ., "sum")) %>% 
  st_drop_geometry() %>% glimpse
  
#### saving ----
PA_cov95<-full_join(PA_dist95_eng_line, PA_pop95) %>% 
  full_join(readRDS("Outputs/matching_did/PA_br_dist95_urb_agr.rds")) %>% 
  glimpse

saveRDS(PA_cov95, 
        "Outputs/matching_did/PA_br_allcov95.rds")
