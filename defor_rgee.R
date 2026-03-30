# Deforestation area with GEE
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

defor_mpb<-ee$Image("projects/mapbiomas-public/assets/brazil/lulc/collection10/mapbiomas_brazil_collection10_deforestation_secondary_vegetation_v2")

#defor_mpb$bandNames()$getInfo()

bd_yr_nm<-c(
  "classification_1990",
  "classification_1991", "classification_1992",
  "classification_1993", "classification_1994",
  "classification_1995", "classification_1996",
  "classification_1997", "classification_1998",
  "classification_1999", "classification_2000",
  "classification_2001", "classification_2002",
  "classification_2003", "classification_2004",
  "classification_2005", "classification_2006",
  "classification_2007", "classification_2008",
  "classification_2009", "classification_2010",
  "classification_2011", "classification_2012",
  "classification_2013", "classification_2014",
  "classification_2015", "classification_2016",
  "classification_2017", "classification_2018",
  "classification_2019", "classification_2020",
  "classification_2021", "classification_2022",
  "classification_2023", "classification_2024")

defor_9024<-defor_mpb$select(bd_yr_nm)
#defor_9024$bandNames()$getInfo()

df_class4_6<-ee$ImageCollection$fromImages(
  defor_9024$bandNames()$map(
    ee_utils_pyfunc(function(bd) {
      bd_d<-ee$String(bd)
      df_bd<-defor_9024$select(bd_d)
      
      class_df_bd<-df_bd$remap(
        from=c(4, 6), 
        to= rep(1, 2),
        defaultValue=0)
      
      return(class_df_bd$set("year", bd))
  })))

PA_ee<-ee$FeatureCollection("projects/test-arthurbarrosar/assets/PA_br_91_25")

area_mpb<-df_class4_6$map(
  ee_utils_pyfunc(function(dt) {
    dt_area <- dt$multiply(ee$Image$pixelArea()) #trasnform from pixels to meters
    
    ar_m <- dt_area$reduceRegions(
      collection = PA_ee,
      reducer = ee$Reducer$sum(), #sum each pixel value (now in meters)
      scale = 30,
      #crs = "EPSG:5880" #reducing memory usage
      )
    
    ar_m_end <- ar_m$map(
      ee_utils_pyfunc(function(end_dist) {
        end_dist$set("year", dt$get("year"))
      }))
    return(ar_m_end)
}))$flatten()

task <- ee_table_to_drive(
  collection = area_mpb,
  description = "PA_defor",
  fileFormat = "CSV"
)

#ee_user_info()
task$start()

