# Gathering distance to energy lines

# Source: Mapbiomas (https://brasil.mapbiomas.org/dados-de-infraestrutura/)

library(tidyverse)
library(sf)        

sf::sf_use_s2(F)

#### Testing roads and railway data ----
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
# energy lines --> OK
summary(as.factor(shp_infr[[2]]$Ano_Opera))

#### Minimun distance btw PA and energy lines----
energy<-shp_infr[[2]] %>% 
  filter(Ano_Opera>=1) %>%  #removing 0 values
  mutate(
    dm_togroup=case_when(
      Ano_Opera<=1990 ~ "y90", 
      Ano_Opera>=1991 & Ano_Opera<=1995 ~ "y91_95",
      Ano_Opera>=1996 & Ano_Opera<=2000 ~ "y96_00",
      Ano_Opera>=2001 & Ano_Opera<=2005 ~ "y01_05",
      Ano_Opera>=2006 & Ano_Opera<=2010 ~ "y06_10",
      Ano_Opera>=2011 & Ano_Opera<=2015 ~ "y11_15",
      Ano_Opera>=2016 & Ano_Opera<=2020 ~ "y16_20",
      Ano_Opera==2021 ~ "y2020",
      Ano_Opera==2022 ~ "y2021")) %>% glimpse

list_energy<-lapply(list(
  c_91_95=energy %>%
    filter(dm_togroup=="y90"),
  c_96_00=energy %>%
    filter(dm_togroup%in%c("y90", "y91_95")),
  c_01_05=energy %>%
    filter(dm_togroup%in%c("y90", "y91_95", "y96_00")),
  c_06_10=energy %>%
    filter(dm_togroup%in%c("y90", "y91_95", 
                           "y96_00", "y01_05")),
  c_11_15=energy %>%
    filter(dm_togroup%in%c("y90", "y91_95", 
                           "y96_00", "y01_05", 
                           "y06_10")),
  c_16_20=energy %>%
    filter(dm_togroup%in%c("y90", "y91_95", 
                           "y96_00", "y01_05", 
                           "y06_10", "y11_15")),
  c_21=energy %>%
    filter(dm_togroup%in%c("y90", "y91_95", 
                           "y96_00", "y01_05", 
                           "y06_10", "y11_15", 
                           "y16_20")),
  c_22=energy %>%
    filter(dm_togroup%in%c("y90", "y91_95", 
                           "y96_00", "y01_05", 
                           "y06_10", "y11_15", 
                           "y16_20", "y2020")),
  c_23_24_25=energy %>%
    filter(dm_togroup%in%c("y90", "y91_95", 
                           "y96_00", "y01_05", 
                           "y06_10", "y11_15", 
                           "y16_20", "y2020", "y2021"))
  ), st_union)
  

list_PA_5880<-read_sf("Outputs/PA_br.gpkg") %>% 
  mutate(first_year_t=if_else(first_year_t==0, 
                              cria_ano, first_year_t)) %>%
  st_transform(., "EPSG:5880") %>% 
  mutate(cht_match = case_when(
    first_year_t == 1991 ~ "c_91_95",
    first_year_t == 1996 ~ "c_96_00",
    first_year_t == 2001 ~ "c_01_05",
    first_year_t == 2006 ~ "c_06_10",
    first_year_t == 2011 ~ "c_11_15",
    first_year_t == 2016 ~ "c_16_20",
    first_year_t == 2021 ~ "c_21",
    first_year_t == 2022 ~ "c_22",
    first_year_t%in%c(2023, 2024,
                      2025) ~ "c_23_24_25"
  )) %>% 
  dplyr::select(new_code, cria_ano, 
                first_year_t, cht_match) %>% 
  group_by(cht_match) %>% 
  group_split()

dtenergy_pa_lst <- lapply(list_PA_5880, function(i) {
  cohort_name <- unique(i$cht_match)
  energy_cohort <- list_energy[[cohort_name]]
  
  dist_energy<-i %>% 
    select(new_code) %>% 
    mutate(dist_energy=as.numeric(st_distance(
    i,
    energy_cohort, 
    which ="Euclidean",
    by_element = F))) %>% 
    st_drop_geometry()
  return(dist_energy)
})

PA_dist_energy<-do.call(rbind, dtenergy_pa_lst) %>% 
  glimpse

saveRDS(PA_dist_energy, "Outputs/PA_dist_energy.rds")
