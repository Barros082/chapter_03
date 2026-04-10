# panel creation

library(tidyverse)
library(sf)
library(plm)

sf_use_s2(F)

PA<-read_sf("Outputs/PA_br.gpkg") %>% 
  left_join(read_sf("Outputs/PA_br_agr.gpkg") %>% 
              st_drop_geometry()) %>%
  left_join(read_sf("Outputs/PA_br_urb.gpkg") %>%
              st_drop_geometry()) %>% 
  left_join(readRDS("Outputs/PA_dist_energy.rds")) %>%
  left_join(readRDS("Outputs/PA_popcount.rds") %>% 
              dplyr::select(new_code, relative_pop)) %>% 
  st_transform(., "EPSG:5880") %>% 
  mutate(PA_area=as.numeric(st_area(geom)), 
         first_year_t=if_else(first_year_t==0, 
                              cria_ano, first_year_t)) %>%
  dplyr::select(new_code, nome_uc, biome, pa_type,
                cria_ano:esfera, PA_area, 
                dist_agr:relative_pop, 
                first_year_t) %>% #dim() #1161
  st_drop_geometry() %>% 
  glimpse 

#summary(as.factor(PA$first_year_t))
#PA %>% filter(first_year_t==0) %>% View()

PA_list<-lapply(readRDS("Outputs/PA_defor.rds") %>% 
                  st_drop_geometry(),
                function(x){
  x_done<-x %>% 
    mutate(first_year_t=if_else(first_year_t==0, 
                                cria_ano, first_year_t)) %>% 
    left_join(PA) %>% 
    dplyr::filter(!first_year_t>=2024) %>% 
    dplyr::filter(year!=2024)
  
  print(summary(as.factor(x_done$first_year_t)))
  DataExplorer::plot_missing(x_done)
  
  x_done_na<-x_done %>% drop_na() # we lose MAT_US 1 PA (UC_2791)
  return(x_done_na)
})
#lapply(PA_list, is.pbalanced)

PA_panel<-list()
for (i in seq_along(PA_list)) {
  i_pm<-PA_list[[i]]
  i_nm_pm<-names(PA_list)[i]
  pm<-pdata.frame(i_pm, index = c("new_code", 
                              "year"))
  print(is.pbalanced(pm))
  print(pdim(pm))
  
  PA_panel[[i_nm_pm]]<-pm
}


saveRDS(PA_panel, "Outputs/PA_br_panel.rds")


