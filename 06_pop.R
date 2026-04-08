# Gathering population data 
# source: https://zenodo.org/records/11179644

library(tidyverse)
library(sf)
library(terra)
library(raster)
library(exactextractr)

sf_use_s2(F)

PA_4326<-read_sf("Outputs/PA_br.gpkg") %>%
  st_transform("EPSG:4326")

pop_data<-list.files("DATA/POP", full.names = T)
nm_pop<-c("pop_1990", "pop_1995", "pop_2000", "pop_2005", 
  "pop_2010", "pop_2015", "pop_2020",
  "pop_2021", "pop_2022")

pop_count<-list()
for (i in seq_along(pop_data)) {
  p_i<-pop_data[[i]]
  r_p_i<-rast(p_i)
  pop_count[[nm_pop[i]]]<-r_p_i
}

pa_list<-PA_4326 %>% 
  mutate(first_year_t=if_else(first_year_t==0, 
                              cria_ano, first_year_t), 
         fty_control=case_when(
           first_year_t == 1991 ~ "pop_1990",
           first_year_t == 1996 ~ "pop_1995",
           first_year_t == 2001 ~ "pop_2000",
           first_year_t == 2006 ~ "pop_2005",
           first_year_t == 2011 ~ "pop_2010",
           first_year_t == 2016 ~ "pop_2015",
           first_year_t == 2021 ~ "pop_2020",
           first_year_t == 2022 ~ "pop_2021",
           first_year_t %in%c(2023, 2024, 2025) ~ "pop_2022"
         )) %>%
  dplyr::select(new_code, cria_ano, 
                first_year_t, fty_control) %>% 
  group_by(fty_control) %>% 
  group_split()

pop_pa_lst <- lapply(pa_list, function(pa_i) {
  year_name <- unique(pa_i$fty_control)
  pop_year <- pop_count[[year_name]]
  pa_i %>% 
    mutate(value_pop = exact_extract(pop_year, ., "sum"))
})

PA_pop<-do.call(rbind, pop_pa_lst) %>% 
  rename(fty_pop_control=fty_control, 
         relative_pop=value_pop) %>% 
  st_drop_geometry() %>% 

saveRDS(PA_pop, "Outputs/PA_popcount.rds")

