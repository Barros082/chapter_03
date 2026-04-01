# Testing Population data  
# source: https://zenodo.org/records/11179644

library(tidyverse)
library(sf)
library(terra)
library(raster)
library(exactextractr)

sf_use_s2(F)

PA<-read_sf("Outputs/PA_br.gpkg") %>%  
  st_transform("EPSG:4326")

Pop_90<-rast("DATA/POP/GlobPOP_Count_30arc_1990_I32.tiff")
#terra::summary(Pop_90)

mkcr_pop90<-mask(crop(Pop_90, PA), PA)
plot(mkcr_pop90)

totest_pop90<-PA %>% 
  mutate(
    pop_90=exact_extract(mkcr_pop90, ., "sum")) %>% 
  st_drop_geometry() %>% 
  group_by(biome, pa_type) %>% 
  summarise(
    across(pop_90, ~quantile(.x, na.rm = TRUE)),
    .groups = "drop") %>% 
  mutate(stat=rep(c("min", "q1", "median",
                    "q3", "max"), 10)) %>%
  dplyr::select(biome, pa_type, stat, pop_90) %>% 
  print(n=50)


#pop 2022 - IBGE data

popibge22<-readxl::read_xlsx("~/Doutorado/Cap02/DATA/pop_UC2022.xlsx") [-1:-5, c(-1, -4:-5)] %>%
  rename(COD_UC=`...2`, 
         nome_uc=`...3`,
         Pop=`...6` ) %>% 
  filter(!is.na(COD_UC)) %>%
  mutate(
    Pop=if_else(Pop=="-", "0", Pop), 
    COD_UC = str_pad(COD_UC, width = 5, 
                     side = "left",
                     pad = "0"), 
    COD_UC=as.factor(COD_UC), 
    Pop=as.numeric(Pop), 
    nome_uc=str_to_upper(nome_uc), 
    nome_uc=stringi::stri_trans_general(nome_uc, "Latin-ASCII")) %>%
  glimpse

PA_corr<-PA %>% 
  mutate(
    pop_90=exact_extract(mkcr_pop90, ., "sum"), 
    nome_uc=stringi::stri_trans_general(nome_uc, "Latin-ASCII")) %>% 
  st_drop_geometry() %>%
  dplyr::select(new_code, nome_uc, municipio, pop_90) %>%
  glimpse


PA_corr %>% 
  left_join(popibge22, by="nome_uc") %>% 
  filter(is.na(Pop)) %>% 
  glimpse

df_corr <- PA_corr %>% 
  left_join(popibge22, by = "nome_uc") %>% 
  dplyr::select(pop_90, Pop) %>% 
  filter(!is.na(Pop))

cor(df_corr$pop_90, df_corr$Pop, method = "pearson")
cor.test(df_corr$pop_90, df_corr$Pop, 
         method = "pearson")

ggplot(df_corr, aes(x = pop_90, y = Pop)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal()
