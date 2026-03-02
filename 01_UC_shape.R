# Gathering PA data

# source: MMA (https://dados.gov.br/dados/conjuntos-dados/unidadesdeconservacao)

library(tidyverse)
library(sf)
library(geobr)

sf_use_s2(FALSE)

UC_br<-st_read("DATA/MMA/cnuc_2025_08.shp") %>% 
  #mutate(
  #  across(.cols=c(3,4, 10, 11, 13, 16, 
  #                 grupo:cat_iucn, situacao, 
  #                 limite, nome_uc), 
  #         .fns=as.factor)
  #) %>%
  #summary()
  #ggplot()+geom_sf()
  #DataExplorer::plot_missing()
  glimpse

# cleaning
stp1_UC_br<-UC_br %>%  #3122
  select(3, 4, nome_uc, cria_ano, 
         10, 11, esfera, uf, municipio, 
         grupo:situacao) %>% 
  mutate(
    cria_ano=year(dmy(cria_ano)), 
    new_code=1:3122,
    new_code=paste0("UC_", new_code),
    cat_iucn=str_remove(cat_iucn, "Category "),
    grupo=ifelse(grupo=="Uso Sustentável", "US", "PI"),
    new_cat=case_when(
      categoria=="Área de Proteção Ambiental" ~ "APA", 
      categoria=="Área de Relevante Interesse Ecológico" ~ "ARIE", 
      categoria%in%c("Estação Ecológica",
                     "Monumento Natural",                       
                     "Parque",                                  
                     "Refúgio de Vida Silvestre",               
                     "Reserva Biológica" ) ~ "PI", 
      categoria%in%c("Floresta",
                     "Reserva de Desenvolvimento Sustentável",  
                     "Reserva de Fauna",                        
                     "Reserva Extrativista") ~ "US",
      categoria=="Reserva Particular do Patrimônio Natural" ~ "RPPN",
   )) %>% 
  filter(cria_ano>=1990 & cria_ano<=2024) %>% # 2,793
  filter(new_cat!="RPPN") %>% # 1,566
  glimpse

# name duplicated ----
check_name_duplicated<-stp1_UC_br %>% 
  st_drop_geometry() %>% 
  count(nome_uc) %>% 
  filter(n>1) %>% 
  #print()
  left_join(stp1_UC_br, by="nome_uc") %>% 
  mutate(nome_uc=as.factor(nome_uc)) %>% 
  group_by(nome_uc) %>% 
  group_split()

for (i in seq_along(check_name_duplicated)) {
  UC_name <- unique(check_name_duplicated[[i]]$nome_uc)
  
  p_name <- check_name_duplicated[[i]] %>% 
    left_join(stp1_UC_br %>% 
                select(nome_uc), 
              by="nome_uc") %>% 
    st_as_sf() %>% 
    ggplot(aes(fill = new_code)) +
    geom_sf(alpha=0.5) +
    ggtitle(UC_name)+
    theme_classic()
  
  print(p_name) 
}

#PARQUE ESTADUAL SERRA DA CANDONGA - Exclude
#PARQUE NATURAL MUNICIPAL AUGUSTO RUSCHI - join new_code
#PARQUE NATURAL MUNICIPAL DA MATA ATLÂNTICA - join new_code
#ÁREA DE PROTEÇÃO AMBIENTAL BOM JESUS - join new_code
#ÁREA DE PROTEÇÃO AMBIENTAL DE MACAÉ DE CIMA - Exclude
#ÁREA DE PROTEÇÃO AMBIENTAL SANTA FÉ - join new_code
#ÁREA DE PROTEÇÃO AMBIENTAL SANTO ANTÔNIO - join new_code

stp2_UC_br<-stp1_UC_br %>% 
  filter(!nome_uc %in% c(
    "PARQUE ESTADUAL SERRA DA CANDONGA", 
    "ÁREA DE PROTEÇÃO AMBIENTAL DE MACAÉ DE CIMA"
  )) %>% #-4=1562
  mutate(nome_uc=ifelse(
    nome_uc %in% c(
      "PARQUE NATURAL MUNICIPAL AUGUSTO RUSCHI", 
      "PARQUE NATURAL MUNICIPAL DA MATA ATLÂNTICA", 
      "ÁREA DE PROTEÇÃO AMBIENTAL BOM JESUS", 
      "ÁREA DE PROTEÇÃO AMBIENTAL SANTA FÉ", 
      "ÁREA DE PROTEÇÃO AMBIENTAL SANTO ANTÔNIO"), 
    paste(nome_uc, new_code, sep="_"),
    nome_uc)
    ) %>% 
  #filter(new_code=="UC_591") %>% # works well
  glimpse

# solving Ucs == marinho ----
stp3_UC_br<-stp2_UC_br %>% 
  mutate(
    only_marinha = ifelse(
      !is.na(marinho) & 
        is.na(amazonia) & 
        is.na(caatinga) & 
        is.na(cerrado) & 
        is.na(matlantica) & 
        is.na(pampa) & 
        is.na(pantanal),
      "only_ocean", "no_worries"
    )) %>% 
  filter(only_marinha!="only_ocean") %>% #11 
  select(-only_marinha, -amazonia:-situacao) %>% 
  glimpse #1,551

# counting PA by biome
br_biomes<-read_biomes(cache = F) %>% 
  select(name_biome)
  
st_intersection(stp3_UC_br, br_biomes) %>% 
  glimpse

# counting 
stp3_UC_br %>%
  st_drop_geometry() %>% 
  group_by(esfera, new_cat, categoria) %>% 
  summarise(n_uc=n_distinct(new_code)) %>% 
  print(n = 50)

stp3_UC_br %>%
  st_drop_geometry() %>% 
  group_by(esfera, new_cat, categoria, 
           pl_manejo) %>% 
  summarise(n_uc=n_distinct(new_code)) %>% 
  print(n = 200)

stp3_UC_br %>%
  st_drop_geometry() %>% 
  group_by(esfera, new_cat, categoria, 
           co_gestor) %>% 
  summarise(n_uc=n_distinct(new_code)) %>% 
  print(n = 200)
  