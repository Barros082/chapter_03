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
                     "Parque",                                  
                     "Reserva Biológica" ) ~ "PI_hr", 
      categoria%in%c("Monumento Natural",                       
                     "Refúgio de Vida Silvestre") ~ "PI_lr", 
      categoria%in%c("Reserva de Desenvolvimento Sustentável",  
                     "Reserva Extrativista") ~ "US_cd",
      categoria=="Floresta" ~ "US_FLONA",
      categoria=="Reserva de Fauna" ~ "US_RF",
      categoria=="Reserva Particular do Patrimônio Natural" ~ "RPPN",
   )) %>% 
  filter(cria_ano>=1990) %>% # 2815
  filter(!new_cat%in%c("RPPN", "ARIE")) %>% # 1487
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

# solving Ucs == marinho and Duplicated biomes ----
stp3_UC_br<-stp2_UC_br %>% 
  mutate(
    only_marinha = if_else(
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
  mutate(multi_bioma = if_else(
    rowSums(!is.na(across(c(amazonia, caatinga, 
                            cerrado, matlantica, 
                            pampa, pantanal, 
                            marinho)))) > 1, 1, 0)) %>% 
  filter(multi_bioma!=1) %>% #221
  rowwise() %>%
  mutate(biomas =paste(
    names(pick(amazonia:marinho))[!is.na(c_across(amazonia:marinho))],
    collapse = "_"), 
    biomas=case_when(
      is.na(marinho) & 
        is.na(amazonia) & 
        is.na(caatinga) & 
        is.na(cerrado) & 
        is.na(matlantica) & 
        is.na(pampa) & 
        is.na(pantanal) ~ "issue", 
      TRUE ~ biomas
    )) %>%
  ungroup() %>% 
  select(-only_marinha, -multi_bioma,
         -amazonia:-situacao) %>% 
  glimpse #1,551


stp4_UC_br<-stp3_UC_br %>% 
  mutate(testbm=st_intersects(., read_biomes(cache = F)),
         testbm=as.character(testbm),
         dum_bm2=case_when(
           testbm%in%c("1", "c(1, 7)")~"amazonia",
           testbm%in%c("2", "c(2, 7)")~"caatinga",
           testbm%in%c("3", "c(3, 7)")~"cerrado",
           testbm%in%c("4", "c(4, 7)")~ "matlantica",
           testbm%in%c("5", "c(5, 7)")~"pampa",
           testbm%in%c("6", "c(6, 7)")~"pantanal",
           testbm=="7"~"Marinho",
           TRUE ~ "duplicated"
         )) %>%
  filter(dum_bm2!="duplicated") %>% #3
  mutate(biome=if_else(biomas=="issue",
                       dum_bm2, biomas)) %>% 
  select(-biomas:-dum_bm2) %>% 
  glimpse
  
  
# counting (wide PA categories) by biome
stp4_UC_br %>%
  filter(cria_ano>=1991) %>% #1157
  mutate(dum_yr_class=factor(case_when(
    cria_ano>=1991 & cria_ano<=1995~"91_95",
    cria_ano>=1996 & cria_ano<=2000~"96_00",
    cria_ano>=2001 & cria_ano<=2005~"01_05", 
    cria_ano>=2006 & cria_ano<=2010~"06_10", 
    cria_ano>=2011 & cria_ano<=2015~"11_15",
    cria_ano>=2016 & cria_ano<=2020~"16_20", 
    TRUE ~ "21-25"), 
    levels=c("91_95", "96_00", "01_05", "06_10",
             "11_15", "16_20", "21-25"))) %>% 
  st_drop_geometry() %>% 
  group_by(biome, esfera, new_cat, dum_yr_class
           ) %>% 
  summarise(n_uc=n_distinct(new_code)) %>% 
  print(n = 400)


stp4_UC_br %>% 
  mutate(
    new_cat_2=case_when(
      categoria=="Área de Proteção Ambiental" ~ "APA", 
      categoria%in%c("Estação Ecológica",
                     "Parque",                                  
                     "Reserva Biológica",
                     "Monumento Natural",                       
                     "Refúgio de Vida Silvestre") ~ "PI", 
      categoria%in%c("Reserva de Desenvolvimento Sustentável",  
                     "Reserva Extrativista", 
                     "Floresta", 
                     "Reserva de Fauna") ~ "US")) %>% 
  filter(cria_ano>=1991) %>% #1157
  mutate(dum_yr_class=factor(case_when(
    cria_ano>=1991 & cria_ano<=1995~"91_95",
    cria_ano>=1996 & cria_ano<=2000~"96_00",
    cria_ano>=2001 & cria_ano<=2005~"01_05", 
    cria_ano>=2006 & cria_ano<=2010~"06_10", 
    cria_ano>=2011 & cria_ano<=2015~"11_15",
    cria_ano>=2016 & cria_ano<=2020~"16_20", 
    TRUE ~ "21-25"), 
    levels=c("91_95", "96_00", "01_05", "06_10",
             "11_15", "16_20", "21-25"))) %>% 
  st_drop_geometry() %>% 
  group_by(biome, esfera, new_cat_2, dum_yr_class
  ) %>% 
  summarise(n_uc=n_distinct(new_code)) %>% 
  print(n = 300)


stp4_UC_br %>% 
  mutate(
    new_cat_2=case_when(
      categoria=="Área de Proteção Ambiental" ~ "APA", 
      categoria%in%c("Estação Ecológica",
                     "Parque",                                  
                     "Reserva Biológica",
                     "Monumento Natural",                       
                     "Refúgio de Vida Silvestre") ~ "PI", 
      categoria%in%c("Reserva de Desenvolvimento Sustentável",  
                     "Reserva Extrativista", 
                     "Floresta", 
                     "Reserva de Fauna") ~ "US")) %>% 
  filter(cria_ano>=1996 &
           cria_ano<=2024) %>% #dim()#1117
  mutate(dum_yr_class=factor(case_when(
    cria_ano>=1996 & cria_ano<=2000~"96_00",
    cria_ano>=2001 & cria_ano<=2005~"01_05", 
    cria_ano>=2006 & cria_ano<=2010~"06_10", 
    cria_ano>=2011 & cria_ano<=2015~"11_15",
    cria_ano>=2016 & cria_ano<=2020~"16_20", 
    TRUE ~ "21-24"), 
    levels=c("96_00", "01_05", "06_10",
             "11_15", "16_20", "21-24"))) %>% 
  st_drop_geometry() %>% 
  group_by(biome, new_cat_2, dum_yr_class) %>% 
  summarise(n_uc=n_distinct(new_code)) %>% 
  print(n = 300)
  

# saving 

UC_br_end<-stp4_UC_br %>% 
  mutate(
    pa_type=case_when(
      categoria=="Área de Proteção Ambiental" ~ "APA", 
      categoria%in%c("Estação Ecológica",
                     "Parque",                                  
                     "Reserva Biológica",
                     "Monumento Natural",                       
                     "Refúgio de Vida Silvestre") ~ "PI", 
      categoria%in%c("Reserva de Desenvolvimento Sustentável",  
                     "Reserva Extrativista", 
                     "Floresta", 
                     "Reserva de Fauna") ~ "US")) %>% 
  filter(cria_ano>=1996 &
           cria_ano<=2024) %>% #dim()#1117
  filter(!biome%in%c("Marinho", "pampa",
                     "pantanal")) %>% #dim()#1100
  mutate(first_year_t=case_when(
  biome%in%c("caatinga", "cerrado") &
    pa_type == "US" ~ "exclude", 
  #gruping into 10 years
  biome=="amazonia" &
    pa_type == "APA" &
    cria_ano>=1996 & cria_ano<=2005 ~ "1996",
  biome=="amazonia" &
    pa_type == "APA" &
    cria_ano>=2006 & cria_ano<=2015 ~ "2006", 
  biome=="amazonia" &
    pa_type == "APA" &
    cria_ano>=2016 ~ "0", 
  biome=="matlantica" &
    pa_type == "US" &
    cria_ano>=1996 & cria_ano<=2005 ~ "1996",
  biome=="matlantica" &
    pa_type == "US" &
    cria_ano>=2006 & cria_ano<=2015 ~ "2006",
  biome=="matlantica" &
    pa_type == "US" &
    cria_ano>=2016 ~ "0",
  # removing unique years class
  biome=="amazonia" &
    pa_type == "US" &
    cria_ano>=2011 & cria_ano<=2015 ~ "exclude",
  biome=="cerrado" &
    pa_type == "APA" &
    cria_ano>=2011 & cria_ano<=2015 ~ "exclude",
  # general
  cria_ano>=1996 & cria_ano<=2000~"1996",
  cria_ano>=2001 & cria_ano<=2005~"2001", 
  cria_ano>=2006 & cria_ano<=2010~"2006", 
  cria_ano>=2011 & cria_ano<=2015~"2011",
  cria_ano>=2016 & cria_ano<=2020~"2016", 
  cria_ano>=2021~ "0",
  TRUE ~ NA), 
  across(c(new_code, biome, pa_type), 
         ~as.factor(.))) %>%
  select(-new_cat) %>%
  filter(first_year_t!="exclude") %>% #dim()#1082 (lose 18)
  mutate(first_year_t=as.integer(first_year_t)) %>% 
  glimpse

#crs=4674
#dir.create("Outputs/matching_did")
write_sf(UC_br_end, "Outputs/matching_did/PA_br.gpkg")

