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
  
  
# counting (wide PA categories) by biome ----

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
             "11_15", "16_20", "21-25")), 
    across(.cols=c(pl_manejo, co_gestor), 
           ~ if_else(.=="Sem informação", "Não", .))) %>%
  st_drop_geometry() %>% 
  group_by(biome, new_cat_2, #esfera, 
           pl_manejo, dum_yr_class) %>% 
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
             "11_15", "16_20", "21-25")), 
    across(.cols=c(pl_manejo, co_gestor), 
           ~ if_else(.=="Sem informação", "Não", .))) %>%
  st_drop_geometry() %>% 
  group_by(biome, new_cat_2, #esfera, 
           pl_manejo, #dum_yr_class
           co_gestor) %>% 
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
             "11_15", "16_20", "21-25")), 
    across(.cols=c(pl_manejo, co_gestor), 
           ~ if_else(.=="Sem informação", "Não", .))) %>%
  st_drop_geometry() %>% 
  group_by(biome, new_cat_2, esfera, 
           pl_manejo, #dum_yr_class
           #co_gestor
           ) %>% 
  summarise(n_uc=n_distinct(new_code)) %>% 
  print(n = 300)

#write.csv(stp4_UC_br %>% 
#            st_drop_geometry, "Outputs/tocollect_PMCG_yr.csv")

stp5_UC_br <-read.csv("Outputs/collected_PMCG_yr.csv") %>% 
  #DataExplorer::plot_missing()
  filter(!uc_id%in%c(
    384, 2906, 2941, 1506, 1997, 2558, 
    2750, 1763, 10409, 15997, 16872, 
    17096,  21017, 21673, 21926, 
    22407, 22444, 22648, 22990, 23259
  )) %>% # -20 UCS 
  filter(usavel_e_condicao_praPM!="excluir") %>% #-25 UCS
  mutate(
    new_pl_manejo=if_else(
      usavel_e_condicao_praPM%in%c("Sim", 
                                   "Sim_2015",
                                   "Sim_2002",
                                   "Sim_2013"),
      "Sim", pl_manejo),
    new_pl_manejo=case_when(
      new_pl_manejo=="Sem informação" ~ "Não",
      TRUE ~ new_pl_manejo),
    usavel_e_condicao_praPM=case_when(
      usavel_e_condicao_praPM=="Sim_2015" ~ "2015",
      usavel_e_condicao_praPM=="Sim_2002" ~ "2002",
      usavel_e_condicao_praPM=="Sim_2013" ~ "2013",
      TRUE ~ usavel_e_condicao_praPM),
    new_cria_ano=case_when(
      !usavel_e_condicao_praPM%in%c("s", "Sim") ~ as.integer(usavel_e_condicao_praPM), 
      TRUE ~ cria_ano),
    new_cat=case_when(
      categoria=="Área de Proteção Ambiental" ~ "APA", 
      categoria%in%c("Estação Ecológica",
                     "Parque",                                  
                     "Reserva Biológica",
                     "Monumento Natural",                       
                     "Refúgio de Vida Silvestre") ~ "PI", 
      categoria%in%c("Reserva de Desenvolvimento Sustentável",  
                     "Reserva Extrativista", 
                     "Floresta", 
                     "Reserva de Fauna") ~ "US"),
    yr_PM=str_remove(yr_PM, "X_"),
    yr_PM=if_else(yr_PM=="", 
                  0, as.integer(yr_PM)),
    pretreat_period=case_when(
      yr_PM==0 ~ 9999,
      TRUE ~ new_cria_ano-yr_PM)
    ) %>% 
  #filter(is.na(new_cria_ano)) %>% glimpse
  select(-X, -cria_ano:-co_gestor,
         -yr_CG:-motivo) %>% 
  glimpse

stp5_UC_br %>%  summary()

stp5_UC_br %>% 
  group_by(new_pl_manejo, yr_PM) %>% 
  summarise(n_uc=n_distinct(new_code)) %>% 
  print(n = 400)


#grouping by three
stp5_UC_br %>% #1204
  #(unless two pos and pre - period)
  mutate(
    dum_yr=case_when(
      new_cria_ano<=2022 ~ "ok",
      new_cria_ano>=2023 & 
        new_pl_manejo=="Não" ~ "ok",
      new_cria_ano>=2023 & 
        new_pl_manejo=="Sim" ~ "exclude")) %>%
  filter(dum_yr!="exclude") %>% #dim() #-05=1199
  filter(!pretreat_period%in%c(0, -1)) %>% #dim() #-17=1182
  # filtering any PM created above 2023
  filter(yr_PM<=2022) %>% #dim() #-58=1124
  #continue
  mutate(dum_yrPM_class=factor(case_when(
    yr_PM>=1991 & yr_PM<=1994~"91_94",
    yr_PM>=1995 & yr_PM<=1998~"95_98",
    yr_PM>=1999 & yr_PM<=2002~"99_02", 
    yr_PM>=2003 & yr_PM<=2006~"03_06", 
    yr_PM>=2007 & yr_PM<=2010~"07_10",
    yr_PM>=2011 & yr_PM<=2014~"11_14",
    yr_PM>=2015 & yr_PM<=2018~"15_18",
    yr_PM>=2019 & yr_PM<=2022~"19_22",
    yr_PM==0 ~ "0",
    TRUE ~ "tosee"), 
    levels=c("91_94", "95_98", "99_02", "03_06",
             "07_10", "11_14", "15_18", "19_22",
             "0", "tosee"))) %>%
  #filter(dum_yrPM_class=="tosee") %>% glimpse
  group_by(biome, new_cat, new_pl_manejo, 
           dum_yrPM_class) %>% 
  summarise(n_uc=n_distinct(new_code)) %>% 
  print(n = 400)

#general
stp5_UC_br %>% #1204
  #(unless two pos and pre - period)
  mutate(
    dum_yr=case_when(
      new_cria_ano<=2022 ~ "ok",
      new_cria_ano>=2023 & 
        new_pl_manejo=="Não" ~ "ok",
      new_cria_ano>=2023 & 
        new_pl_manejo=="Sim" ~ "exclude")) %>%
  filter(dum_yr!="exclude") %>% #dim() #-05=1199
  filter(!pretreat_period%in%c(0, -1)) %>% #dim() #-17=1182
  # filtering any PM created above 2023
  filter(yr_PM<=2022) %>% #dim() #-58=1124
  #continue
  mutate(dum_yrPM_class=factor(case_when(
    yr_PM>=1991 & yr_PM<=1994~"91_94",
    yr_PM>=1995 & yr_PM<=1998~"95_98",
    yr_PM>=1999 & yr_PM<=2002~"99_02", 
    yr_PM>=2003 & yr_PM<=2006~"03_06", 
    yr_PM>=2007 & yr_PM<=2010~"07_10",
    yr_PM>=2011 & yr_PM<=2014~"11_14",
    yr_PM>=2015 & yr_PM<=2018~"15_18",
    yr_PM>=2019 & yr_PM<=2022~"19_22",
    yr_PM==0 ~ "0",
    TRUE ~ "tosee"), 
    levels=c("91_94", "95_98", "99_02", "03_06",
             "07_10", "11_14", "15_18", "19_22",
             "0", "tosee"))) %>%
  #filter(dum_yrPM_class=="tosee") %>% glimpse
  group_by(new_pl_manejo, 
           dum_yrPM_class) %>% 
  summarise(n_uc=n_distinct(new_code)) %>% 
  print(n = 400)


# a respeito dos pre-periods
#stp5_UC_br %>% #1204
  #(unless two pos and pre - period)
#  mutate(
#    dum_yr=case_when(
#      new_cria_ano<=2022 ~ "ok",
#      new_cria_ano>=2023 & 
#        new_pl_manejo=="Não" ~ "ok",
#      new_cria_ano>=2023 & 
#        new_pl_manejo=="Sim" ~ "exclude")) %>%
#  filter(dum_yr!="exclude") %>% #dim() #-05=1199
#  filter(!pretreat_period%in%c(0, -1)) %>% #dim() #-17=1182
  # filtering any PM created above 2023
#  filter(yr_PM<=2022) %>% #dim() #-58=1124
  #continue
#  mutate(dum_yrPM_class=factor(case_when(
#    yr_PM>=1991 & yr_PM<=1994~"91_94",
#    yr_PM>=1995 & yr_PM<=1998~"95_98",
#    yr_PM>=1999 & yr_PM<=2002~"99_02", 
#    yr_PM>=2003 & yr_PM<=2006~"03_06", 
#    yr_PM>=2007 & yr_PM<=2010~"07_10",
#    yr_PM>=2011 & yr_PM<=2014~"11_14",
#    yr_PM>=2015 & yr_PM<=2018~"15_18",
#    yr_PM>=2019 & yr_PM<=2022~"19_22",
#    yr_PM==0 ~ "0",
#    TRUE ~ "tosee"), 
#    levels=c("91_94", "95_98", "99_02", "03_06",
#             "07_10", "11_14", "15_18", "19_22",
#             "0", "tosee"))) %>%
  #filter(dum_yrPM_class=="tosee") %>% glimpse
#  group_by(new_pl_manejo, 
#           dum_yrPM_class, pretreat_period) %>% 
#  summarise(n_uc=n_distinct(new_code)) %>% 
#  print(n = 400)


#### saving ----
UC_br_end<-stp5_UC_br %>% #1204
  #(unless two pos and pre - period)
  mutate(
    dum_yr=case_when(
      new_cria_ano<=2022 ~ "ok",
      new_cria_ano>=2023 & 
        new_pl_manejo=="Não" ~ "ok",
      new_cria_ano>=2023 & 
        new_pl_manejo=="Sim" ~ "exclude")) %>%
  filter(dum_yr!="exclude") %>% #dim() #-05=1199
  filter(!pretreat_period%in%c(0, -1)) %>% #dim() #-17=1182
  # filtering any PM created above 2023
  filter(yr_PM<=2022) %>% #dim() #-58=1124
  #continue
  mutate(dum_yrPM_class=factor(case_when(
    yr_PM>=1991 & yr_PM<=1994~"91_94",
    yr_PM>=1995 & yr_PM<=1998~"95_98",
    yr_PM>=1999 & yr_PM<=2002~"99_02", 
    yr_PM>=2003 & yr_PM<=2006~"03_06", 
    yr_PM>=2007 & yr_PM<=2010~"07_10",
    yr_PM>=2011 & yr_PM<=2014~"11_14",
    yr_PM>=2015 & yr_PM<=2018~"15_18",
    yr_PM>=2019 & yr_PM<=2022~"19_22",
    yr_PM==0 ~ "0",
    TRUE ~ "tosee"), 
    levels=c("91_94", "95_98", "99_02", "03_06",
             "07_10", "11_14", "15_18", "19_22",
             "0", "tosee"))) %>%
  filter(!biome%in%c("pampa", "pantanal",
                     "Marinho")) %>% #dim() #-17=1107
  filter(!(biome=="caatinga" & new_cat=="US")) %>% #dim()#-1=1106
  filter(!(biome=="amazonia" & 
             new_cat=="PI" &
             dum_yrPM_class%in%c("99_02",
                                 "03_06"))) %>% #dim()#-2=1104
  filter(!(biome=="cerrado" & 
             new_cat=="APA" &
             dum_yrPM_class%in%c("95_98",
                                 "11_14"))) %>% #dim()#-2=1102
  filter(!(biome=="cerrado" & 
             new_cat=="PI" &
             dum_yrPM_class=="99_02")) %>% #dim()#-1=1101
  filter(!(biome=="matlantica" & 
             new_cat=="PI" &
             dum_yrPM_class=="99_02")) %>% #dim()#-1=1100
  glimpse


UC_br_end_geom<-UC_br_end %>% 
  left_join(stp4_UC_br %>% select(new_code),
            by="new_code") %>%
  st_as_sf() %>% 
  #st_crs()#4674
  #mutate(test_geom=st_is_valid(geometry)) %>% 
  #filter(test_geom==F) %>% #none
  glimpse

saveRDS(UC_br_end, "Outputs/PA_br.rds")
write_sf(UC_br_end_geom, "Outputs/PA_br.gpkg")


