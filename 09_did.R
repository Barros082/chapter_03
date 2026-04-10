# Models and Sensitive test

library(tidyverse)
library(did)
library(HonestDiD)

PA_panel<-readRDS("Outputs/PA_br_panel.rds")

m_attgt<-list()
m_aggte<-list()
p_attgt<-list()
p_aggte<-list()
for (i in seq_along(PA_panel)) {
  pa_i<-PA_panel[[i]] %>% 
    mutate(id_numeric=as.numeric(str_remove(new_code,
                                            "UC_"))) %>% 
    filter()
  nm_pa_i<-names(PA_panel[i])
  
  m_attgt[[nm_pa_i]]<-att_gt(
    deforestation,
    year,
    idname = id_numeric,
    first_year_t,
    xformla = ~ esfera + PA_area + dist_agr + dist_urb + dist_energy + relative_pop,
    pa_i,
    panel = TRUE,
    control_group = "notyettreated",
    est_method = "dr",
    base_period = "universal"
  )
  
  p_attgt[[nm_pa_i]]<-ggdid()
  
  m_aggte[[nm_pa_i]]<-aggte()
  
  p_aggte[[nm_pa_i]]<-ggdid()
}


PA_panel$AMZ_PI %>%
  mutate(id_numeric=as.numeric(str_remove(new_code, "UC_"))) %>% View()
  filter()


attgt_AMZ_PI<-att_gt(
  yname ="deforestation",
  tname = "year",
  idname = "id_numeric",
  gname = "first_year_t",
  xformla = ~ dist_urb + dist_energy + relative_pop,
  PA_panel$AMZ_PI %>%
    mutate(id_numeric=as.numeric(str_remove(new_code, "UC_"))),
  panel = TRUE,
  control_group = "notyettreated",
  est_method = "dr",
  base_period = "universal"
)
ggdid(attgt_AMZ_PI)
te_AMZ_PI<-aggte(attgt_AMZ_PI, "dynamic", na.rm = TRUE)
ggdid(te_AMZ_PI)



summary(as.factor(PA_panel$AMZ_PI$first_year_t))
