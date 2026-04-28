# Matching using baseline covariates (1995)

library(tidyverse)
library(MatchIt)
library(cobalt)
library(corrplot)
library(sf)
sf_use_s2(FALSE)

pa_cov95<-readRDS("Outputs/matching_did/PA_br_allcov95.rds") %>% glimpse

PA_list<-read_sf("Outputs/matching_did/PA_br.gpkg") %>% 
  st_transform("EPSG:5880") %>% 
  mutate(area=as.numeric(st_area(geom))) %>%
  st_drop_geometry() %>% 
  left_join(pa_cov95, by="new_code") %>% 
  #group_by(biome, pa_type, first_year_t) %>% 
  #summarise(n_distinct(new_code)) %>%  print(n=100)
  # allright! 
  mutate(treat=as.factor(if_else(first_year_t==0, 0, 1))) %>% 
  #group_by(biome, pa_type, treat) %>% 
  #summarise(n_distinct(new_code)) %>%  print(n=100)
  group_by(biome, pa_type) %>% 
  group_split()

names(PA_list) <- c("AMZ_APA", "AMZ_PI", "AMZ_US",
                    "CAAT_APA", "CAAT_PI", 
                    "CER_APA","CER_PI", 
                    "MAT_APA", "MAT_PI", "MAT_US")


corr_plot<-list()
for (i in seq_along(PA_list)) {
  df_vif<-PA_list[[i]] %>% 
    dplyr::select(area:treat) %>% 
    mutate(treat=as.numeric(as.character(treat)))
  
  corr_mtx <- cor(df_vif)
  corr_plot[[i]]<-corrplot(corr_mtx, method = "circle",
                           type = "upper", 
                           tl.col = "black", tl.srt = 45, 
                           addCoef.col = "black",
                           title = paste0("Corr - ", 
                                          paste(unique(PA_list[[i]]$biome), 
                                                unique(PA_list[[i]]$pa_type),
                                                sep = "_")),
                           mar = c(0,0,2,0))
}
#just Caatinga - APA --> remove area (>0.7 with pop95)

fml_base<-"treat ~ area + dist_energy + Pop95 + dist95_agr + dist95_urb"
fml_CAT_APA<-"treat ~ dist_energy + Pop95 + dist95_agr + dist95_urb"

fm_match<-list()
baltab_match<-list()
sd_loveplot<-list()
vr_loveplot<-list()

for (g in seq_along(PA_list)) {
  pa_g<-PA_list[[g]]
  nm_PA_list<-names(PA_list[g])
  if(g==4){form=as.formula(fml_CAT_APA)
  mv_form<-"area + dist_energy + Pop95 + dist95_agr + dist95_urb"
  }
  else{form=as.formula(fml_base) 
  mv_form<-"dist_energy + Pop95 + dist95_agr + dist95_urb"}
  #FM with glm + mehalonobis 
  fm_model <- matchit(form,
                      data = pa_g, 
                      method = "full", 
                      distance = "glm",
                      link = "logit",
                      estimand = "ATT",
                      verbose = TRUE,
                      include.obj = FALSE, 
                      discard = "none", 
                      #caliper = 0.25, 
                      mahvars = as.formula(paste("~", mv_form))
                      )
  fm_match[[nm_PA_list]] <- fm_model
  # Balance
  b_tab<-bal.tab(fm_model, thresholds = c(m = .25), 
                     v.threshold = 2, un = TRUE)
  baltab_match[[nm_PA_list]] <- b_tab
  # LovePlot - sd
  sd_loveplot[[nm_PA_list]] <- love.plot(fm_model,
                      estimand = "ATT",
                      stat = "mean.diffs",
                      thresholds = c(m = 0.1),
                      shapes = c("circle filled",
                                 "circle filled"),
                      colors = c("red", "blue")) +
    geom_vline(xintercept = c(-0.25, -0.5, 0.25, 0.5), 
               linetype = "solid", 
               color = c("green", "orange",
                         "green", "orange")) +
    ggtitle(paste("Love Plot (sd) -", nm_PA_list))
  # LovePlot - variance
  vr_loveplot[[nm_PA_list]] <- love.plot(fm_model,
                           stat =  "variance.ratios", 
                           thresholds = c(v = 2),
                           shapes = c("circle filled",
                                      "circle filled"),
                           colors = c("red", "blue")) + 
    ggtitle(paste("Love Plot (vr) -", nm_PA_list))

}

sd_loveplot













        