library(sf)
library(tmap)
library(tidyverse)
env <- new.env()
source('.env', local=env)
options(scipen=999)

country <- 'gaza'
indir <- file.path(env$wd,'in')
col_governorate = c("#001D23", "#457C36", "#83A40E", "#e7d97e", "#ffca79")

pop <- read_csv(file.path(indir, 'baseline_population', 'ocha',  'pse_admpop_adm2_2024.csv')) |> 
  filter(ADM1_EN=='Gaza Strip' & year == 2023)|> 
    mutate(ADM2_EN = factor(ADM2_EN, levels=c('North Gaza','Gaza', 'Deir Al-Balah','Khan Younis', 'Rafah')))

map <- st_read(file.path(indir,'baseline_population', 'cod_ab', 'pse_admbnda_adm2_pamop_20210806.gpkg')) |> 
  filter(ADM1_EN=='Gaza Strip') |> 
  mutate(ADM2_EN = factor(ADM2_EN, levels=c('North Gaza','Gaza', 'Deir Al-Balah','Khan Younis', 'Rafah')))

map_centroid <- st_centroid(map) |> 
  left_join(pop |> select(ADM2_EN, T_TL))

tm_shape(map) +
  tm_polygons('ADM2_EN', palette=col_governorate, legend.show = F)+
  tm_layout(frame = F, legend.outside = T, legend.outside.position = 'bottom', legend.title.size = 2, legend.text.size = 0.7)+
  tm_shape(map_centroid) +
  tm_symbols(size='T_TL', border.lwd = 3 , border.col = 'grey30', alpha = 0, scale=2, perceptual=T,
sizes.legend=c(200000, 500000, 800000), title.size = 'Population before the conflict', legend.format = list(fun = function(x) formatC(x, digits = 0, big.mark = " ", format = "f")))
