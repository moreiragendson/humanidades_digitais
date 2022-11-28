## =============================================
##
##      HUMANIDADES DIGITAIS - MAPAS
##
## =============================================


library(rio) # importar base
library(tidyverse) # organizar dados
# library(dplyr) pacote alternativo ao tidyverse
library(janitor) # tabular


votacao_mg <- import("mapa_mg.RDS")

## ============================================
##        TRUQUES COM MAPAS 
## ============================================

##----------------------------------
## mapa 1: mapa mais basico
##----------------------------------

ggplot() + 
  geom_sf(data = votacao_mg, 
          aes(fill = prop)) 

##----------------------------------
## mapa 2: com zonas de calor na ascendente
##----------------------------------

ggplot() + 
  geom_sf(data = votacao_mg, 
          aes(fill = prop)) +
  scale_fill_distiller(direction = 1)

ggplot() + 
  geom_sf(data = votacao_mg, color = "black",
          size = 0.2, alpha = 1,  
          aes(fill = prop)) +
  scale_fill_distiller(direction = 1) # zonas de calor com cores mais quentes

## color = cor das fronteiras
## size = tamanho das linhas de fronteira
## alpha = nivel de transparencia

##----------------------------------
## mapa 3: mudanca de cores e temas
##----------------------------------

mcont <- ggplot() + 
  geom_sf(data = votacao_mg, color = "black",
          size = 0.2, alpha = 1,  
          aes(fill = prop)) +
  theme_light() + # aqui!
  scale_fill_distiller(palette = "Reds", # e aqui!
                       direction = 1) 

mcont 

##----------------------------------
## mapa 4: sem gueri-gueris
##----------------------------------

## com legenda

ggplot() + 
  geom_sf(data = votacao_mg, color = "black",
          size = 0.2, alpha = 1,  
          aes(fill = prop)) +
  theme_light() +
  scale_fill_distiller(palette = "Reds",
                       direction = 1) +
  style

style <- theme(axis.text = element_blank(),
                     strip.background = element_rect(fill="black"),
                     strip.text = element_text(size=12),
                     panel.grid.major = element_line(colour = "white"),
                     axis.ticks = element_blank(),
                     legend.position="none",  # diferenca aqui!
                     plot.title = element_text(hjust = 0.5),
                     plot.margin = unit(c(0, 0, 0, 0), "cm")) 

## sem legenda

ggplot() + 
  geom_sf(data = votacao_mg, color = "black",
          size = 0.2, alpha = 1,  
          aes(fill = prop)) +
  theme_light() +
  scale_fill_distiller(palette = "Greens",
                       direction = 1) +
  theme(axis.text = element_blank(),
        strip.background = element_rect(fill="black"),
        strip.text = element_text(size=12),
        panel.grid.major = element_line(colour = "white"),
        axis.ticks = element_blank(),
        legend.position="none",  # diferenca aqui!
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) 

##----------------------------------
## mapa 5: dica 1 - testar escalas categoricas
##----------------------------------

# install.packages("gtools")
library(gtools)

votacao_mg <- votacao_mg %>% 
  mutate(prop_cat = quantcut(prop, 5))

mcat <- ggplot() + 
  geom_sf(data = votacao_mg, color = "black",
          size = 0.2, alpha = 1,  
          aes(fill = prop_cat)) + # mudanca aqui
  theme_light() +
  scale_fill_brewer(palette = "Reds",
                    direction = 1)

mcat
mcat+style

# install.packages("patchwork")
library(patchwork)

mcont + mcat

##----------------------------------
## mapa 6: dica 2 - testar cores divergentes
##----------------------------------

# com calor na descendente

ggplot() + 
  geom_sf(data = votacao_mg, color = "black",
          size = 0.2, alpha = 1,  
          aes(fill = prop_cat)) +
  theme_light() +
  scale_fill_brewer(palette = "RdYlBu",
                    direction = 1)

# com calor na ascendente

ggplot() + 
  geom_sf(data = votacao_mg, color = "black",
          size = 0.2, alpha = 1,  
          aes(fill = prop_cat)) +
  theme_light() +
  scale_fill_brewer(palette = "RdYlBu",
                    direction = -1)

# saber o nome das variáveis?

names(votacao_brasil)

##----------------------------------
## Dica 3 - mapas interativos
##----------------------------------

# install.packages("terra")
# install.packages("mapview")
library(mapview)
library(terra)

mapview(votacao_mg, zcol = "prop_cat")

mapview(votacao_mg, zcol = "prop_cat", 
        col.regions = RColorBrewer::brewer.pal(9, "Greens"), 
        alpha.regions = 0.7)

mapview(votacao_mg, zcol = "prop_cat", 
        native.crs = TRUE)

mylabel <- glue::glue("{votacao_mg$nome_mun} {votacao_mg$prop_cat}%")

mapview(votacao_mg, zcol = "prop_cat", 
        label = mylabel)

votacao_mg$nome_mun

votacao_mg <- votacao_mg %>% 
  mutate(name_muni2 = iconv(name_muni, from = "latin1"))  

mylabel <- glue::glue("{votacao_mg$name_muni2} {votacao_mg$prop_cat}")

map_voto <- mapview(votacao_mg, zcol = "prop_cat", 
                        label = mylabel)

map_para1 <- mapview(votacao_mg, zcol = "prop_cat")

map_para2 <- mapview(votacao_mg, zcol = "prop")

map_para1 | map_para2
