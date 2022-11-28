## Pacotes necessarios para a atividade

library(sf)
library(geobr)
library(gtools)
library(patchwork)
library(ggalt)
library(gridExtra)
library(rio)
library(janitor)
library(tidyverse)

## 1) Baixar dados de votação de MG em 2022 
## (https://dadosabertos.tse.jus.br/dataset/resultados-2022). 
## Peguem votação por partido.

## OBS: Será feita a junção das bases necessarias para as atividades

vot_pp <- import("votacao_partido_munzona_2022_MG.csv")
names (vot_pp)
vot_pp <- vot_pp %>% select (SG_PARTIDO,
                             DS_CARGO,
                             QT_VOTOS_NOMINAIS_VALIDOS, 
                             codigo_tse = CD_MUNICIPIO)

cod_map <- import("mapa_mg.RDS")
names(cod_map)
cod_map <- cod_map %>% select(codigo_ibge = cod_ibge, geom)

base_codigos <- import("municipios_brasileiros_tse.csv")
glimpse (cod_map)
glimpse (base_codigos)

base_codigos <- base_codigos %>% 
  filter(uf=="MG") %>% 
  mutate(codigo_ibge = as.character(codigo_ibge))

base_codigos <- base_codigos %>% 
  left_join(cod_map, by= "codigo_ibge")

names(base_codigos)

vot_pp <- base_codigos %>% 
  left_join(vot_pp, by = "codigo_tse")

## 2) Descobrir os 10 municípios em que PP foi mais 
## votado para deputado federal. E os 10 em que foi menos votado.

vot_pp <- vot_pp %>% 
  filter (SG_PARTIDO=="PP", DS_CARGO=="Deputado Federal")

names(vot_pp)
maior_vot <- vot_pp %>%
  group_by(nome_municipio) %>% 
  summarise (votos=sum(QT_VOTOS_NOMINAIS_VALIDOS)) %>% 
  arrange(-votos)

menor_vot <- vot_pp %>%  group_by(nome_municipio) %>% 
  summarise (votos=sum(QT_VOTOS_NOMINAIS_VALIDOS)) %>% 
  arrange(+votos)

## 3) Fazer representação gráfica mais adequada da informação 
## contida no item 2. Disponha os dois gráficos lado a lado 
## e exporte em pdf.

fig_1 <- maior_vot %>%
  filter(nome_municipio %in% c("UBERLÂNDIA","UBERABA","BELO HORIZONTE",
                             "IBIRITÉ","JUIZ DE FORA",
                             "RIBEIRÃO DAS NEVES", "CAMPO BELO",
                             "ITURAMA","VARGINHA","CARATINGA"))%>% 
  ggplot(aes(x= reorder(nome_municipio, +votos), y = votos)) +
  geom_col(fill = "dodgerblue") + theme_bw() + 
  geom_text(aes(label = votos), hjust=-.01)+ 
  labs(x = "", y = "Votos", title= "Municípios com mais votos para dep. federal (PP)") +  coord_flip()


fig_2 <- menor_vot %>%
  filter(nome_municipio %in% c("DORESÓPOLIS","PASSABÉM","PESCADOR",
                             "SÃO JOÃO DA MATA","GLAUCILÂNDIA",
                             "ARAÚJOS", "ALBERTINA","BIQUINHAS",
                             "PEDRA DOURADA","LEANDRO FERREIRA"))%>% 
  ggplot(aes(x= reorder(nome_municipio, -votos), y = votos)) + 
  geom_col(fill = "dodgerblue") + theme_bw() + 
  geom_text(aes(label = votos), hjust=-.01)+
  labs(x = "", y = "Votos", title= "Municípios com menos votos para dep. federal (PP)") +  coord_flip()

maior_e_menor_vot <- grid.arrange(fig_1, fig_2)
pdf("maior_e_menor_vot.pdf")

## 4) Fazer mapa de votação dos deputados federais do PP no 
##  primeiro turno,segundo a porcentagem de votos dados ao partido em 
## cada município (exportar em png).

style_sf <- function(legenda = FALSE){
  
  legend_position <- ifelse(  legenda  == TRUE, "bottom" , "none")
  
  theme(plot.title = element_text(family = "serif", face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(family = "serif", size = 8, hjust = 0.5),
        plot.caption = element_text(face = "bold", size = 10),
        plot.caption.position = "plot", # caption se alinha ao plot
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = legend_position,
        legend.title = element_text(face = "bold"))
  
}

vot_pp <- vot_pp %>% 
  mutate(prop = QT_VOTOS_NOMINAIS_VALIDOS/
           sum(QT_VOTOS_NOMINAIS_VALIDOS)*100) 
vot_pp<- vot_pp %>% 
  mutate(prop = quantcut(prop,5))

map_pp <- ggplot() + 
  geom_sf(data = vot_pp, color = "white",
          size = 0.1, alpha = 1,  
          aes(geometry = geom, fill = prop)) +
  style_sf(T)+   labs(fill = "Votos (%)")+
  scale_fill_brewer(palette = "Blues", direction = 1)

png("map_pp.png")
