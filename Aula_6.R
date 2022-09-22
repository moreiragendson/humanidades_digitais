## ==================================================
##
##          Aula 6 - Atividade Avaliativa I
##
## ==================================================

## Na nossa primeira atividade avaliativa, vamos lidar com 
## dados dos candidatos na ultima eleicao de MG. 
## Segue abaixo as tarefas:

## 1) Instalar e abrir pacotes indicados nas aulas 1 a 3. 

library(tidyverse)
library(rio)
library(janitor)

## 2) Abrir base "votacao_mg_2018_edit.csv"
vot_mg <- import("votacao_mg_2018_edit.csv")

## 3) Descobrir nomes das variaveis
names(vot_mg)

## 4) Descobrir tipos das variaveis
glimpse(vot_mg)

## 5) Selecionar as variaveis "SG_PARTIDO", "QT_VOTOS_NOMINAIS",
## "DS_CARGO", "NM_MUNICIPIO", "CD_MUNICIPIO", "DS_SIT_TOT_TURNO".
vars <- c("SG_PARTIDO", "QT_VOTOS_NOMINAIS", "DS_CARGO",
          "NM_MUNICIPIO", "CD_MUNICIPIO", "DS_SIT_TOT_TURNO",
          "NM_URNA_CANDIDATO")

vot_mod <- vot_mg %>% 
  select(all_of(vars))

## 6) Renomear as variaveis como quiser.
vot_mod <- vot_mod %>% 
  rename(partido= SG_PARTIDO, 
         votos_nom=QT_VOTOS_NOMINAIS,
         cargo=DS_CARGO, 
         municipio=NM_MUNICIPIO,
         cod_mu=CD_MUNICIPIO,
         situacao =DS_SIT_TOT_TURNO,
         candidato=NM_URNA_CANDIDATO)

## 7) Descobrir os 5 distritos mais populosos de MG no Google,

# Belo Horizonte, Uberlândia, Contagem, Juiz de Fora e Betim.

## 8) Filtrar somente deputados federais, 
vot_df <- vot_mod %>% 
  filter(cargo=="Deputado Federal")

## 9) Descobrir quais os candidatas(os) a deputada(o) federal
## mais votadas(os) nesses distritos.
## Dica: combinem a funcao principal "mutate" com funcao auxiliar "sum".
vot_df2 <- vot_df %>% 
  filter(municipio%in% c("BELO HORIZONTE","UBERL\xc2NDIA",
                         "CONTAGEM","JUIZ DE FORA","BETIM"))

vot_df2 %>% 
  group_by(municipio, candidato) %>% 
  summarise(votos = sum(votos_nom, na.rm = TRUE)) %>% 
  slice(which.max(votos)) %>% 
  arrange(desc(votos))
  

## 10) Descobrir quem foram os candidatos mais votados que nao
## foram eleitos. 

vot_df %>% 
  filter(situacao=="N\xc3O ELEITO") %>%
  group_by(candidato) %>% 
  summarise(votos = sum(votos_nom, na.rm = TRUE)) %>% 
  arrange(desc(votos)) %>% 
  head(10)
  
## 11) Agrupar os partidos por esquerda/centro/direita e ver
## percentualmente quem recebeu mais votos
## nos 5 distritos mais populosos e em MG em geral.

esquerda <- c("PT",  "PCB","PSOL", "PC do B", "PSTU", "PSB", "PDT", "REDE")
centro <- c("PPS","DEM","PPL","MDB","PSDB","PMN",
            "AVANTE","PV","PODE","PHS", "PROS","PSD",
            "SOLIDARIEDADE")


vot_df <- vot_df %>% 
  mutate(ideologia = case_when(partido %in% esquerda ~ "esquerda",
                               partido %in% centro~ "centro",
                               TRUE~ "direita"))

# Nos 5 distritos mais populosos
vot_df %>% 
  filter(municipio%in% c("BELO HORIZONTE","UBERL\xc2NDIA",
                         "CONTAGEM","JUIZ DE FORA","BETIM")) %>% 
  group_by(municipio, ideologia) %>% 
  summarise(votos = sum(votos_nom, na.rm = TRUE)) %>% 
  mutate(percentage = round(votos/sum(votos),2)) %>% 
  arrange(.by_group=TRUE)

# MG em geral
vot_df %>% 
  group_by(ideologia) %>% 
  summarise(votos = sum(votos_nom, na.rm = TRUE)) %>% 
  mutate(percentage = round(votos/sum(votos),2)) %>% 
  arrange(desc(percentage))

# A esquerda teve um resultado ligeiramente superior em MG
# se comparado com os 5 distritos mais populosos de MG.