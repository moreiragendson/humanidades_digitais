## ==================================================
##
##           Aula 5 - Exercício em Sala
##
## ==================================================

## 1) Achar as variaveis ideologia, religiao, casamento
## entre pessoas do mesmo sexo e vb50 no questionario do
## lapop.

# ideologia: L1
# religiao: Q3CN
# casamento gay: D6
# vb50: homens são melhores líderes políticos?

## 2) Abrir base "lapop_2018.dta".
library(janitor)
library(tidyverse)
library(rio)
dt <- rio::import("lapop_2018.dta")
class(dt)
## 3) Descobrir nomes das variaveis
names(dt)
## 4) Descobrir tipos das variaveis.
map(.x = dt, .f= class)
## 5) Selecionar somente as variaveis indicadas.
dt2 <- dt %>% 
  select(l1, q3cn, d6, vb50)
## 6) Renomear para "ideologia", "religiao", "homo", 
## e "mul_pol".
dt2 <- dt2 %>% 
  rename(ideologia = l1,
         religiao = q3cn,
         homo = d6,
         mul_pol = vb50)
## 9) Criar variavel "posicionamento", tricotomizando
## ideologia em esquerda/centro/direita.

dt2 <- dt2 %>% 
  mutate(posicionamento = case_when(ideologia %in% 7:10 ~ "direita",
                                    ideologia %in% 5:6 ~ "centro",
                                    ideologia %in% 1:4 ~ "esquerda"))
## 10) Tricotomizar religiao em evangelicos/catolicos/outros.
dt2 <- dt2 %>% 
  mutate(cristao = case_when(religiao == 1 ~ "católico",
                             religiao %in% c(2, 5) ~ "evangélico",
                             is.na(religiao) ~ as.character(religiao),
                             TRUE ~ "outros"))


## 11) Dicotomizar "mul_pol" em concorda/discorda.
unique(dt2$mul_pol)

dt2 <- dt2 %>% 
  mutate(lideranca = case_when(mul_pol %in% 1:2 ~ "concorda",
                               mul_pol %in% 3:4 ~ "discorda"))
## 12) Tabular as diferencas dos grupos religiosos em 
## relacao a ideologia (tirar porcentagem).
dt3 <- dt2 %>%  
  tabyl(cristao, posicionamento) %>% 
  adorn_percentages()
dt3
## 13) Tabular as diferencas dos grupos religiosos em 
## relacao a presenca de mulheres na politica
## (tirar porcentagem).
dt4 <- dt2 %>%  
  tabyl(cristao, lideranca) %>% 
  adorn_percentages()




