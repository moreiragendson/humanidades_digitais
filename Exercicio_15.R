
## Alunos: Gendson Moreira e Maria Eduarda

## Pacotes necessarios.

library(rio)
library(janitor)
library(tidyverse)
library(abjutils)
library(tm)
library(tidytext)
library(ptstem)

## 1) Selecionem os mandatos presidenciais de Lula e Bolsonaro

discursos <- import("PresidentialSpeeches.xlsx")

discurso_lula <- discursos %>% 
  filter(PT==1, date %in% c(2003,2004,2005,2006,2007,2008,2009,2010)) %>%
  as_tibble() %>% 
  select(text)

discurso_bozo <- discursos %>% filter(PSL==1) %>% select(text)

## 2) Arrumem as palavras da melhor maneira 
## (removendo stopwords, tirando radical, etc)

discurso_bozo <- discurso_bozo %>% 
  mutate(text = str_to_lower(text)) %>% 
  mutate(text = rm_accent(text)) %>% 
  mutate(text = removeNumbers(text)) %>%
  mutate(text = removePunctuation(text))

palavras_discurso_bozo <- discurso_bozo %>%
  unnest_tokens(word, text) %>% 
  filter(!(word %in% stopwords(kind = "portuguese")))


discurso_lula <- discurso_lula %>% 
  mutate(text = str_to_lower(text)) %>% 
  mutate(text = rm_accent(text)) %>% 
  mutate(text = removeNumbers(text)) %>%
  mutate(text = removePunctuation(text))

palavras_discurso_lula <- discurso_lula %>%
  unnest_tokens(word, text) %>% 
  filter(!(word %in% stopwords(kind = "portuguese")))

ncol(palavras_discurso_lula)
### NAO FOI POSSIVEL RODAR PELA FALTA DO PACOTE ##
palavras_semradicais_bozo <- ptstem(palavras_discurso_bozo, 
                               algorithm = "rslp", complete = FALSE)

palavras_semradicais_lula <- ptstem(palavras_discurso_lula, 
                                    algorithm = "rslp", complete = FALSE)
### NAO FOI POSSIVEL RODAR PELA FALTA DO PACOTE ##

## 3) Se o pacote para stopwords não for o melhor, 
## espero que avancem um pouco mais 
## e deletem palavras inadequadas (ex: sr, sra, porque) manualmente. 

mod_palavras_discurso_bozo <- palavras_discurso_bozo %>% 
  filter(!(str_detect(word, "nao"))) %>% 
  filter(!(str_detect(word, "presidente"))) %>%
  filter(!(str_detect(word, "todos"))) %>% 
  filter(!(str_detect(word, "aqui"))) %>%
  filter(!(str_detect(word, "cada"))) %>% 
  filter(!(str_detect(word, "dia"))) %>% 
  filter(!(str_detect(word, "porque"))) %>% 
  filter(!(str_detect(word, "voces"))) %>% 
  filter(!(str_detect(word,"la"))) %>% 
  filter(!(str_detect(word,"sao"))) %>% 
  filter(!(str_detect(word,"ser"))) %>% 
  filter(!(str_detect(word,"tambem"))) %>% 
  filter(!(str_detect(word,"entao"))) %>% 
  filter(!(str_detect(word,"bem"))) %>% 
  filter(!(str_detect(word,"ate"))) %>% 
  filter(!(str_detect(word,"agora"))) %>% 
  filter(!(str_detect(word,"senhores"))) %>% 
  filter(!(str_detect(word,"grande"))) %>% 
  filter(!(str_detect(word,"obrigado"))) %>% 
  filter(!(str_detect(word,"fazer"))) %>% 
  filter(!(str_detect(word,"momento")))  %>% 
  filter(!(str_detect(word,"tudo"))) %>% 
  filter(!(str_detect(word,"gente"))) %>% 
  filter(!(str_detect(word,"ja"))) %>% 
  filter(!(str_detect(word,"vai"))) %>% 
  filter(!(str_detect(word,"anos"))) %>% 
  filter(!(str_detect(word,"ha"))) %>% 
  filter(!(str_detect(word,"prezado"))) %>% 
  filter(!(str_detect(word,"ter"))) %>% 
  filter(!(str_detect(word,"parte"))) %>% 
  filter(!(str_detect(word,"ministro"))) %>% 
  filter(!(str_detect(word,"ai"))) %>% 
  filter(!(str_detect(word,"vez"))) %>% 
  filter(!(str_detect(word,"so"))) %>% 
  filter(!(str_detect(word,"hoje"))) %>% 
  filter(!(str_detect(word,"questao"))) %>% 
  filter(!(str_detect(word,"senhor"))) %>% 
  filter(!(str_detect(word,"certeza"))) %>% 
  filter(!(str_detect(word,"pouco"))) %>% 
  filter(!(str_detect(word,"todo"))) %>% 
  filter(!(str_detect(word,"voce"))) %>% 
  filter(!(str_detect(word,"pode"))) %>% 
  filter(!(str_detect(word,"onde"))) %>% 
  filter(!(str_detect(word,"sempre"))) %>% 
  filter(!(str_detect(word,"paulo"))) %>% 
  filter(!(str_detect(word,"assim"))) %>% 
  filter(!(str_detect(word,"melhor"))) %>% 
  filter(!(str_detect(word,"estado"))) %>% 
  filter(!(str_detect(word,"frente"))) %>% 
  filter(!(str_detect(word,"coisa"))) %>% 
  filter(!(str_detect(word,"tempo"))) %>% 
  filter(!(str_detect(word,"realmente"))) %>% 
  filter(!(str_detect(word,"vamos"))) %>% 
  filter(!(str_detect(word,"nesse"))) %>% 
  filter(!(str_detect(word,"outros"))) %>% 
  filter(!(str_detect(word,"queremos"))) %>% 
  filter(!(str_detect(word,"estar"))) %>% 
  filter(!(str_detect(word,"quero")))%>% 
  filter(!(str_detect(word,"estao")))%>% 
  filter(!(str_detect(word,"ano")))%>% 
  filter(!(str_detect(word,"sim")))%>% 
  filter(!(str_detect(word,"acima")))%>% 
  filter(!(str_detect(word,"primeiro")))%>% 
  filter(!(str_detect(word,"alguns")))%>% 
  filter(!(str_detect(word,"bom")))%>% 
  filter(!(str_detect(word,"obviamente")))%>% 
  filter(!(str_detect(word,"toda")))%>% 
  filter(!(str_detect(word,"certo")))%>% 
  filter(!(str_detect(word,"dessa")))


mod_palavras_discurso_lula <- palavras_discurso_lula %>% 
  filter(!(str_detect(word, "nao"))) %>%  
  filter(!(str_detect(word,"gente")))%>%
  filter(!(str_detect(word, "todos"))) %>%
  filter(!(str_detect(word, "site"))) %>%
  filter(!(str_detect(word, "visite"))) %>% 
  filter(!(str_detect(word, "aqui"))) %>%
  filter(!(str_detect(word, "cada"))) %>% 
  filter(!(str_detect(word, "dia"))) %>% 
  filter(!(str_detect(word, "porque"))) %>% 
  filter(!(str_detect(word, "voces"))) %>% 
  filter(!(str_detect(word,"la"))) %>% 
  filter(!(str_detect(word,"sao"))) %>% 
  filter(!(str_detect(word,"ser"))) %>% 
  filter(!(str_detect(word,"tambem"))) %>% 
  filter(!(str_detect(word,"entao"))) %>%
  filter(!(str_detect(word,"presidente"))) %>% 
  filter(!(str_detect(word,"secretaria"))) %>% 
  filter(!(str_detect(word,"dizer"))) %>% 
  filter(!(str_detect(word,"agora"))) %>% 
  filter(!(str_detect(word,"senhores"))) %>% 
  filter(!(str_detect(word,"apenas"))) %>% 
  filter(!(str_detect(word,"fazer")))%>% 
  filter(!(str_detect(word,"importante"))) %>% 
  filter(!(str_detect(word,"companheiro")))  %>% 
  filter(!(str_detect(word,"companheiros"))) %>% 
  filter(!(str_detect(word,"acho"))) %>% 
  filter(!(str_detect(word,"neste"))) %>% 
  filter(!(str_detect(word,"vai"))) %>% 
  filter(!(str_detect(word,"anos"))) %>% 
  filter(!(str_detect(word,"ha"))) %>%   
  filter(!(str_detect(word,"ter"))) %>% 
  filter(!(str_detect(word,"voce"))) %>% 
  filter(!(str_detect(word,"quero")))%>% 
  filter(!(str_detect(word,"vamos")))%>% 
  filter(!(str_detect(word,"ja")))%>% 
  filter(!(str_detect(word,"coisa")))%>% 
  filter(!(str_detect(word,"hoje")))%>% 
  filter(!(str_detect(word,"pode")))%>% 
  filter(!(str_detect(word,"estado")))%>% 
  filter(!(str_detect(word,"estao")))%>% 
  filter(!(str_detect(word,"so")))%>% 
  filter(!(str_detect(word,"muitas")))%>% 
  filter(!(str_detect(word,"ate")))%>%
  filter(!(str_detect(word,"paises")))%>%
  filter(!(str_detect(word,"ano")))%>%
  filter(!(str_detect(word,"vezes")))%>%
  filter(!(str_detect(word,"tempo")))%>%
  filter(!(str_detect(word,"todo")))%>%
  filter(!(str_detect(word,"vez")))%>%
  filter(!(str_detect(word,"ainda")))%>%
  filter(!(str_detect(word,"grande")))%>%
  filter(!(str_detect(word,"vou")))%>%
  filter(!(str_detect(word,"ministro")))%>%
  filter(!(str_detect(word,"bem")))%>%
  filter(!(str_detect(word,"sabe")))%>%
  filter(!(str_detect(word,"pais")))%>%
  filter(!(str_detect(word,"primeiro")))%>%
  filter(!(str_detect(word,"mil")))%>%
  filter(!(str_detect(word,"milhoes")))%>%
  filter(!(str_detect(word,"brasileiros")))%>%
  filter(!(str_detect(word,"sei")))%>%
  filter(!(str_detect(word,"ai")))%>%
  filter(!(str_detect(word,"tudo")))%>%
  filter(!(str_detect(word,"rio")))%>%
  filter(!(str_detect(word,"caro")))%>%
  filter(!(str_detect(word,"paulo")))%>%
  filter(!(str_detect(word,"pouco")))%>%
  filter(!(str_detect(word,"vao")))%>%
  filter(!(str_detect(word,"deste")))%>%
  filter(!(str_detect(word,"queria")))%>%
  filter(!(str_detect(word,"fazendo")))%>%
  filter(!(str_detect(word,"queremos")))%>%
  filter(!(str_detect(word,"momento")))%>%
  filter(!(str_detect(word,"parte")))

## 4) Elaborem gráficos comparativos para os dois casos 
## (com as 15 palavras mais utilizadas). 

mod_palavras_discurso_bozo <- mod_palavras_discurso_bozo %>% 
  mutate(presidente= "Bolsonaro") 


mod_palavras_discurso_lula <- mod_palavras_discurso_lula %>% 
  mutate(presidente= "Lula") 


palavras_bozo_lula <- 
  bind_rows(mod_palavras_discurso_lula, mod_palavras_discurso_bozo) %>% 
  count(word,presidente) 

palavras_bozo_lula <- palavras_bozo_lula %>% 
  mutate(n_total= case_when(presidente=="Lula"~ 1487339, 
                            presidente=="Bolsonaro"~76036))

mod_palavras_discurso_bozo %>% 
  count(word) %>% 
  arrange(-n) %>% 
  slice_head(n = 15)

n_bozo <-  palavras_bozo_lula %>% 
  filter(word %in% c("brasil","deus","governo","povo","mundo","vida",
                     "trabalho","brasileiro","militar","liberdade","verdade",
                     "exercito","federal","economia","passado"))%>% 
  mutate(prop = n/n_total)

n_bozo %>% 
  ggplot(aes(reorder(word,prop), prop, fill = presidente)) +
  geom_col(position = "dodge")+
  labs(x = "", y = "Proporção", fill= "Presidente",
       title = "Palavras mais faladas pelo Bolsonaro")+
  coord_flip()+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("#5F9344", "#F40101"))


  
mod_palavras_discurso_lula %>% 
  count(word) %>% 
  arrange(-n) %>% 
  slice_head(n = 15) 

n_lula <-  palavras_bozo_lula %>% 
  filter(word %in% c("brasil","mundo","governo","republica","povo",
                     "politica", "dinheiro","brasileiro","desenvolvimento",
                     "dar","brasileira","vida","casa","nacional",
                     "programa")) %>% 
  mutate(prop = n/n_total)

n_lula %>% 
  ggplot(aes(reorder(word,prop), prop, fill = presidente)) +
  geom_col(position = "dodge")+
  labs(x = "", y = "Proporção", fill= "Presidente",
       title = "Palavras mais faladas pelo Lula")+
  coord_flip()+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("#5F9344", "#F40101"))

