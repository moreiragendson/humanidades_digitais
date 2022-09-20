

# libraries ---------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(googlesheets4)
library(janitor)
library(tidytext)
library(wordcloud)
library(ggraph)
library(igraph)

# read data ---------------------------------------------------------------


# spreadsheet_id <- "1vs_G-EpkVq7Ce7wkw-E9H3zL8EBY_dS3Dzw_TfTpFlQ"
# 
# debate <- read_sheet(spreadsheet_id,
#                      sheet = "debate_28_08") %>% 
#   clean_names()


# write data --------------------------------------------------------------


# ifelse(dir.exists("data/")==FALSE, dir.create("data"), "Diretório já existe!")
# 
# ifelse(file.exists("data/debate_band_28_08.csv")==FALSE,
#        write_csv(debate, "data/debate_band_28_08.csv"),
#        "Dados já estão no diretório!")


# load data ---------------------------------------------------------------

debate <- read_csv("data/debate_band_28_08.csv")

# EDA 1 -------------------------------------------------------------------


names(debate)
 
debate %>% head(10)

pare_palavras <- stopwords::data_stopwords_nltk$pt

pare_palavras <- tibble(word = pare_palavras)
pare_palavras %>% head()


# check if `ordem` are subdivisions
length(debate$ordem)==max(debate$ordem)



# transform data ----------------------------------------------------------


debate <- unnest_tokens(
  tbl = debate,
  output = word,
  input = texto
)


debate %>% pull(word) %>% length()


debate <- debate %>% 
  anti_join(pare_palavras)


debate %>% pull(word) %>% length()


# EDA 2 -------------------------------------------------------------------

debate %>% pull(quem) %>% unique()

debate %>% names()

debate %>% 
  filter(quem == "Candidato") %>% 
  group_by(nome) %>% 
  count(word, sort = TRUE)

debate %>% 
  filter(str_length(word)<=2) -> short_words


debate %>% 
  filter(quem == "Candidato") %>% 
  group_by(nome) %>% 
  count(word, sort = TRUE) %>% 
  slice_max(word, n = 10) %>% 
    ggplot()+
      geom_col(aes(word, n))+
      facet_wrap(~nome)+
      coord_flip()


# grafos ------------------------------------------------------------------

debate2 <- read_csv("data/debate_band_28_08.csv")

debate2 %<>%
  unnest_tokens(bigram, texto, token = "ngrams", n = 2)

debate2 %<>%
  separate(bigram, c("word1", "word2"), sep = " ")

pt_stop_words <- stopwords::data_stopwords_nltk$pt

debate2 %<>%
  filter(!word1 %in% pt_stop_words) %>%
  filter(!word2 %in% pt_stop_words) %>% 
  filter(!is.na(word1) & !is.na(word2))

debate_count <- debate2 %>% 
  count(word1, word2, sort = TRUE)

# Grafo

bigram_graph <- debate_count %>%
  filter(n > 5) %>%
  graph_from_data_frame()

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
