# ----------------------------------------------------------------
# Name:
#
# Author: Curtis Murray
# Date:	 25 Aug 2020
#
# Description:
# 
# ----------------------------------------------------------------
# Libs
library(tidyverse)
library(ggraph)
library(igraph)
library(visNetwork)
library(tidytext)
library(RColorBrewer)
library(tidygraph)
library(networkD3)
library(visNetwork)
library(htmlwidgets)
library(ape)
# ----------------------------------------------------------------
# Loading Data

# ----------------------------------------------------------------
# Functions

# ----------------------------------------------------------------

clean_posts <- read_csv("data/hSBM_pain/train.csv")

hSBM_input <- clean_posts

word_probs <- hSBM_input %>% 
  unnest_tokens(word, content) %>% 
  group_by(word) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count))

keep_words <- hSBM_input %>% unnest_tokens(word, content) %>% 
  group_by(word) %>% 
  summarise(count = n()) %>% 
  #top_n(2000, count) %>% 
  arrange(-count) %>% 
  pull(word)

tidy_topics_all <- read_csv("data/hSBM_pain/Clean/tidy_topics.csv") %>% 
  group_by(Level) %>% 
  nest() %>% 
  ungroup() %>% 
  transmute(level = Level, posts = data)

max_level <- tidy_topics_all$level %>% as.numeric %>% max

tidy_topic_docs_all <- read_csv("data/hSBM_pain/Clean/tidy_topics_docs.csv") %>% 
  group_by(Level) %>% 
  nest() %>% 
  ungroup() %>% 
  transmute(level = Level, posts = data)

topic_probs <- tidy_topic_docs_all %>% 
  unnest(posts) %>% 
  group_by(topic, level) %>% 
  summarise(p = mean(p)) %>% 
  mutate(topic = paste("L",level, "_", topic, sep = "")) %>% 
  select(-level)

words_wide <- tidy_topics_all %>% 
  unnest(posts) %>%
  filter(word %in% keep_words) %>% 
  pivot_wider(names_from = level, values_from = topic, names_prefix = "L") %>% 
  group_by(word, word_ID) %>% 
  summarise_at(c("p", paste("L", 0:max_level, sep = "")), ~max(.x, na.rm = t)) %>% 
  ungroup()

for(i in 0:max_level){
  col <- words_wide[,4]
  col_name <- names(col)
  
  words_wide <- words_wide[,-4] %>% bind_cols(col %>% mutate_at(1, ~paste(col_name, .x, sep ="_")))
}

for(i in (max_level+1):0){
  print(i)
  if(i == max_level+1){
    edges <- words_wide %>%
      group_by_at(i-1+4) %>%
      summarise() %>%
      ungroup() %>% 
      mutate(from = paste("L",i+1,"_", sep = "")) %>%
      rename(to = 1)
  }else if(i == 0){
    tmp <- words_wide %>% 
      group_by_at(c(1, 4)) %>% 
      summarise() %>% 
      ungroup() %>% 
      rename(from = 2, to = 1) 
    edges <- edges %>% bind_rows(tmp)
  }else{
    tmp <- words_wide %>%
      group_by_at(c(i-1+4, i+4)) %>%
      summarise() %>%
      ungroup() %>% 
      rename(from = 2, to = 1) 
    edges <- edges %>% bind_rows(tmp)
    
  }
}

nodes <- tibble(
  label = unique(c(edges$to, edges$from))
) %>% 
  mutate(id = 1:n()) %>% 
  select(id, label) %>% 
  mutate(level = as.numeric(str_extract(str_extract(label,"L\\d_"), "\\d"))) %>% 
  mutate(level = ifelse(is.na(level), -1, level)) %>%
  left_join(tibble(level = (-1):2, color = brewer.pal(n = 4, name = "Dark2"))) %>% 
  left_join(words_wide %>% select(word, p), by = c("label" = "word")) %>% 
  mutate(p = ifelse(is.na(p), 1, p)) %>% 
  rename(size = p) %>% 
  left_join(word_probs, by = c("label" = "word"))

edges_id <- edges %>% 
  inner_join(nodes %>% select(-level), by = c("from" = "label")) %>% 
  inner_join(nodes %>% select(-level), by = c("to" = "label")) %>% 
  select(from = id.x, to = id.y)

nodes_named <- nodes %>% 
  left_join(topic_probs, by = c("label" = "topic")) %>% 
  select(id, label, level, color, size, p, prop) %>% 
  mutate(size = ifelse(str_detect(label, "L\\d_"), p, prop)) %>% 
  select(-p) %>% 
  mutate(label = ifelse(str_detect(label, "L\\d_"), "", label)) %>% 
  mutate(font.size = ifelse(label == "", 0, size^(1/.7))) %>% 
  mutate(font.size = font.size/max(font.size)*1000) %>% 
  mutate(size = ifelse(label == "", 0, size)) %>% 
  mutate(node_key = id) %>% 
  mutate(size = size*10000)

edges_id_full <- edges_id %>% 
  inner_join(nodes_named %>% select(id, label), by = c("to" = "id")) %>% 
  mutate(edge.width = ifelse(label == "", 0, 1),
         edge.length = 1)

root <- edges_id_full$from[1]
# 
# visNetwork(nodes = nodes_named, edges = edges_id, height = "2000px", width = "100%") %>%
#   #visNodes(color = "white") %>%
#   visEdges(hidden = TRUE)


my_graph <- graph_from_data_frame(edges_id_full %>%
                                    #filter(from != root) %>%
                                    select(-label),
                                  directed = TRUE,
                                  vertices = nodes_named %>%
                                    #filter(id != root) %>%
                                    mutate(font.size = 5*font.size/str_length(label)) %>%
                                    mutate(font.size = ifelse(font.size < 1, 1,font.size)) %>% 
                                    mutate(size = size) %>%
                                    mutate(font.alpha = 1) %>%
                                    mutate(shape = "circle"))

save(list = "my_graph", file = "data/hSBM_pain/Clean/graph.rda")

saveWidget(visIgraph(my_graph,
                     layout="layout_as_tree",
                     circular = T,
                     #root=edges_id_full$from[2],
                     idToLabel = F) %>%
             visOptions(highlightNearest = list(enabled = T, hover = T),
                        nodesIdSelection = T,height = 1440, width = 2560),
           file = paste("Figures/topic_network_alt_pain.html",sep=""))
