library(tidyverse)
library(tidytext)

p_w_tw_all_path <- list.files(path = "data/hSBM_pain", pattern=glob2rx("*p_w_tw*.csv"), full.names = T)
p_tw_d_all_path <- list.files(path = "data/hSBM_pain", pattern=glob2rx("*p_tw_d*.csv"), full.names = T)

words_all_all_path <- list.files(path = "data/hSBM_pain", pattern = "words_all*", full.names = T)

# Get the full vocab
# Vocab <- read_csv(words_all_all_path[str_detect(words_all_all_path, "Full")]) %>% 
# 	select(word = `0`)

Vocab <- read_csv("data/hSBM_pain/train.csv") %>% 
  unnest_tokens(word, content) %>% 
  group_by(word) %>%
  summarise(count = n()) %>% 
  mutate(freq = count/sum(count)) %>% 
  arrange(word) %>% 
  mutate(word_ID_full = 1:n())

#write_csv(Vocab, "data/hSBM_pain/Sampling_Problem/Vocab/Vocab.csv")

# Read all p_w_tw files and get adjacency word-topic matrix, join words, construct full word-topic matrix

words_all <- tibble(words_all_all_path = words_all_all_path) %>% 
  mutate(
    words = map(
      words_all_all_path,
      ~read_csv(.x) %>% 
        mutate(word_ID = X1 + 1, word = `0`) %>% 
        select(word_ID, word)
    )) %>% 
  unnest(words) %>% 
  select(-words_all_all_path)

docs_all <- read_csv("data/hSBM_pain/docs_all.csv") %>% 
  transmute(doc_ID = X1 + 1, document = `0`)

tidy_topics <- tibble(p_w_tw_all_path = p_w_tw_all_path) %>% 
  mutate(Level = str_extract(p_w_tw_all_path, "(?<=p_w_tw)\\d{1,}")) %>% 
  map_at("Level", as.double) %>% 
  as_tibble() %>% 
  mutate(
    mat = map(
      p_w_tw_all_path, 
      ~read_csv(.x) %>%
        select(word_ID = X1, everything()) %>%
        mutate(word_ID = word_ID + 1) %>% 
        gather("topic", "p", -word_ID) %>% 
        mutate(topic = as.numeric(topic) + 1) %>% 
        filter(p > 0)
    )) %>% 
  ungroup() %>% 
  arrange(Level) %>% 
  unnest(mat) %>% 
  select(-p_w_tw_all_path) %>% 
  left_join(words_all, by = "word_ID")

write_csv(tidy_topics, "data/hSBM_pain/Clean/tidy_topics.csv")

tidy_topic_docs <- tibble(p_tw_d_all_path = p_tw_d_all_path) %>% 
  mutate(Level = str_extract(p_tw_d_all_path, "\\d")) %>% 
  map_at("Level", as.double) %>% 
  as_tibble() %>% 
  mutate(
    mat = map(
      p_tw_d_all_path, 
      ~read_csv(.x) %>%
        select(topic = X1, everything()) %>%
        mutate(topic = topic + 1) %>% 
        gather("doc_ID", "p", -1) %>% 
        mutate(doc_ID = as.numeric(doc_ID) + 1) %>% 
        full_join(docs_all, by = "doc_ID") %>% 
        select(topic, doc_ID, p, document)
    )
  ) %>% 
  select(-p_tw_d_all_path) %>% 
  unnest(mat)

write_csv(tidy_topic_docs, "data/hSBM_pain/Clean/tidy_topics_docs.csv")
