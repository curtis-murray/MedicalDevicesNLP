library(tidyverse)
library(tidytext)

all_reports_mached_manufacturer_df <- read_csv("data/all_reports/all_reports_mached_manufacturer_df.csv")

train <- all_reports_mached_manufacturer_df %>% 
  group_by(`Event description`, `Report date`) %>% 
  mutate(tmp = 1:n()) %>% 
  ungroup() %>% 
  filter(tmp == 1) %>%  
  ungroup() %>% 
  select(-tmp) %>% 
  unnest_tokens(word, `Event description`) %>% 
  anti_join(stop_words, by = "word") %>% 
  group_by(word) %>% 
  filter(n() > 3) %>% 
  group_by(`Report number`) %>% 
  filter(n() > 3) %>% 
  group_by(across(-word)) %>% 
  summarise(content = paste0(word, collapse = " "))

train

write_csv(train, "data/hSBM/train.csv")

train_pain <-
  all_reports_mached_manufacturer_df %>% 
    filter(str_detect(`Event description`, "(?i)pain")) %>% 
    unnest_tokens(word, `Event description`) %>% 
    anti_join(stop_words, by = "word") %>% 
    group_by(word) %>% 
    filter(n() > 3) %>% 
    group_by(`Report number`) %>% 
    filter(n() > 3) %>% 
    group_by(across(-word)) %>% 
    summarise(content = paste0(word, collapse = " "))

write_csv(train_pain,"data/hSBM_pain/train.csv")
