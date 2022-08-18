library(tidyverse)
library(tidytext)
library(DT)

codes_desc <- read_csv("data/codes/codes.csv")
tidy_topics <- read_csv("data/hSBM/Clean/tidy_topics.csv")
tidy_topics_docs <- read_csv("data/hSBM/Clean/tidy_topics_docs.csv")

data <- read_csv("data/all_reports/all_reports_mached_manufacturer_df.csv")

data <- data %>% 
  mutate(`GMDN term` = str_to_lower(`GMDN term`)) %>%
  filter(str_detect(`GMDN term`, "mesh")) %>% 
  group_by(`GMDN term`) %>% 
  mutate(hernia = str_detect(`Event description`, "(?i)hernia"),
         pelvic = str_detect(`Event description`, "(?i)((pelvi)|(urina)|(incont)|(vagina)|(uterus)|(anterior)|(posterior))")) %>% 
  filter(hernia | pelvic) %>% 
  filter(!str_detect(`Event description`,"Journal Article")) %>% 
  filter(`Report date` < "2018-01-01") %>% 
  ungroup()

L1 <- codes_desc %>% 
  filter(!is.na(`Level 1 Term`)) %>% 
  mutate(def_L1 = paste(`Level 1 Term`, Definition, sep = " ")) %>% 
  select(L1 = Code, def_L1)

L2 <- codes_desc %>% 
  filter(!is.na(`Level 2 Term`)) %>% 
  mutate(def_L2 = paste(`Level 2 Term`, Definition, sep = " ")) %>% 
  select(L2 = Code, def_L2)

codes_full <- codes_desc %>%
  mutate(L1 = map(CodeHierarchy, ~str_split(.x, "\\|")[[1]][1]) %>% 
           unlist()) %>% 
  mutate(L2 = map(CodeHierarchy, ~str_split(.x, "\\|")[[1]][2]) %>% 
           unlist()) %>% 
  mutate(L3 = map(CodeHierarchy, ~str_split(.x, "\\|")[[1]][3]) %>% 
           unlist()) %>% 
  filter(!is.na(L3)) %>% 
  mutate(def_L3 = paste(`Level 3 Term`, Definition, sep = " ")) %>% 
  select(Code, def_L3,L1,L2) %>% 
  left_join(L1, by = "L1") %>% 
  left_join(L2, by = "L2") %>% 
  mutate(def_full = paste(def_L1,def_L2,def_L3, sep = " \\n \\n ")) %>% 
  select(Code, def_full)

codes_topic <- codes_full %>% 
  unnest_tokens(word, def_full) %>% 
  anti_join(stop_words) %>% 
  left_join(tidy_topics %>% filter(Level == 0)) %>% 
  group_by(topic, Code) %>% 
  mutate(count = n()) %>% 
  group_by(Code) %>% 
  mutate(p_code = count/sum(count)) %>% 
  select(Code, topic, p_code) %>% 
  drop_na()

matches <- tidy_topics_docs %>% filter(Level == 0) %>% 
  #filter(doc_ID < 10) %>% # TODO remove this line
  select(document, topic, p) %>% 
  inner_join(codes_topic, by = "topic") %>% 
  arrange(Code, topic) %>% 
  group_by(document, Code) %>% 
  summarise(similarity = sum(p*p_code)) %>% 
  #left_join(codes_desc %>% select(Code, `Level 3 Term`, `Definition`), by = "Code") %>%
  left_join(codes_full, by = "Code") %>% 
  left_join(data %>% select(document = `Report number`, `Event description`), by = "document") %>% 
  select(document, `Event description`, Code, def_full, similarity) %>% 
  arrange(document, -similarity)

best <- matches %>% 
  group_by(document) %>% 
  filter(similarity == max(similarity)) 

best %>% 
  datatable()



