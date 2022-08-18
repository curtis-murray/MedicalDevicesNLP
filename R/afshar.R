library(tidyverse)
library(kableExtra)
library(knitr)

tidy_topics <- read_csv("data/hSBM/Clean/tidy_topics.csv")
tidy_topics_docs <- read_csv("data/hSBM/Clean/tidy_topics_docs.csv")
docs_all <- read_csv("data/hSBM/docs_all.csv") %>% 
  transmute(doc_ID = X1+1, `Report number` = `0`)
train <- read_csv("data/hSBM/train.csv")
docs_metadata <- docs_all %>% left_join(train, by = "Report number")

df <- read_csv("data/all_reports/all_reports_mached_manufacturer_df.csv") %>% 
  group_by(`Event description`, `Report date`) %>% 
  mutate(tmp = 1:n()) %>% 
  filter(tmp == 1) %>%  
  ungroup() %>% 
  select(-tmp)

topics_whole <- tidy_topics_docs %>% 
  group_by(Level, topic) %>% 
  summarise(p = mean(p)) %>% 
  arrange(-p) %>% 
  group_by(Level) %>% 
  mutate(rank = dense_rank(-p)) %>% 
  left_join(tidy_topics %>% 
              group_by(Level, topic) %>% 
              arrange(-p) %>% 
              select(Level, topic, word, p_w = p), by = c("Level", "topic")) %>% 
  group_by(Level, topic,p,rank) %>% 
  filter(Level == 1) %>%
  slice_max(order_by = -p,n = 10,with_ties = FALSE) %>% 
  summarise(words = paste0(word, collapse = ", ")) %>% 
  arrange(rank) %>% 
  ungroup() 

topics_whole %>%
  filter(rank <= 20) %>% 
  select(`Topic rank` = rank, `Toipc density` = p, `Topic words` = words) %>% 
  kable("latex", booktabs = T, align = "ccl", digits = 3) %>%
  collapse_rows(columns = 1, latex_hline = "major", valign = "top")


input <- c()
input$focus_term = "mesh"





topics_mesh <- docs_metadata %>% 
  mutate(`GMDN term` = str_squish(str_to_lower(`GMDN term`))) %>%
  filter(str_detect(`GMDN term`, input$focus_term) & str_detect(content, "(?i)((pelvi)|(urina)|(incont)|(vagina)|(uterus)|(anterior)|(posterior))")) %>% 
  select(`Report number`) %>% 
  mutate(Term = TRUE) %>% 
  full_join(tidy_topics_docs, by = c(`Report number` = "document")) %>% 
  mutate(Term = ifelse(is.na(Term), "Other_Terms", "Selected_Term")) %>%  
  group_by(Level, topic, Term) %>% 
  summarise(p = mean(p)) %>% 
  pivot_wider(names_from = Term, values_from = p) %>% 
  mutate(ratio = Selected_Term/Other_Terms,
         kld = Selected_Term*log(Selected_Term/Other_Terms)) %>% 
  arrange(-kld) %>% 
  left_join(tidy_topics %>% 
              select(Level, topic, word, p_w = p) %>% 
              group_by(Level, topic) %>% 
              slice_max(order_by = p_w, n = 10,with_ties = FALSE) %>% 
              arrange(-p_w) %>% 
              summarise(words = paste0(word, collapse = ", ")), 
              by = c("Level", "topic")) %>% 
  arrange(-kld) %>% 
  filter(Level == 0) %>% 
  arrange(-Selected_Term) %>% 
  left_join(topics_whole %>% select(Level, topic, rank), by = c("Level","topic"))

topics_mesh %>% 
  ungroup() %>% 
  arrange(-Selected_Term) %>% 
  mutate(`Topic rank` = 1:n()) %>% 
  arrange(-ratio) %>% 
  mutate(ratio_rank = 1:n()) %>% 
  filter(ratio_rank <= 10 | `Topic rank` <= 10) %>% 
  select(`Topic rank`, `Topic ratio` = ratio, `Topic density` = Selected_Term, `Topic words` = words) %>% 
  #DT::datatable()
  kable("latex", booktabs = T, align = "cccl", digits = 3) %>%
  collapse_rows(columns = 1, latex_hline = "major", valign = "top")






topics_mesh <- docs_metadata %>% 
  mutate(`GMDN term` = str_squish(str_to_lower(`GMDN term`))) %>%
  filter(str_detect(`GMDN term`, input$focus_term) & str_detect(content, "(?i)((pelvi)|(urina)|(incont)|(vagina)|(uterus)|(anterior)|(posterior))")) %>% 
  select(`Report number`) %>% 
  mutate(Term = TRUE) %>% 
  full_join(tidy_topics_docs, by = c(`Report number` = "document")) %>% 
  mutate(Term = ifelse(is.na(Term), "Other_Terms", "Selected_Term")) %>%  
  group_by(Level, topic, Term) %>% 
  summarise(p = mean(p)) %>% 
  pivot_wider(names_from = Term, values_from = p) %>% 
  mutate(ratio = Selected_Term/Other_Terms,
         kld = Selected_Term*log(Selected_Term/Other_Terms)) %>% 
  arrange(-kld) %>% 
  left_join(tidy_topics %>% 
              select(Level, topic, word, p_w = p) %>% 
              group_by(Level, topic) %>% 
              slice_max(order_by = p_w, n = 10,with_ties = FALSE) %>% 
              arrange(-p_w) %>% 
              summarise(words = paste0(word, collapse = ", ")), 
            by = c("Level", "topic")) %>% 
  arrange(-kld) %>% 
  filter(Level == 0) %>% 
  arrange(-Selected_Term) %>% 
  left_join(topics_whole %>% select(Level, topic, rank), by = c("Level","topic"))


input <- c()
input$focus_term = "mesh"

res <- docs_metadata %>% 
  mutate(`GMDN term` = str_squish(str_to_lower(`GMDN term`))) %>%
  filter(str_detect(`GMDN term`, input$focus_term) & str_detect(content, "(?i)hernia")) %>% 
  select(`Report number`) %>% 
  mutate(Term = TRUE) %>% 
  full_join(tidy_topics_docs, by = c(`Report number` = "document")) %>% 
  mutate(Term = ifelse(is.na(Term), "Other_Terms", "Selected_Term")) %>%  
  group_by(Level, topic, Term) %>% 
  summarise(p = mean(p)) %>% 
  pivot_wider(names_from = Term, values_from = p) %>% 
  mutate(ratio = Selected_Term/Other_Terms,
         kld = Selected_Term*log(Selected_Term/Other_Terms)) %>% 
  arrange(-kld) %>% 
  left_join(tidy_topics, by = c("Level", "topic")) %>% 
  group_by(Level, topic,Other_Terms, Selected_Term, ratio, kld) %>% 
  arrange(-p) %>%
  top_n(5,p) %>% 
  summarise(words = paste0(word, collapse = ", ")) %>% 
  arrange(-kld) %>% 
  select(Level, topic, words, Selected_Term, Other_Terms, ratio, kld) %>%
  mutate(Selected_Term = signif(Selected_Term, 3),
         Other_Terms = signif(Other_Terms, 3),
         ratio = signif(ratio, 3),
         kld = signif(kld, 3)) 

res %>% 
  DT::datatable()
(?i)((pelvi)|(urina)|(incont)|(vagina)|(uterus)|(anterior)|(posterior))

docs_metadata %>% 
  mutate(`GMDN term` = str_squish(str_to_lower(`GMDN term`))) %>%
  filter(str_detect(`GMDN term`, input$focus_term) & str_detect(content, "(?i)hernia")) %>% 
  select(`Report number`, content) %>% 
  mutate(Term = TRUE) %>% 
  full_join(tidy_topics_docs, by = c(`Report number` = "document")) %>% 
  filter(Term) %>% 
  filter(!str_detect(content, "pain"), Level == 2, topic == tidy_topics %>% filter(Level == 2, word == "pain") %>% pull(topic),p > 0) %>% 
  arrange(-p) %>% 
  DT::datatable()

tidy_topics %>% inner_join(tidy_topics %>% filter(word == "pain") %>% 
                             select(Level, topic), by = c("Level", "topic")) %>% 
  group_by(Level) %>% 
  arrange(-p) %>% 
  mutate(id = 1:n()) %>% 
  filter(id < 20) %>% 
  DT::datatable()
