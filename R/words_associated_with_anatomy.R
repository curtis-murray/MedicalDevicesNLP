library(tidyverse)
library(tidytext)
library(ggridges)
library(epitools)
library(DT)

reports <- read_csv("data/all_reports/all_reports_mached_manufacturer_df.csv")
#anatomical_dictionary <- read_csv("data/anatomy/anatomical_dictionary.csv")
anatomical_mapping <- read_csv("data/anatomy/anatomical_mapping.csv")
anatomical_terms <- anatomical_mapping %>% 
  group_by(part) %>% 
  summarise() %>% 
  ungroup()

mesh_reports <- reports %>% 
  mutate(`GMDN term` = str_to_lower(`GMDN term`)) %>%
  filter(str_detect(`GMDN term`, "mesh")) %>% 
  group_by(across(-c(X1))) %>% 
  summarise() %>% 
  ungroup()


# Number of times a body part is used
mesh_reports %>% 
  unnest_tokens(word,`Event description`,drop = F) %>%
  mutate(is_anatomy = word %in% anatomical_terms$part) %>% 
  group_by(mapped_name,`Report number`) %>% 
  summarise(count = sum(is_anatomy)) %>%
  group_by(mapped_name) %>% 
  summarise(
    mean = mean(count),
    n = n(),
    var = var(count)
  ) %>% 
  arrange(-mean) %>% 
  filter(n >= 10)

# Manufactur summary
man_counts <- mesh_reports %>% 
  group_by(mapped_name) %>% 
  summarise(n_reports = n()) %>% 
  arrange(-n_reports) %>% 
  mutate(n_reports_total = sum(n_reports))

# Fisher exact test to determine if the odds ratio
# is greater than 2 for anatomy word use in one
# manufacturer to the others
mesh_reports %>% 
  unnest_tokens(word,`Event description`,drop = F) %>%
  mutate(is_anatomy = word %in% anatomical_terms$part) %>% 
  select(`Report number`,mapped_name, is_anatomy, word) %>% 
  filter(is_anatomy) %>% 
  group_by(mapped_name,word,`Report number`) %>% 
  summarise() %>% 
  group_by(mapped_name,word) %>% 
  summarise(count = n()) %>% 
  ungroup()%>% 
  full_join(man_counts, by = "mapped_name") %>% 
  transmute(Manufacturer = mapped_name,word, count, n_reports,n_reports_total) %>%
  filter(n_reports > 10) %>% 
  group_by(word) %>% 
  mutate(word_count_total = sum(count)) %>% 
  mutate(n_reports_other = n_reports_total-n_reports,
         word_count_other = word_count_total - count) %>% 
  group_by(Manufacturer, word) %>% 
  nest() %>% 
  mutate(test = map(data, function(data){
    dat <- data.frame(
      "word_yes" = c(data$count, data$word_count_other),
            "word_no" = c(data$n_reports-data$count, data$n_reports_other-data$word_count_other),
            row.names = c("Manufacturer", "Others")
            )
    #print(dat)
      fisher.test(dat, alternative = "greater",or=2,conf.int = T) %>% 
      broom::tidy()
  })) %>% 
  unnest(test) %>% 
  select(Manufacturer,word,p.value) %>%
  filter(p.value < 0.05) %>% 
  arrange(p.value) %>% 
  datatable()

mesh_reports %>% 
  select(Manufacturer = mapped_name, `Event description`) %>% 
  filter(str_detect(`Event description`, "breast")) %>% 
  datatable()


mesh_reports %>% 
  unnest_tokens(word,`Event description`,drop = F) %>%
  mutate(is_anatomy = word %in% anatomical_terms$part) %>% 
  select(`Report number`,mapped_name, is_anatomy, word) %>% 
  filter(is_anatomy) %>% 
  group_by(mapped_name,word,`Report number`) %>% 
  summarise() %>% 
  group_by(mapped_name,word) %>% 
  summarise(count = n()) %>% 
  ungroup()%>% 
  full_join(man_counts, by = "mapped_name") %>% 
  transmute(Manufacturer = mapped_name,word, count, n_reports) %>%
  filter(n_reports > 10) %>% 
  mutate(count = round(count/n_reports,3)) %>% 
  pivot_wider(names_from = "word", values_from = "count") %>% 
  mutate_all(function(x){ifelse(is.na(x),0,x)} ) %>% 
  datatable()
