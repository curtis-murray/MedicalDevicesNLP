library(tidyverse)
library(tidytext)
library(ggridges)

reports <- read_csv("data/all_reports/all_reports_mached_manufacturer_df.csv")
anatomical_dictionary <- read_csv("data/anatomy/anatomical_dictionary.csv")

mesh_reports <- reports %>% 
  mutate(`GMDN term` = str_to_lower(`GMDN term`)) %>%
  filter(str_detect(`GMDN term`, "pacemaker")) %>% 
  group_by(across(-c(X1))) %>% 
  summarise()

mesh_reports %>%
  unnest_tokens(word, `Event description`) %>% 
  anti_join(stop_words, by = "word") %>% 
  group_by(`Report number`) %>%
  summarise(text = paste0(word, collapse = " ")) %>% 
  write_csv("data/mesh_reports/mesh_reports_exculde_stopwords.csv")

mesh_tf_idf <- mesh_reports %>% 
  unnest_tokens(word,`Event description`,drop = F) %>% 
  group_by(mapped_name,word) %>% 
  summarise(count = n()) %>% 
  bind_tf_idf(word, mapped_name, count) %>%
  arrange(-tf_idf)


get_word <- function(desc){
  res <- c(
    str_extract_all(desc,paste("\\b",anatomical_dictionary$part,"\\b",sep ="")),
    gsub(
      pattern = '.{2}$',
      replacement =  '',
      str_extract_all(desc,paste("\\b",anatomical_dictionary$part,"es\\b",sep ="")) %>% 
        unlist()
    ),
    gsub(
      pattern = '.{1}$',
      replacement =  '',
      str_extract_all(desc,paste("\\b",anatomical_dictionary$part,"s\\b",sep ="")) %>% 
        unlist()
    ),
    gsub(
      pattern = '.{2}$',
      replacement =  '',
      str_extract_all(desc,paste("\\b",anatomical_dictionary$part,"'s\\b",sep ="")) %>% 
        unlist()
    ),
    gsub(
      pattern = '.{3}$',
      replacement =  '',
      str_extract_all(desc,paste("\\b",gsub(
        pattern = '.{1}$',
        replacement =  '',
        anatomical_dictionary$part
      ),"'ies\\b",sep ="")) %>% unlist()
    ) %>% paste("y",sep ="")
  )
}
get_word_adv <- function(desc){
  c(
    str_extract_all(desc,paste("\\b",anatomical_mapping$same,"\\b",sep ="")),
    gsub(
      pattern = '.{2}$',
      replacement =  '',
      str_extract_all(desc,paste("\\b",anatomical_mapping$same,"es\\b",sep ="")) %>% 
        unlist()
    ),
    gsub(
      pattern = '.{1}$',
      replacement =  '',
      str_extract_all(desc,paste("\\b",anatomical_mapping$same,"s\\b",sep ="")) %>% 
        unlist()
    ),
    gsub(
      pattern = '.{2}$',
      replacement =  '',
      str_extract_all(desc,paste("\\b",anatomical_mapping$same,"'s\\b",sep ="")) %>% 
        unlist()
    ),
    gsub(
      pattern = '.{3}$',
      replacement =  '',
      str_extract_all(desc,paste("\\b",gsub(
        pattern = '.{1}$',
        replacement =  '',
        anatomical_mapping$same
      ),"'ies\\b",sep ="")) %>% 
        unlist()
    ) %>% paste("y",sep ="")
  )
}


pain_locations <- mesh_reports %>%
  ungroup() %>% 
  select(`Event description`,`Report number`) %>% 
  #filter(`Report number` == 55616) %>% 
  unnest_tokens(sentence, `Event description`,token = "sentences") %>% 
  filter(str_detect(sentence, "pain")) %>% 
  mutate(location = map(sentence, get_word)) %>% 
  unnest(location) %>% 
  unnest(location) %>% 
  filter(!(location %in% c("character(","character(0","charactery","y"))) %>% 
  group_by(`Report number`,location) %>% 
  summarise() %>% 
  group_by(location) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  bind_rows(tibble(location = connections$from, 
                   count = 0)) %>% 
  bind_rows(tibble(location =connections$to,
                   count = 0)) %>% 
  group_by(location) %>% 
  summarise(count = sum(count))

pain_connections <- connections %>% 
  filter(to %in% pain_locations$location) %>% 
  filter(from %in% pain_locations$location)


my_graph <- graph_from_data_frame(connections, 
                                  vertices = pain_locations %>% 
                                    mutate(size = ifelse(count > 0, count+10,1)*2) %>%
                                    mutate(font.size = size+10) %>% 
                                    mutate(color = ifelse(count == 0,"#8D94BA", "#87677B")),
                                  directed = TRUE)

saveWidget(visIgraph(my_graph, 
                     idToLabel = T,
                     physics = T) %>% 
             visOptions(highlightNearest = list(enabled = T, hover = T), 
                        nodesIdSelection = T,height = 1440, width = 2560) %>% 
             visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -200,
                                                                             springLength = 100)),
           file = "Figures/pain_anatomy.html")


pain_locations <- mesh_reports %>%
  ungroup() %>% 
  select(`Event description`,`Report number`) %>% 
  #filter(`Report number` == 55616) %>% 
  unnest_tokens(sentence, `Event description`,token = "sentences") %>% 
  filter(str_detect(sentence, "pain")) %>% 
  mutate(location = map(sentence, get_word_adv)) %>% 
  unnest(location) %>% 
  unnest(location) %>% 
  filter(!(location %in% c("character(","character(0","charactery"))) %>% 
  filter(!(location %in% c("","y"))) %>% 
  left_join(anatomical_mapping %>% select(part, same), by = c("location"="same")) %>% 
  transmute(location = part,`Report number`,sentence) %>% 
  group_by(`Report number`,location,sentence) %>% 
  summarise() %>% 
  group_by(location) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  ungroup() %>% 
  bind_rows(tibble(location = connections$from, 
                   count = 0)) %>% 
  bind_rows(tibble(location =connections$to,
                   count = 0)) %>% 
  group_by(location) %>% 
  summarise(count = sum(count))

pain_connections <- connections %>% 
  filter(to %in% pain_locations$location) %>% 
  filter(from %in% pain_locations$location)


my_graph <- graph_from_data_frame(connections, 
                                  vertices = pain_locations %>% 
                                    mutate(size = ifelse(count > 0, count+10,1)*2) %>%
                                    mutate(size = size/sum(size)*1000) %>% 
                                    mutate(font.size = size+10) %>% 
                                    mutate(color = ifelse(count == 0,"#8ecae6", "#031D44")),
                                  directed = TRUE)

saveWidget(visIgraph(my_graph, 
                     idToLabel = T,
                     physics = T) %>% 
             visOptions(highlightNearest = list(enabled = T, hover = T), 
                        nodesIdSelection = T,height = 1440, width = 2560) %>% 
             visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -200,
                                                                             springLength = 100,
                                                                             damping = 1)),
           file = "Figures/pain_anatomy_adv.html")

report_connections <- mesh_reports %>%
  ungroup() %>% 
  select(`Event description`,`Report number`) %>% 
  #filter(`Report number` == 55616) %>% 
  unnest_tokens(sentence, `Event description`,token = "sentences") %>% 
  filter(str_detect(sentence, "pain")) %>% 
  mutate(location = map(sentence, get_word_adv)) %>% 
  unnest(location) %>% 
  unnest(location) %>% 
  filter(!(location %in% c("character(","character(0","charactery"))) %>% 
  filter(!(location %in% c("","y"))) %>% 
  transmute(`Report number`,location) %>%
  group_by_all() %>% 
  summarise() %>% 
  mutate(from = location,
         to = location) %>% 
  group_by(`Report number`) %>% 
  transmute(nest(expand_grid(from, to))) %>% 
  unnest(data) %>% 
  group_by_all() %>% 
  summarise() %>% 
  ungroup()
#select(-`Report number`) %>% 
#group_by() %>% 
#summarise(size = n()) %>% 
#ungroup()

all_locs <- report_connections %>%
  transmute(`Report number`, label = from) %>%
  bind_rows(report_connections %>% 
              transmute(`Report number`, label = to)) %>% 
  group_by_all() %>% 
  summarise() %>% 
  group_by(label) %>% 
  summarise(size = n()) %>% 
  mutate(font.size = size)

my_graph <- graph_from_data_frame(report_connections %>% 
                                    filter(from != to) %>% 
                                    select(-`Report number`) %>% 
                                    group_by_all() %>% 
                                    summarise(length = 1000/n()) %>% 
                                    ungroup(), 
                                  vertices = all_locs,
                                  directed = TRUE)

saveWidget(visIgraph(my_graph, 
                     idToLabel = T,
                     physics = F) %>% 
             visOptions(highlightNearest = list(enabled = T, hover = T), 
                        nodesIdSelection = T,height = 1440, width = 2560) %>% 
             visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -1000)),
           file = "Figures/pain_anatomy_adv_two.html")

