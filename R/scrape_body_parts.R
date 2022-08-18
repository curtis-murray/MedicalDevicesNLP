library(tidyverse)
library(rvest)
library(tidyverse)
library(xml2)
library(purrrlyr)


url <- "https://www.collinsdictionary.com/word-lists/body-parts-of-the-body"
url_heart <- "https://www.collinsdictionary.com/word-lists/heart-parts-of-the-heart"
url_brain <- "https://www.collinsdictionary.com/word-lists/brain-parts-of-the-brain"
url_eye <- "https://www.collinsdictionary.com/word-lists/eye-parts-of-the-eye"
url_ear <- "https://www.collinsdictionary.com/word-lists/ear-parts-of-the-ear"

body_parts <- read_html(url)

part <- body_parts %>% 
  html_node(".note") %>% 
  html_nodes(".tr")

heart_parts <- read_html(url_heart)

part_heart <- heart_parts %>%  
  html_node(".note") %>% 
  html_nodes(".tr")

brain_parts <- read_html(url_brain)

part_brain <- brain_parts %>%  
  html_node(".note") %>% 
  html_nodes(".tr")

eye_parts <- read_html(url_eye)

part_eye <- eye_parts %>%  
  html_node(".note") %>% 
  html_nodes(".tr")

ear_parts <- read_html(url_ear)

part_ear <- ear_parts %>%  
  html_node(".note") %>% 
  html_nodes(".tr")

get_text <- function(input,full=TRUE){
  parts <- input %>% 
    html_nodes(".td") %>% 
    html_text() %>% 
    str_squish()
  if(length(parts) == 3 & full){
    parts[4] <- parts[3]
    parts[3] <- "-"
  }else if(!full){
    parts[3] <- "-"
    parts[4] <- parts[2]
    parts[2] <- "-"
  }
  tibble(part = parts[1], equiv = parts[2],adj = parts[3], desc = parts[4])
}

anatomical_dictionary <- map_df(part,get_text)
heart_dictionary <- map_df(part_heart,~get_text(., full=FALSE))
brain_dictionary <-  map_df(part_brain,~get_text(., full=FALSE))
ear_dictionary <-  map_df(part_ear,~get_text(., full=FALSE))
eye_dictionary <-  map_df(part_eye,~get_text(., full=FALSE))

anatomical_dictionary <- anatomical_dictionary %>% 
  bind_rows(heart_dictionary) %>% 
  bind_rows(ear_dictionary) %>% 
  bind_rows(eye_dictionary) %>% 
  bind_rows(brain_dictionary)

write_csv(anatomical_dictionary, "data/anatomy/anatomical_dictionary.csv")

get_word <- function(desc){
  c(
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
      ),"'ies\\b",sep ="")) %>% 
        unlist()
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
# 
# connections <- anatomical_dictionary %>% 
#   mutate(connected = map(desc, get_word)) %>% 
#   unnest(connected) %>% 
#   unnest(connected) %>%
#   transmute(from = part, to = connected) %>% 
#   filter(!(to %in% c("character(","character(0","charactery","y")))
# 
# my_graph <- graph_from_data_frame(connections, 
#                                   directed = TRUE,
#                                   vertices = anatomical_dictionary %>% 
#                                     select(label = part))
# 
# 
# 
# saveWidget(visIgraph(my_graph, 
#                      idToLabel = T,
#                      physics = T) %>% 
#              visOptions(highlightNearest = list(enabled = T, hover = T), 
#                         nodesIdSelection = T,height = 1440, width = 2560) %>% 
#              visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -200,
#                                                                              springLength = 100)),
#            file = "Figures/anatomy.html")


anatomical_mapping <- anatomical_dictionary %>% 
  mutate_at(c("adj","equiv"),~str_split(.x, ",|(or)|(,or)")) %>% 
  unnest(adj) %>% 
  unnest(equiv) %>% 
  mutate_at(c("adj","equiv"),str_squish) %>% 
  mutate(part_rep = part) %>% 
  pivot_longer(cols = c(equiv, adj,part_rep), names_to = "type",values_to = "same") %>% 
  group_by(part,same,desc) %>% 
  summarise() %>% 
  filter(same != "-") %>% 
  ungroup()

write_csv(anatomical_mapping, "data/anatomy/anatomical_mapping.csv")


connections <- anatomical_mapping %>% 
  mutate(connected = map(desc, get_word_adv)) %>% 
  unnest(connected) %>% 
  unnest(connected) %>%
  transmute(from = part, to = connected) %>% 
  filter(!(to %in% c("character(","character(0","charactery"))) %>% 
  filter(!(to %in% c("","y"))) %>% 
  group_by(from,to) %>% 
  summarise() %>% 
  ungroup() %>% 
  left_join(anatomical_mapping %>% select(part, same), by = c("to"="same")) %>% 
  transmute(from, to = part) %>% 
  group_by(from,to) %>% 
  summarise()

write_csv(connections,"data/anatomy/connections.csv")
  

my_graph <- graph_from_data_frame(connections, 
                                  directed = T,
                                  vertices = anatomical_dictionary %>% 
                                    select(label = part))


saveWidget(visIgraph(my_graph, 
                     idToLabel = T,
                     physics = T) %>% 
             visOptions(highlightNearest = list(enabled = T, hover = T), 
                        nodesIdSelection = T,height = 1440, width = 2560) %>% 
             visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -200,
                                                                             springLength = 100)),
           file = "Figures/anatomy_adv.html")
