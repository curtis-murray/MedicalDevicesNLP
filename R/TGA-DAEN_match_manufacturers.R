library(tidyverse)
library(tidytext)
library(lubridate)
library(ggwordcloud)
library(RColorBrewer)
library(fuzzyjoin)
library(stringdist)
library(visNetwork)
library(htmlwidgets)
library(igraph)
all_reports <- read_csv("data/all_reports/all_reports_df.csv")

# Number of products by Manufacturer

all_reports %>% 
  group_by(Manufacturer) %>% 
  summarise(count = n()) %>% 
  top_n(50, count) %>% 
  ggplot() + 
  geom_histogram(aes(x = reorder(stringr::str_wrap(Manufacturer, 60), count), y = count), stat = "identity") + 
  coord_flip() + 
  theme_minimal() + 
  labs(x = "Manufacturer", y = "Number of Products",
       title = "Number of Products by Manufacturer")

tf_idf <- all_reports %>% unnest_tokens(word, Manufacturer,drop = F) %>% 
  group_by(Manufacturer) %>% 
  mutate(Manufacturer_count = n()) %>% 
  group_by(word, Manufacturer, Manufacturer_count) %>% 
  summarise(word_count =n()) %>% 
  arrange(-word_count) %>% 
  ungroup() %>% 
  bind_tf_idf(word, Manufacturer, word_count) %>% 
  pivot_wider(names_from = word, values_from = tf_idf, values_fill = 0) %>% 
  select(-c(word_count, tf, idf)) %>% 
  group_by(Manufacturer, Manufacturer_count)%>% 
  summarise_all(sum) %>% 
  arrange(-Manufacturer_count)

all_reports %>% 
  group_by(Manufacturer) %>% 
  summarise(count = n())

tibble(Manufacturer = "Boston Scientific Corporation") %>% 
  stringdist_join(all_reports %>% 
                    group_by(Manufacturer) %>%
                    summarise() %>% 
                    drop_na(),
                  ignore_case = TRUE, 
                  method = "jw", 
                  max_dist = 999, 
                  distance_col = "dist") %>% 
  arrange(dist)


stop_word_companies <- tibble(word = c(
  "ltd",
  "inc",
  "medical",
  "co",
  "pty",
  "corporation",
  "limited",
  "llc", 
  "systems",
  "international",
  "products",
  "technology",
  "surgical",
  "healthcare",
  "industries",
  "incorporated"
))

all_manufacturers <- all_reports %>% 
  group_by(Manufacturer) %>% 
  mutate(Manufacturer_ID = 1:n()) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  drop_na() %>% 
  mutate(Manufacturer_ID = 1:n()) %>% 
  mutate(word = str_split(Manufacturer, "[., ]+")) %>% 
  unnest(word) %>% 
  mutate(word = str_to_lower(word)) %>% 
  mutate(word = str_replace_all(word,"[^a-z0-9]+"," ")) %>% 
  anti_join(stop_word_companies, by = "word") %>% 
  group_by(Manufacturer,count, Manufacturer_ID) %>% 
  summarise(simple_manufacturer = paste(word, collapse = " ") %>% str_squish) %>% 
  ungroup() %>% 
  arrange(-count)

write_csv(all_manufacturers,"data/companies/all_manufacturers.csv")

all_manufacturers %>% 
  group_by(Manufacturer) %>% 
  summarise() %>% 
  unnest_tokens(word, Manufacturer,drop = F) %>% 
  count(Manufacturer, word, sort = T) %>% 
  bind_tf_idf(word, Manufacturer, n) %>% 
  arrange(tf_idf)

all_manufacturers %>% 
  group_by(Manufacturer) %>% 
  summarise() %>% 
  mutate(n_man = n()) %>% 
  unnest_tokens(word, Manufacturer,drop = F) %>% 
  group_by(word, Manufacturer, n_man)%>%
  summarise() %>% 
  arrange(Manufacturer) %>% 
  group_by(word) %>%
  summarise(prop_in = n()/n_man) %>% 
  group_by(word, prop_in) %>% 
  summarise() %>% 
  arrange(-prop_in) %>% 
  top_n(30, -prop_in) %>% 
  view()

mat <- stringdistmatrix(all_manufacturers$simple_manufacturer,
                        all_manufacturers$simple_manufacturer,
                        method = "jw")
# 
# for(i in 1:nrow(all_manufacturers)){
#   for(j in 1:nrow(all_manufacturers)){
#     man_1 = all_manufacturers$simple_manufacturer[i]
#     man_2 = all_manufacturers$simple_manufacturer[j]
#     mat[i,j] = str_detect(man_1, man_2)
#   }
# }

rownames(mat) <- all_manufacturers$Manufacturer
colnames(mat) <- all_manufacturers$Manufacturer

mat[upper.tri(mat)] <- NA
#mat = mat + mat %>% t


similar_companies <- mat %>% as_tibble() %>% 
  mutate(Manufacturer.x = rownames(mat)) %>% 
  pivot_longer(cols = -Manufacturer.x, 
               names_to = "Manufacturer.y",
               values_to = "d") %>% 
  arrange(d)

similar_companies %>% 
  write_csv("data/companies/similar_companies.csv")


same_companies <- similar_companies %>% filter(d < 2)

company_network <- same_companies %>% bind_rows(same_companies %>% 
                                                  mutate(Manufacturer = Manufacturer.y,
                                                         Manufacturer.y = Manufacturer.x,
                                                         Manufacturer.x = Manufacturer) %>%
                                                  select(-Manufacturer)) %>% 
  group_by_all() %>% 
  summarise() %>% #Removes duplicates
  ungroup() %>% 
  #select(from = Manufacturer.x, to = Manufacturer.y) %>% 
  left_join(all_manufacturers %>% select(Manufacturer, from = Manufacturer_ID), 
            by = c("Manufacturer.x" = "Manufacturer")) %>%
  left_join(all_manufacturers %>% select(Manufacturer, to = Manufacturer_ID),
            by = c("Manufacturer.y" = "Manufacturer"))


edges <- company_network %>% 
  transmute(from, to, size = 1,
            #    length = d
  ) %>% 
  filter(from != to)

nodes <- all_manufacturers %>% 
  transmute(id = Manufacturer_ID,
            label = Manufacturer,
            size = count %>% as.numeric()) %>% 
  mutate(size = size /5,
         font.size = 10*sqrt(size)) %>% 
  filter(id %in% company_network$from | id %in% company_network$to) %>% 
  ungroup() %>% 
  select(id, label, size, font.size)

my_graph <- graph_from_data_frame(edges,
                                  directed = FALSE, 
                                  vertices = nodes
)

saveWidget(visIgraph(my_graph,idToLabel = F,physics = T) %>% 
             visOptions(highlightNearest = list(enabled = T, hover = T),
                        nodesIdSelection = T,
                        height = 1440, 
                        width = 2560),
           file = "Figures/company_network.html")


my_graph %>% 
  igraph::component_distribution()

comp <- my_graph %>% 
  components()

my_graph %>% 
  components()

get_component_name <- function(ids){
  nodes %>% filter(id %in% ids) %>% 
    filter(size == max(size)) %>% 
    filter(str_length(label) == min(str_length(label))) %>%
    pull(label) %>% .[1]
}

get_component_names <- function(ids){
  names <- all_manufacturers %>% filter(Manufacturer_ID %in% ids) %>% 
    pull(simple_manufacturer)
  Reduce(intersect, str_split(names," ")) %>% 
    paste(collapse = " ")
}

name_mapping <- tibble(component = groups(comp)) %>% 
  mutate(mapped_name = map(component, get_component_name)) %>% 
  unnest(mapped_name) %>% 
  unnest(component) %>% 
  transmute(Manufacturer_ID = component %>% as.numeric(), mapped_name)

all_manufacturers_named <- all_manufacturers %>% 
  left_join(name_mapping,  by = "Manufacturer_ID") %>% 
  group_by(mapped_name) %>% 
  summarise(count = sum(count)) %>% 
  left_join(all_manufacturers %>% select(Manufacturer, Manufacturer_ID), 
            by = c("mapped_name" = "Manufacturer")) %>% 
  arrange(-count) %>% 
  transmute(Manufacturer = mapped_name, count, Manufacturer_ID)

write_csv(all_manufacturers_named, "data/companies/all_manufacturers_named.csv")

all_reports_mached_manufacturer_df <- all_reports %>% left_join(all_manufacturers, by = "Manufacturer") %>% 
  left_join(name_mapping, by = "Manufacturer_ID") 

all_reports_mached_manufacturer_df %>% 
  write_csv("data/all_reports/all_reports_mached_manufacturer_df.csv")

# train <- all_reports_mached_manufacturer_df %>% 
#   unnest_tokens(word, `Event description`) %>% 
#   anti_join(stop_words, by = "word") %>% 
#   group_by(word) %>% 
#   filter(n() > 3) %>% 
#   group_by(`Report number`) %>% 
#   filter(n() > 3) %>% 
#   group_by(across(-word)) %>% 
#   summarise(content = paste0(word, collapse = " "))
# 
# write_csv(train, "data/hSBM/train.csv")

all_manufacturers_named %>% 
  top_n(50, count) %>% 
  ggplot() + 
  geom_histogram(aes(x = reorder(stringr::str_wrap(Manufacturer, 60), count), y = count), stat = "identity") + 
  coord_flip() + 
  theme_minimal() + 
  labs(x = "Manufacturer", y = "Number of Products",
       title = "Number of Products by Manufacturer")

