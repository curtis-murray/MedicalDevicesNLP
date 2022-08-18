
top_man <- all_manufacturers %>% 
  top_n(100, count) %>% 
  pull(Manufacturer_ID)

library(heatmaply)
heatmaply(mat[top_man, top_man],colors = c("white", "red"), grid_gap = 1,
          dendrogram = "both",show_dendrogram = c(T,F),
          revC=T,main = "Distances between companies",
          # file = paste("Figures/heatmap_",dist,".html",sep = ""),
          dend_hoverinfo = F,column_text_angle = 90)

library(umap)

same <- tribble(~"Manufacturer.i", 
                ~"Manufacturer.j",
                ~"d")
thresh = 0.2

for(i in 1:(nrow(all_manufacturers)-1)){
  for(j in (i+1):nrow(all_manufacturers))
    
    if(mat[i,j] < thresh){
      
      same <- same %>% bind_rows(
        tibble(Manufacturer.i = all_manufacturers[i,"Manufacturer"],
               Manufacturer.j = all_manufacturers[j,"Manufacturer"],
               d = mat[i,j])
      )
    }
  
  
}

#custom.config = umap.defaults
#custom.config$n_neighbors = 3
data_umap <- mat %>% umap(
  #config = custom.config
) %>% .$layout %>%  
  as_tibble() %>%
  mutate(Manufacturer = all_manufacturers) %>% 
  left_join(all_reports %>% group_by(Manufacturer) %>% 
              summarise(count = n()))

data_umap %>% 
  top_n(100,count) %>% 
  ggplot() + 
  geom_point(aes(V1,V2,size = count),show.legend = F) + 
  ggrepel::geom_text_repel(aes(V1,V2, label= Manufacturer), max.overlaps = 999) + 
  theme_void()



# Number of Reports by date

all_reports %>% 
  mutate(Date = round_date(`Report date`, unit = "quarter")) %>%  
  group_by(Date) %>% 
  summarise(count = n()) %>% 
  ggplot() + 
  geom_col(aes(x = Date, y = count), stat = "identity")  + 
  theme_minimal() + 
  scale_x_date(date_breaks = "year", date_labels = "%Y") + 
  labs(x = "Report Date", y = "Number of Reports", title = "Number of Reports Over Time")

# Wordcloud of GMDN terms

all_reports %>%
  select(`GMDN term`) %>% 
  mutate(term = str_split(`GMDN term`, ',')) %>% 
  select(term) %>% unnest(term) %>% 
  mutate(term = str_squish(term) %>% str_to_lower()) %>% 
  group_by(term) %>% 
  summarise(count = n()) %>% 
  mutate(freq = count/sum(count)) %>% 
  top_n(50, count) %>% 
  arrange(-count) %>% 
  ggplot(aes(label = term, size = freq, color = freq)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_minimal()

all_reports %>% 
  unnest_tokens(word, `Event description`)
