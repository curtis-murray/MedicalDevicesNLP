# Load in data
library(tidyverse)

tidy_topics <- read_csv("data/hSBM/Clean/tidy_topics.csv")

tidy_topics_docs <- read_csv("data/hSBM/Clean/tidy_topics_docs.csv")

tidy_topics_docs_wide <- tidy_topics_docs %>% 
  pivot_wider(id_cols = c("document","doc_ID"),
              names_from = 1:2, values_from = p) %>% 
  arrange(document)

docs_all <- read_csv("data/hSBM/docs_all.csv") %>% 
  transmute(doc_ID = X1+1, `Report number` = `0`)

train <- read_csv("data/hSBM/train.csv")
docs_metadata <- docs_all %>% left_join(train, by = "Report number")

docs_metadata %>% DT::datatable()


docs_metadata %>% group_by(`Device classification`) %>% summarise(count = n()) %>% arrange(-count) %>% DT::datatable()

# Get series of topic densities for given topic at level for each document
plot_data <- tidy_topics_docs %>% 
  filter(Level == 1,
         topic == 28) %>% 
  select((-c(Level, topic))) %>% 
  left_join(docs_metadata %>% select(doc_ID, `GMDN term`, `Report date`), by = "doc_ID") %>% 
  filter(str_detect(`GMDN term`, paste("(?i)","mesh",sep=""))) %>% 
  select(Date = `Report date`, p) %>% 
  arrange(Date)

plot_data

# Smoothing above for plotting
plot_data %>% 
  group_by(Date) %>% 
  summarise(p = mean(p),count = n()) %>% 
  full_join(tibble(Date = seq(min(plot_data$Date),max(plot_data$Date), by = "1 day"))) %>% 
  arrange(Date) %>% 
  mutate(y = rollapply(p, 31, na.pad=TRUE, function(x) mean(x,na.rm = T)))