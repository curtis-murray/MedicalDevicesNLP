#----- Pain topic other
library(epuRate)
library(rmarkdown)
library(tidyverse)
library(lubridate)
library(mds)
library(mdsstat)

df <- read_csv("data/all_reports/all_reports_mached_manufacturer_df.csv")


mesh_filtered <- df %>% 
  mutate(`GMDN term` = str_to_lower(`GMDN term`)) %>%
  filter(str_detect(`GMDN term`, "mesh")) %>% 
  group_by(`GMDN term`) %>% 
  mutate(hernia = str_detect(`Event description`, "(?i)hernia"),
         pelvic = str_detect(`Event description`, "(?i)((pelvi)|(urina)|(incont)|(vagina)|(uterus)|(anterior)|(posterior))")) %>% 
  filter(hernia | pelvic) %>% 
  filter(!str_detect(`Event description`,"Journal Article")) %>% 
  filter(`Report date` < "2018-01-01")

mesh_pain <- mesh_filtered %>% 
  mutate(pain = str_detect(`Event description`, "(?i)pain")) %>% 
  group_by(hernia, pelvic, pain) %>% 
  summarise(count = n())


pain_table <- mesh_pain %>% 
  ungroup() %>% 
  mutate(type = ifelse(pelvic, "pelvic","hernia"),
         pain = ifelse(pain, "pain","no pain")) %>% 
  select(type,pain,count) %>% 
  pivot_wider(values_from = count, names_from = pain) %>% 
  select(type, pain, `no pain`) %>% 
  as.data.frame()

rownames(pain_table) <- pain_table$type
pain_table <- pain_table[,2:3]
pain_table


mesh_filtered <- df %>% 
  mutate(`GMDN term` = str_to_lower(`GMDN term`)) %>%
  mutate(
    hernia = (str_detect(`Event description`, "(?i)hernia") & 
                str_detect(`GMDN term`, "mesh"))
  ) %>% 
  mutate(other = !hernia) %>% 
  ungroup() %>% 
  mutate(Date = floor_date(`Report date`, unit = unit)) %>% 
  filter(Date < "2018-01-01")

mesh_pain <- mesh_filtered %>% 
  ungroup() %>% 
  left_join(tidy_topics_docs %>% 
              filter(Level == 1, topic == pain_topic_1),
            by = c("Report number" = "document")) %>% 
  select(other, hernia, p, `Event description`, Date) %>% 
  mutate(pain = ifelse(p >= 0.05, "pain threshold", "no pain threshold")) %>% 
  mutate(pain = ifelse(is.na(p), "no pain threshold",pain)) %>% 
  group_by(other, hernia, pain, Date) %>% 
  summarise(count = n()) 

pain_table <- mesh_pain %>% 
  ungroup() %>% 
  mutate(type = ifelse(hernia, "hernia","other")) %>% 
  select(Date,type,pain,count) %>% 
  pivot_wider(values_from = count, names_from = pain) %>% 
  arrange(Date) %>% 
  bind_rows(
    tibble(Date = seq.Date(from = min(mesh_pain$Date), to = max(mesh_pain$Date), by = unit)) %>% 
      mutate(type = "hernia", `no pain threshold` = 0, `pain threshold` = 0)
  ) %>% 
  bind_rows(
    tibble(Date = seq.Date(from = min(mesh_pain$Date), to = max(mesh_pain$Date), by = unit)) %>% 
      mutate(type = "other", `no pain threshold` = 0, `pain threshold` = 0)
  ) %>% 
  group_by(Date, type) %>% 
  summarise(`pain threshold`= sum(`pain threshold`, na.rm = T),
            `no pain threshold` = sum(`no pain threshold`, na.rm = T)) %>% 
  pivot_wider(names_from = type, values_from = c("pain threshold", "no pain threshold")) %>% 
  ungroup() %>% 
  transmute(time = Date, nA = `pain threshold_hernia`, nB = `no pain threshold_hernia`, nC = `pain threshold_other`, nD = `no pain threshold_other`,event = nA) %>% 
  as.data.frame()

pain_table
run_algos(pain_table, algos)