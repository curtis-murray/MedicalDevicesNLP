library(tidyverse)
library(lubridate)
df <- read_csv("data/all_reports/all_reports_mached_manufacturer_df.csv")

# Fisher test that the odds ratio between pain levels in pelvic reports and other reports is equal to 1.

# -------
# Pelvic vs hernia
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
  as.data.frame()

rownames(pain_table) <- pain_table$type
pain_table <- pain_table[,2:3]
pain_table

fisher.test(pain_table)


# --------- 
# Pelvic against all other devices

df <- read_csv("data/all_reports/all_reports_mached_manufacturer_df.csv")

mesh_filtered <- df %>% 
  mutate(`GMDN term` = str_to_lower(`GMDN term`)) %>%
  mutate(
    pelvic = (str_detect(`Event description`, "(?i)((pelvi)|(urina)|(incont)|(vagina)|(uterus)|(anterior)|(posterior))") & 
                str_detect(`GMDN term`, "mesh"))
  ) %>% 
  mutate(other = !pelvic) %>% 
  filter(`Report date` < "2018-01-01")

mesh_pain <- mesh_filtered %>% 
  mutate(pain = str_detect(`Event description`, "pain")) %>% 
  group_by(other, pelvic, pain) %>% 
  summarise(count = n())

pain_table <- mesh_pain %>% 
  ungroup() %>% 
  mutate(type = ifelse(pelvic, "pelvic","other"),
         pain = ifelse(pain, "pain","no pain")) %>% 
  select(type,pain,count) %>% 
  pivot_wider(values_from = count, names_from = pain) %>% 
  as.data.frame()

rownames(pain_table) <- pain_table$type
pain_table <- pain_table[,2:3]
pain_table

fisher.test(pain_table)


#----- Pain topic
tidy_topics <- read_csv("data/hSBM/Clean/tidy_topics.csv")

pain_topic_1 <- tidy_topics %>% filter(word == "pain", Level == 1) %>% 
  pull(topic)

tidy_topics %>% 
  filter(Level == 1, topic == pain_topic_1) %>% 
  DT::datatable()

mesh_filtered <- df %>% 
  mutate(`GMDN term` = str_to_lower(`GMDN term`)) %>%
  filter(str_detect(`GMDN term`, "mesh")) %>% 
  group_by(`GMDN term`) %>% 
  mutate(hernia = str_detect(`Event description`, "(?i)hernia"),
         pelvic = str_detect(`Event description`, "(?i)((pelvi)|(urina)|(incont)|(vagina)|(uterus)|(anterior)|(posterior))")) %>% 
  filter(hernia | pelvic) %>% 
  filter(!str_detect(`Event description`,"Journal Article"))

mesh_pain <- mesh_filtered %>% 
  ungroup() %>% 
  left_join(tidy_topics_docs %>% 
              filter(Level == 1, topic == pain_topic_1),
            by = c("Report number" = "document")) %>% 
  select(hernia, pelvic, p, `Event description`) %>% 
  mutate(pain = ifelse(p >= 0.3, "pain threshold", "no pain threshold")) %>% 
  mutate(pain = ifelse(is.na(p), "no pain threshold",pain)) %>% 
  group_by(hernia, pelvic, pain) %>% 
  summarise(count = n())

pain_table <- mesh_pain %>% 
  ungroup() %>% 
  mutate(type = ifelse(pelvic, "pelvic","other")) %>% 
  select(type,pain,count) %>% 
  pivot_wider(values_from = count, names_from = pain) %>% 
  select(type, `pain threshold`, `no pain threshold`) %>% 
  as.data.frame()


rownames(pain_table) <- pain_table$type
pain_table <- pain_table[,2:3]
pain_table

fisher.test(pain_table)

#----- Pain 

unit = "years"

mesh_filtered <- df %>% 
  mutate(`GMDN term` = str_to_lower(`GMDN term`)) %>%
  filter(str_detect(`GMDN term`, "mesh")) %>% 
  group_by(`GMDN term`) %>% 
  mutate(hernia = str_detect(`Event description`, "(?i)hernia"),
         pelvic = str_detect(`Event description`, "(?i)((pelvi)|(urina)|(incont)|(vagina)|(uterus)|(anterior)|(posterior))")) %>% 
  filter(hernia | pelvic) %>% 
  filter(!str_detect(`Event description`,"Journal Article")) %>% 
  filter(`Report date` < "2018-01-01") %>% 
  ungroup() %>% 
  mutate(Date = floor_date(`Report date`, unit = unit))

mesh_pain <- mesh_filtered %>% 
  mutate(pain = str_detect(`Event description`, "(?i)pain")) %>% 
  group_by(hernia, pelvic, pain, Date) %>% 
  summarise(count = n())


pain_table <- mesh_pain %>% 
  ungroup() %>% 
  mutate(type = ifelse(pelvic, "pelvic","hernia"),
         pain = ifelse(pain, "pain","no pain")) %>% 
  select(Date,type,pain,count) %>% 
  pivot_wider(values_from = count, names_from = pain) %>% 
  as.data.frame() %>% 
  arrange(Date) %>% 
  bind_rows(
    tibble(Date = seq.Date(from = min(mesh_pain$Date), to = max(mesh_pain$Date), by = unit)) %>% 
      mutate(type = "pelvic", `no pain` = 0, `pain` = 0)
  ) %>% 
  bind_rows(
    tibble(Date = seq.Date(from = min(mesh_pain$Date), to = max(mesh_pain$Date), by = unit)) %>% 
      mutate(type = "hernia", `no pain` = 0, `pain` = 0)
  ) %>% 
  group_by(Date, type) %>% 
  summarise(pain = sum(pain, na.rm = T),
            `no pain` = sum(`no pain`, na.rm = T)) %>% 
  pivot_wider(names_from = type, values_from = c("pain", "no pain")) %>% 
  ungroup() %>% 
  transmute(time = Date, nA = pain_pelvic, nB = `no pain_pelvic`, nC = pain_hernia, nD = `no pain_hernia`) %>% 
  as.data.frame() %>% 
  mutate(ids = 1:n())

prr(pain_table)
bcpnn(pain_table)

#----- Pain topic hernia

mesh_filtered <- df %>% 
  mutate(`GMDN term` = str_to_lower(`GMDN term`)) %>%
  filter(str_detect(`GMDN term`, "mesh")) %>% 
  group_by(`GMDN term`) %>% 
  mutate(hernia = str_detect(`Event description`, "(?i)hernia"),
         pelvic = str_detect(`Event description`, "(?i)((pelvi)|(urina)|(incont)|(vagina)|(uterus)|(anterior)|(posterior))")) %>% 
  filter(hernia | pelvic) %>% 
  filter(!str_detect(`Event description`,"Journal Article")) %>% 
  filter(`Report date` < "2018-01-01") %>% 
  mutate(Date = floor_date(`Report date`, unit = unit))

mesh_pain <- mesh_filtered %>% 
  ungroup() %>% 
  left_join(tidy_topics_docs %>% 
              filter(Level == 1, topic == pain_topic_1),
            by = c("Report number" = "document")) %>% 
  select(hernia, pelvic, p, `Event description`, Date) %>% 
  mutate(pain = ifelse(p >= 0.05, "pain threshold", "no pain threshold")) %>% 
  mutate(pain = ifelse(is.na(p), "no pain threshold",pain)) %>% 
  group_by(hernia, pelvic, pain, Date) %>% 
  summarise(count = n()) 

pain_table <- mesh_pain %>% 
  ungroup() %>% 
  mutate(type = ifelse(pelvic, "pelvic","hernia")) %>% 
  select(Date,type,pain,count) %>% 
  pivot_wider(values_from = count, names_from = pain) %>% 
  as.data.frame() %>% 
  arrange(Date) %>% 
  bind_rows(
    tibble(Date = seq.Date(from = min(mesh_pain$Date), to = max(mesh_pain$Date), by = unit)) %>% 
      mutate(type = "pelvic", `no pain` = 0, `pain` = 0)
  ) %>% 
  bind_rows(
    tibble(Date = seq.Date(from = min(mesh_pain$Date), to = max(mesh_pain$Date), by = unit)) %>% 
      mutate(type = "hernia", `no pain` = 0, `pain` = 0)
  ) %>% 
  group_by(Date, type) %>% 
  summarise(`pain threshold` = sum(`pain threshold`, na.rm = T),
            `no pain threshold` = sum(`no pain threshold`, na.rm = T)) %>% 
  pivot_wider(names_from = type, values_from = c("pain threshold", "no pain threshold")) %>% 
  ungroup() %>% 
  transmute(time = Date, nA = `pain threshold_pelvic`, nB = `no pain threshold_pelvic`, nC = `pain threshold_hernia`, nD = `no pain threshold_hernia`) %>% 
  as.data.frame() %>% 
  mutate(ids = 1:n())

prr(pain_table)
bcpnn(pain_table)

#----- Pain topic other

mesh_filtered <- df %>% 
  mutate(`GMDN term` = str_to_lower(`GMDN term`)) %>%
  mutate(
    pelvic = (str_detect(`Event description`, "(?i)((pelvi)|(urina)|(incont)|(vagina)|(uterus)|(anterior)|(posterior))") & 
                str_detect(`GMDN term`, "mesh"))
  ) %>% 
  mutate(other = !pelvic) %>% 
  ungroup() %>% 
  mutate(Date = floor_date(`Report date`, unit = unit)) %>% 
  filter(Date < "2018-01-01")

mesh_pain <- mesh_filtered %>% 
  ungroup() %>% 
  left_join(tidy_topics_docs %>% 
              filter(Level == 1, topic == pain_topic_1),
            by = c("Report number" = "document")) %>% 
  select(other, pelvic, p, `Event description`, Date) %>% 
  mutate(pain = ifelse(p >= 0.05, "pain threshold", "no pain threshold")) %>% 
  mutate(pain = ifelse(is.na(p), "no pain threshold",pain)) %>% 
  group_by(other, pelvic, pain, Date) %>% 
  summarise(count = n()) 

pain_table <- mesh_pain %>% 
  ungroup() %>% 
  mutate(type = ifelse(pelvic, "pelvic","other")) %>% 
  select(Date,type,pain,count) %>% 
  pivot_wider(values_from = count, names_from = pain) %>% 
  arrange(Date) %>% 
  bind_rows(
    tibble(Date = seq.Date(from = min(mesh_pain$Date), to = max(mesh_pain$Date), by = unit)) %>% 
      mutate(type = "pelvic", `no pain threshold` = 0, `pain threshold` = 0)
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
  transmute(time = Date, nA = `pain threshold_pelvic`, nB = `no pain threshold_pelvic`, nC = `pain threshold_other`, nD = `no pain threshold_other`) %>% 
  as.data.frame()

x <- list(prr=list(),
          bcpnn = list()
          #xbar=list(ts_event=c(Count = "nA"))
          #poisson_rare=list(p_rate=0.3)
          )
algos <- define_algos(x)

run_algos(pain_table, algos)


prr(pain_table)
bcpnn(pain_table)
xbar(pain_table, ts_event = c(Count = "nA"))

xbar(pain_table %>% transmute(time, event = nA))

pain_table
