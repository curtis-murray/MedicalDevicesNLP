library(tidyverse)
library(lubridate)
library(zoo)

data <- read_csv("data/all_reports/all_reports_mached_manufacturer_df.csv")



pain <-  data %>% 
  mutate(`GMDN term` = str_squish(str_to_lower(`GMDN term`))) %>%
  filter(str_detect(`GMDN term`, "mesh")) %>% 
  mutate(pain_level = str_count(`Event description`,"(?i)pain")/(str_count(`Event description`," ")+1)) %>% 
  mutate(date = round_date(`Report date`, unit = "day"))

pain <- pain %>% 
  full_join(tibble(date = seq(min(pain$date),max(pain$date), by = "1 day")))

pain %>% 
  ggplot() +
  geom_line(aes(x = date, y = rollmean(pain_level, 7, na.pad=TRUE))) +
  #geom_point(aes(date, pain_present)) + 
  #geom_line(aes(month, pain_level)) + 
  #geom_line(aes(month, count), linetype = "dotted") + 
  geom_smooth(aes(date, pain_level)) + 
  theme_minimal()
