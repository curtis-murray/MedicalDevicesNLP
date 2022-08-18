library(tidyverse)

clean_data <- read_csv("data/clean_data.csv")

#' Function to get data for disproportionality analysis
#' 
#' @param group_1 any vector combination of "pelvic_mesh", "hernia_mesh","other_mesh", "other_device".
#' @param group_2 any vector combination of "pelvic_mesh", "hernia_mesh","other_mesh", "other_device".
#' @param pain_type either "pain_word" which detects if the word pain appears in a doucment, or "pain_topic", which will detect the pain topic in a document.
#' @param topic_threshold if pain_type == "pain_topic" then this is the value that the topic density must exceed for the topic to be registered. The default value of 0.05 will count documents with at least 5% of words being from the pain topic.
#' @return Tibble of counts nA nB nC nD
#' @examples
#' get_data(group_1 = c("pelvic_mesh"), group_2 = c("hernia_mesh", "other_mesh"), pain_type = "pain_topic", topic_threshold = 0.05)
#' get_data(group_1 = c("pelvic_mesh"), group_2 = c("hernia_mesh"), pain_type = "pain_word", topic_threshold = 0.05)
get_data <- function(group_1, group_2, pain_type, topic_threshold = 0.05){
  
  intersection = intersect(group_1, group_2)
  if(length(intersection) > 0){
    warning(paste(intersection, " contained in both groups", sep = ""))
  }
  clean_data %>% 
    filter(type %in% c(group_1,group_2)) %>% 
    mutate(group = ifelse(type %in% group_1, "group_1", "group_2")) %>% 
    mutate(pain_topic = pain_topic >= topic_threshold) %>% 
    select(Report_ID, Date, group, pain = !!as.name(pain_type)) %>% 
    mutate(group_tf = (group == "group_1")) %>% 
    mutate(nA = 1*(group_tf & pain),
           nB = 1*(group_tf & !pain),
           nC = 1*(!group_tf & pain),
           nD = 1*(!group_tf & !pain)) %>% 
    select(-group_tf)
}

# These are the device groups and subgroups.
clean_data %>% group_by(type) %>% 
  summarise(count = n())

# Example 1:
# Use pelvic mesh as group 1 and all other mesh devices (including hernia) as group 2. 
# The value of interest is the pain topic, being above the threshold of 0.05. (i.e. 5% of the document contains words from the pain topic)
# You can adjust the topic threshold if you want to balance the groups more. 
# A higher topic_threshold will look for documents that discuss "pain" more, and hence find less pain documents.
get_data(group_1 = c("pelvic_mesh"), group_2 = c("hernia_mesh", "other_mesh"), pain_type = "pain_topic", topic_threshold = 0.05)

# Example 2:
# group 1 is pelvic mesh devices and the comparator is hernia mesh devices
# The value of interest is pain_word (i.e. the prensence of the word "pain" in the event description.)
get_data(group_1 = c("pelvic_mesh"), group_2 = c("hernia_mesh"), pain_type = "pain_word", topic_threshold = 0.05)
