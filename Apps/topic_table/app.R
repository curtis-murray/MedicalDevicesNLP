{
  library(shiny)
  library(shinydashboard)
  
  library(tidyverse)
  library(tidytext)
  library(DT)
  library(wordcloud)
  library(zoo)
  library(lubridate)
  
  data <- read_csv("data/all_reports_mached_manufacturer_df.csv")
  
  tidy_topics <- read_csv("data/tidy_topics.csv")
  
  tidy_topics_docs <- read_csv("data/tidy_topics_docs.csv")
  
  docs_all <- read_csv("data/docs_all.csv") %>% 
    transmute(doc_ID = X1+1, `Report number` = `0`)
  
  train <- read_csv("data/train.csv")

  docs_metadata <- docs_all %>% left_join(train, by = "Report number")
  
  join_data <- data %>% 
    mutate(`GMDN term` = str_to_lower(`GMDN term`)) %>%
    filter() %>% 
    mutate(hernia = (str_detect(`Event description`, "(?i)hernia") & str_detect(`GMDN term`, "mesh")),
           pelvic = (str_detect(`Event description`, "(?i)((pelvi)|(urina)|(incont)|(vagina)|(uterus)|(anterior)|(posterior))") & str_detect(`GMDN term`, "mesh"))) %>% 
    full_join(tidy_topics_docs, by = c(`Report number` = "document"))
  
  rm(docs_all)
  rm(train)
  gc()
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Controls", tabName = "dashboard", icon = icon("dashboard")),
      textInput("focus_term", "Medical Device Term:", value =  "mesh", width = "80%"),
      textInput("focus_word", "Focus word:", value = "pain"),
      numericInput("topic_level", "Topic Level:", min = 0, max = 4, value = 0),
      numericInput("window", "Window for moving Ave:", value = 31)
    )
  )
  
  box_info <- box(
    "Selecting a focus word shows the topic that it belongs in. The topic level 0 is the deepest level in the topic heirarchy, showing the most relevant words. As the topic level increases the topics become more specific. A medical device term can be selected to filter down to specific types of medical devices (This is the GMDN term in the table). From this category, we look at our topic's density againt report dates.",
    height = "auto",width = "auto"
  )
  
  # box_network <- 
  #   tabBox(
  #     height = "auto", width = "auto",
  #     tabPanel("Topic Network", visNetworkOutput("topic_network"))
  #   )
  
  box_wc <- tabBox(
    height = "auto", width ="auto",
    tabPanel("Topic Wordcloud", plotOutput("wc_from_input"))
  )
  
  box_topic_table <- tabBox(
    height = "auto", width ="auto",
    tabPanel("Topic Table", dataTableOutput("topic_table"))
  )  
  
  box_dens <- tabBox(
    height = "auto",width = "auto",
    tabPanel("Topic Density", plotOutput("topic_density")),
    tabPanel("Posts per day", plotOutput("report_density"))
  )
  box_term_topic_density <- tabBox(
    height = "auto",width = "auto",
    tabPanel("Mean Topic Densities for Medical Devices", dataTableOutput("term_topic_density"))
  )
  
  body <- dashboardBody(
    titlePanel("Medical device topics"),
    fluidRow(
      box_wc,
      box_topic_table
    )
  )
  
  ui <- dashboardPage(
    dashboardHeader(title = "Medical Device"),
    sidebar,
    body,
    skin = "purple"
  )
  
  server <- function(input, output) {
    
    output$menuitem <- renderMenu({
      menuItem("Menu item", icon = icon("calendar"))
    })
    
    output$wc_from_input <- renderPlot({
      
      if(input$topic_level == -1){
        return()
      }
      
      relevant_topic <- tidy_topics %>% 
        filter(word == input$focus_word,
               Level == input$topic_level) %>% 
        pull(topic)
      
      wc_data <- tidy_topics %>% 
        filter(topic == relevant_topic, 
               Level == input$topic_level) 
      
      # wordcloud(words = wc_data$word, freq = wc_data$p, min.freq = 0,
      #           max.words=200, random.order=FALSE, rot.per=0,
      #           colors=brewer.pal(8, "Dark2"))
      
      wc_data %>% 
        mutate(color = ifelse(word == input$focus_word, 1,0)) %>% 
        ggplot(aes(label = word, size = p*10, color = color)) + 
        ggwordcloud::geom_text_wordcloud() + 
        scale_size_area(max_size = 20) + 
        theme_minimal()
      
    })
    
    output$topic_table <- renderDataTable(
      (function(){
        
        focus_topic <- tidy_topics %>% 
          filter(Level == input$topic_level,
                 word == input$focus_word) %>% 
          pull(topic)
        
      out_data <- join_data %>% 
        filter(Level == input$topic_level,
               topic == focus_topic) %>% 
        select(`GMDN term`, `Event description`, p)
      
      return(out_data)})(),
    )
  }
  
  shinyApp(ui, server)
}
