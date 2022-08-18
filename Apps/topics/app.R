{
  library(shiny)
  library(shinydashboard)
  
  library(tidyverse)
  library(tidytext)
  library(ggridges)
  library(DT)
  library(wordcloud)
  library(zoo)
  library(lubridate)
  
  tidy_topics <- read_csv("data/hSBM/Clean/tidy_topics.csv")
  
  tidy_topics_docs <- read_csv("data/hSBM/Clean/tidy_topics_docs.csv")
  
  docs_all <- read_csv("data/hSBM/docs_all.csv") %>% 
    transmute(doc_ID = X1+1, `Report number` = `0`)
  
  train <- read_csv("data/hSBM/train.csv")
  
  docs_metadata <- docs_all %>% left_join(train, by = "Report number")
  
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Controls", tabName = "dashboard", icon = icon("dashboard")),
      textInput("focus_word", "Focus word:", value = "pain"),
      numericInput("topic_level", "Topic Level:", min = 0, max = 4, value = 0),
      textInput("focus_term", "Medical Device Term:", value =  "mesh", width = "80%"),
      numericInput("window", "Window for moving Ave:", value = 7)
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
      box_info,
      box_wc,
      box_dens,
      box_term_topic_density
      #box_network
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
    # 
    # output$topic_network <- renderVisNetwork({
    #   visIgraph(my_graph,
    #             layout="layout_as_tree",
    #             circular = T,
    #             #root=edges_id_full$from[2],
    #             idToLabel = F) %>%
    #     visOptions(highlightNearest = list(enabled = T, hover = T),
    #                nodesIdSelection = T)
    # })
    
    output$topic_density <- renderPlot({
      
      relevant_topic <- tidy_topics %>% 
        filter(word == input$focus_word,
               Level == input$topic_level) %>% 
        pull(topic)
      
      plot_data <- tidy_topics_docs %>% 
        filter(Level == input$topic_level,
               topic == relevant_topic) %>% 
        select((-c(Level, topic))) %>% 
        left_join(docs_metadata %>% select(doc_ID, `GMDN term`, `Report date`), by = "doc_ID") %>% 
        filter(str_detect(`GMDN term`, paste("(?i)",input$focus_term,sep=""))) %>% 
        group_by(`Report date`) %>% 
        summarise(p = mean(p))
      
      full_join(plot_data,
                tibble(`Report date` = seq(min(plot_data$`Report date`),max(plot_data$`Report date`), by = "1 day"))) %>% 
        arrange(`Report date`) %>% 
        mutate(y = rollapply(p, input$window, na.pad=TRUE, function(x) mean(x,na.rm = T))) %>% 
        filter(!is.na(y)) %>% 
        ggplot(aes(x = `Report date`, y = p)) +
        #geom_point() + 
        geom_line(aes(x = `Report date`, y = y)) +
        theme_minimal() + 
        labs(x = "Report date", title = paste("Topic density (",input$window," day moving average)", sep = ""),y = "Topic density") + 
        lims(x = c(min(docs_metadata$`Report date`),max(docs_metadata$`Report date`)))
      
    })
    
    output$report_density <- renderPlot({
      
      docs_metadata %>% select(doc_ID, `GMDN term`, `Report date`) %>% 
        filter(str_detect(`GMDN term`, paste("(?i)",input$focus_term,sep=""))) %>% 
        select(`Report date`) %>% 
        mutate(is_a_doc = 1) %>% 
        bind_rows(
          tibble(`Report date` = seq(min(docs_metadata$`Report date`),max(docs_metadata$`Report date`), by = "1 day")) %>% 
            mutate(is_a_doc = 0)) %>% 
        #mutate(`Report date` = round_date(`Report date`, unit = paste(input$window, "days")) %>% 
        group_by(`Report date`) %>% 
        summarise(count = sum(is_a_doc)) %>% 
        ggplot(aes(x = `Report date`, y = rollmean(count, input$window, na.pad = TRUE))) +
        geom_line() + 
        theme_minimal() + 
        labs(x = "Report date", title = paste("Reports per day (",input$window," day moving average)", sep = ""), y = "Posts") + 
        lims(x = c(min(docs_metadata$`Report date`),max(docs_metadata$`Report date`)))
      
    })
    
    output$term_topic_density <- renderDataTable(
      (function(){
        relevant_topic <- tidy_topics %>% 
          filter(word == input$focus_word,
                 Level == input$topic_level) %>% 
          pull(topic)
        
        data <- docs_metadata %>% 
          select(doc_ID, `Report number`, `Report date`, `GMDN term`,`content`) %>% 
          mutate(`GMDN term` = str_to_lower(`GMDN term`)) %>%
          mutate(`GMDN term` = str_split(`GMDN term`, ',')) %>% 
          unnest(`GMDN term`) %>% 
          mutate(`GMDN term` = `GMDN term` %>% str_squish()) %>% 
          group_by(`GMDN term`) %>% 
          ungroup() %>% 
          left_join(
            tidy_topics_docs %>% 
              filter(Level == input$topic_level, topic == relevant_topic) %>% 
              select(doc_ID, p),
            by = "doc_ID"
          ) %>% 
          arrange(-p) %>% 
          group_by(`GMDN term`) %>% 
          summarise(p = mean(p),
                    count = n()) %>% 
          arrange(-p) %>% 
          transmute(`GMDN term`, `Topic Density` = signif(p,3), count)
        
        return(data)})(),
      filter = 'top',
      options = list(
        searching = T,
        searchHighlight = T,
        dom = 'Bfrtip',
        buttons = c('csv'),
        scrollX = TRUE
      )
    )
  }
  
  shinyApp(ui, server)
}
