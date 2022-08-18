{
  library(shiny)
  library(shinydashboard)
  
  library(tidyverse)
  library(tidytext)
  library(ggridges)
  library(epitools)
  library(DT)
  
  reports <- read_csv("data/all_reports/all_reports_mached_manufacturer_df.csv")
  #anatomical_dictionary <- read_csv("data/anatomy/anatomical_dictionary.csv")
  anatomical_mapping <- read_csv("data/anatomy/anatomical_mapping.csv")
  anatomical_terms <- anatomical_mapping %>% 
    group_by(part) %>% 
    summarise() %>% 
    ungroup()
  
  
  fisher_test <- function(input_word, input_or){
    
    mesh_reports <- reports %>% 
      mutate(`GMDN term` = str_to_lower(`GMDN term`)) %>%
      filter(str_detect(`GMDN term`, input_word)) %>% 
      group_by(across(-c(X1))) %>% 
      summarise() %>% 
      ungroup()
    
    # Manufacturer summary
    man_counts <- mesh_reports %>% 
      group_by(mapped_name) %>% 
      summarise(n_reports = n()) %>% 
      arrange(-n_reports) %>% 
      mutate(n_reports_total = sum(n_reports))
    # Fisher exact test to determine if the odds ratio
    # is greater than 2 for anatomy word use in one
    # manufacturer to the others
    out <- mesh_reports %>% 
      unnest_tokens(word,`Event description`,drop = F) %>%
      mutate(is_anatomy = word %in% anatomical_terms$part) %>% 
      select(`Report number`,mapped_name, is_anatomy, word) %>% 
      filter(is_anatomy) %>% 
      group_by(mapped_name,word,`Report number`) %>% 
      summarise() %>% 
      group_by(mapped_name,word) %>% 
      summarise(count = n()) %>% 
      ungroup()%>% 
      full_join(man_counts, by = "mapped_name") %>% 
      transmute(Manufacturer = mapped_name,word, count, n_reports,n_reports_total) %>%
      filter(n_reports > 10) %>% 
      group_by(word) %>% 
      mutate(word_count_total = sum(count)) %>% 
      mutate(n_reports_other = n_reports_total-n_reports,
             word_count_other = word_count_total - count) %>% 
      drop_na() %>% 
      group_by(Manufacturer, word) %>% 
      nest() %>% 
      mutate(test = map(data, function(data){
        dat <- data.frame(
          "word_yes" = c(data$count, data$word_count_other),
          "word_no" = c(data$n_reports-data$count, data$n_reports_other-data$word_count_other),
          row.names = c("Manufacturer", "Others")
        )
        #print(dat)
        fisher.test(dat, alternative = "greater",or=input_or,conf.int = T) %>% 
          broom::tidy()
      })) %>% 
      unnest(test) %>% 
      select(Manufacturer,word,p.value) %>%
      filter(p.value < 0.05) %>% 
      arrange(p.value) %>% 
      mutate(p.value = signif(p.value,2))
    
    return(out)
    
  }
  
  
  get_reports <- function(input_word, input_word2){
    
    mesh_reports <- reports %>% 
      mutate(`GMDN term` = str_to_lower(`GMDN term`)) %>%
      filter(str_detect(`GMDN term`, input_word)) %>% 
      group_by(across(-c(X1))) %>% 
      summarise() %>% 
      ungroup()
    
    mesh_reports %>% 
      select(Manufacturer = mapped_name, `Event description`,`GMDN term`) %>% 
      filter(str_detect(`Event description`, input_word2)) %>% 
      return()
  }
  
  get_props <- function(input_word){
    mesh_reports <- reports %>% 
      mutate(`GMDN term` = str_to_lower(`GMDN term`)) %>%
      filter(str_detect(`GMDN term`, input_word)) %>% 
      group_by(across(-c(X1))) %>% 
      summarise() %>% 
      ungroup()
    
    # Manufacturer summary
    man_counts <- mesh_reports %>% 
      group_by(mapped_name) %>% 
      summarise(n_reports = n()) %>% 
      arrange(-n_reports) %>% 
      mutate(n_reports_total = sum(n_reports))
    
    mesh_reports %>% 
      unnest_tokens(word,`Event description`,drop = F) %>%
      mutate(is_anatomy = word %in% anatomical_terms$part) %>% 
      select(`Report number`,mapped_name, is_anatomy, word) %>% 
      filter(is_anatomy) %>% 
      group_by(mapped_name,word,`Report number`) %>% 
      summarise() %>% 
      group_by(mapped_name,word) %>% 
      summarise(count = n()) %>% 
      ungroup()%>% 
      full_join(man_counts, by = "mapped_name") %>% 
      transmute(Manufacturer = mapped_name,word, count, n_reports) %>%
      filter(n_reports > 10) %>% 
      mutate(count = signif(count/n_reports,2)) %>% 
      pivot_wider(names_from = "word", values_from = "count") %>%  
      mutate_all(function(x){ifelse(is.na(x),0,x)}) %>% 
      arrange(-n_reports) %>% 
      return()
  }
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Controls", tabName = "dashboard", icon = icon("dashboard")),
      textInput("input_word", "Medical Device Term:", value =  "mesh", width = "80%")
      
    )
  )
  
  box_info <- box(
    "If a particular manufacturer is having more problems than usual in a particular region of the body, that may be evidence that the device is malfunctioning. To see this, we can find the proportion of reports by a manufacturer that contain that particular term. We can test the hypothesis this proportion is the same as it is for other manufacturer using a Fisher Test. (Lots of zeros so a Chi-squared test wouldn't be appropriate)",
    height = "auto",width = "auto"
  )
  
  box_dt1 <- tabBox(
    height = "auto", width = "auto",
    tabPanel("Proportion of reports featuring word", dataTableOutput("table3")),
    tabPanel("Higher than usual", numericInput("input_or","Odds ratio",1), dataTableOutput("table1"))
  )
  
  box_dt2 <- box(
    title = "Find the corresponding reports",
    textInput("input_word2", "Anatomical Term:", value =  "breast", width = "80%"),
    side = "right", height = "auto", width = "auto",
    dataTableOutput("table2")
  )
  
  # box_dt3 <- box(
  #   side = "right", height = "auto", width = "auto",
  #   dataTableOutput("table3")
  # )
  
  body <- dashboardBody(
    titlePanel("Does this word appear more often than it should?"),
    fluidRow(
      box_info,
      box_dt1,
      box_dt2
      # box_dt3
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
    
    output$table1 <- renderDataTable(
      fisher_test(input_word = input$input_word,input_or = input$input_or),
      options = list(
        searching = F,
        searchHighlight = F,
        dom = 'Bfrtip',
        buttons = c('csv'),
        scrollX = TRUE
      )
      
    )
    output$table2 <- renderDataTable(
      
      get_reports(input_word = input$input_word,input_word2 = input$input_word2),
        filter = 'top',
      options = list(
        searching = T,
        searchHighlight = T,
        dom = 'Bfrtip',
        buttons = c('csv'),
        scrollX = TRUE

      )
    )
    output$table3 <- renderDataTable(
      
      get_props(input_word = input$input_word),
      options = list(
        searching = F,
        searchHighlight = F,
        dom = 'Bfrtip',
        buttons = c('csv'),
        scrollX = TRUE
      )
    )
  } 
  shinyApp(ui, server)
}


