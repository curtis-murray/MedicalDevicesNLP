{
  library(shiny)
  library(shinydashboard)
  
  library(tidyverse)
  library(tidytext)
  library(visNetwork)
  library(igraph)
  library(DT)
  
  reports <- read_csv("data/all_reports/all_reports_mached_manufacturer_df.csv")
  anatomical_dictionary <- read_csv("data/anatomy/anatomical_dictionary.csv")
  anatomical_mapping <- read_csv("data/anatomy/anatomical_mapping.csv")
  connections <- read_csv("data/anatomy/connections.csv")
  
  
  
  get_word_adv <- function(desc){
    c(
      str_extract_all(desc,paste("\\b",anatomical_mapping$same,"\\b",sep ="")),
      gsub(
        pattern = '.{2}$',
        replacement =  '',
        str_extract_all(desc,paste("\\b",anatomical_mapping$same,"es\\b",sep ="")) %>% 
          unlist()
      ),
      gsub(
        pattern = '.{1}$',
        replacement =  '',
        str_extract_all(desc,paste("\\b",anatomical_mapping$same,"s\\b",sep ="")) %>% 
          unlist()
      ),
      gsub(
        pattern = '.{2}$',
        replacement =  '',
        str_extract_all(desc,paste("\\b",anatomical_mapping$same,"'s\\b",sep ="")) %>% 
          unlist()
      ),
      gsub(
        pattern = '.{3}$',
        replacement =  '',
        str_extract_all(desc,paste("\\b",gsub(
          pattern = '.{1}$',
          replacement =  '',
          anatomical_mapping$same
        ),"'ies\\b",sep ="")) %>% 
          unlist()
      ) %>% paste("y",sep ="")
    )
  }
  
  gen_graph <- function(input_word){
    
    
    mesh_reports <- reports %>% 
      mutate(`GMDN term` = str_to_lower(`GMDN term`)) %>%
      filter(str_detect(`GMDN term`, input_word)) %>% 
      group_by(across(-c(X1))) %>% 
      summarise()
    
    pain_locations <- mesh_reports %>%
      ungroup() %>% 
      select(`Event description`,`Report number`) %>% 
      #filter(`Report number` == 55616) %>% 
      unnest_tokens(sentence, `Event description`,token = "sentences") %>% 
      filter(str_detect(sentence, "pain")) %>% 
      mutate(location = map(sentence, get_word_adv)) %>% 
      unnest(location) %>% 
      unnest(location) %>% 
      filter(!(location %in% c("character(","character(0","charactery"))) %>% 
      filter(!(location %in% c("","y"))) %>% 
      left_join(anatomical_mapping %>% select(part, same), by = c("location"="same")) %>% 
      transmute(location = part,`Report number`,sentence) %>% 
      group_by(`Report number`,location,sentence) %>% 
      summarise() %>% 
      group_by(location) %>% 
      summarise(count = n()) %>% 
      arrange(-count) %>% 
      ungroup() %>% 
      bind_rows(tibble(location = connections$from, 
                       count = 0)) %>% 
      bind_rows(tibble(location =connections$to,
                       count = 0)) %>% 
      group_by(location) %>% 
      summarise(count = sum(count))
    
    pain_connections <- connections %>% 
      filter(to %in% pain_locations$location) %>% 
      filter(from %in% pain_locations$location)
    
    
    my_graph <- graph_from_data_frame(connections, 
                                      vertices = pain_locations %>% 
                                        mutate(size = ifelse(count > 0, count+10,5)*2) %>%
                                        mutate(size = size/sum(size)*2500) %>% 
                                        mutate(font.size = size+10) %>% 
                                        mutate(color = ifelse(count == 0,"#8ecae6", "#031D44")),
                                      directed = TRUE)
    
    
    
    visIgraph(my_graph, 
              idToLabel = T,
              physics = T) %>% 
      visOptions(highlightNearest = list(enabled = T, hover = T), 
                 nodesIdSelection = T,height = 1440, width = 2560) %>% 
      visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -200,
                                                                      springLength = 100,
                                                                      damping = 1))
  }
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Controls", tabName = "dashboard", icon = icon("dashboard"))
    ),
    textInput("input_word", "Word:", value =  "mesh", width = "80%")
  )
  
  box_plot <- box(
    side = "right", height = "auto", width = "auto",
    visNetworkOutput("plot", height = "auto"),
    DTOutput("table")
    
  )
  
  body <- dashboardBody(
    titlePanel("Pain levels for input device in anatomical network"),
    fluidRow(
      box_plot
    )
  )
  
  ui <- dashboardPage(
    dashboardHeader(title = "Pain Network"),
    sidebar,
    body,
    skin = "purple"
  )
  
  server <- function(input, output) {
    
    output$menuitem <- renderMenu({
      menuItem("Menu item", icon = icon("calendar"))
    })
    
    output$plot <- renderVisNetwork({
      gen_graph(
        input_word = input$input_word      
      )
    })
    
    output$table <- renderDataTable(
      reports %>% 
        mutate(`GMDN term` = str_to_lower(`GMDN term`)) %>%
        mutate(`GMDN term` = str_split(`GMDN term`, ',')) %>% 
        unnest(`GMDN term`) %>% 
        mutate(`GMDN term` = `GMDN term` %>% str_squish()) %>% 
        group_by(`GMDN term`) %>% 
        summarise(count = n()) %>% 
        arrange(-count) %>% 
        datatable(class = "compact"),
      options = list(scrollX=TRUE)
    )
  } 
  shinyApp(ui, server)
}

