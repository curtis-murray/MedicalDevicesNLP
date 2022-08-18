{
  library(shiny)
  library(shinydashboard)
  
  library(tidyverse)
  library(tidytext)
  library(visNetwork)
  library(igraph)
  library(DT)
  
  reports <- read_csv("data/all_reports/all_reports_mached_manufacturer_df.csv")
  all_manufacturers <- read_csv("data/companies/all_manufacturers.csv")
  similar_companies <- read_csv(file = "data/companies/similar_companies.csv") 
  
  gen_graph <- function(input_threshold){
    
    same_companies <- similar_companies %>% filter(d <= input_threshold)
    
    company_network <- same_companies %>% bind_rows(same_companies %>% 
                                                      mutate(Manufacturer = Manufacturer.y,
                                                             Manufacturer.y = Manufacturer.x,
                                                             Manufacturer.x = Manufacturer) %>%
                                                      select(-Manufacturer)) %>% 
      group_by_all() %>% 
      summarise() %>% #Removes duplicates
      ungroup() %>% 
      #select(from = Manufacturer.x, to = Manufacturer.y) %>% 
      left_join(all_manufacturers %>% select(Manufacturer, from = Manufacturer_ID), 
                by = c("Manufacturer.x" = "Manufacturer")) %>%
      left_join(all_manufacturers %>% select(Manufacturer, to = Manufacturer_ID),
                by = c("Manufacturer.y" = "Manufacturer"))
    
    
    edges <- company_network %>% 
      transmute(from, to, size = 1-d,
                #    length = d
      ) %>% 
      filter(from != to)
    
    nodes <- all_manufacturers %>% 
      transmute(id = Manufacturer_ID,
                label = Manufacturer,
                size = count %>% as.numeric()) %>% 
      mutate(size = size /5,
             font.size = 10*sqrt(size)) %>% 
      filter(id %in% company_network$from | id %in% company_network$to) %>% 
      ungroup() %>% 
      select(id, label, size, font.size)
    
    my_graph <- graph_from_data_frame(edges,
                                      directed = FALSE, 
                                      vertices = nodes
    )
    
    visIgraph(my_graph,idToLabel = F,physics = T) %>% 
      visOptions(highlightNearest = list(enabled = T, hover = T),
                 nodesIdSelection = T,
                 height = 1440, 
                 width = 2560) %>% 
      visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -50,
                                                                      springLength = 100,
                                                                      damping = 1))
  }
  
  gen_graph_hist <- function(input_threshold){
    
    same_companies <- similar_companies %>% filter(d <= input_threshold)
    
    company_network <- same_companies %>% bind_rows(same_companies %>% 
                                                      mutate(Manufacturer = Manufacturer.y,
                                                             Manufacturer.y = Manufacturer.x,
                                                             Manufacturer.x = Manufacturer) %>%
                                                      select(-Manufacturer)) %>% 
      group_by_all() %>% 
      summarise() %>% #Removes duplicates
      ungroup() %>% 
      #select(from = Manufacturer.x, to = Manufacturer.y) %>% 
      left_join(all_manufacturers %>% select(Manufacturer, from = Manufacturer_ID), 
                by = c("Manufacturer.x" = "Manufacturer")) %>%
      left_join(all_manufacturers %>% select(Manufacturer, to = Manufacturer_ID),
                by = c("Manufacturer.y" = "Manufacturer"))
    
    
    edges <- company_network %>% 
      transmute(from, to, size = 1-d,
                #    length = d
      ) %>% 
      filter(from != to)
    
    nodes <- all_manufacturers %>% 
      transmute(id = Manufacturer_ID,
                label = Manufacturer,
                size = count %>% as.numeric()) %>% 
      mutate(size = size /5,
             font.size = 10*sqrt(size)) %>% 
      filter(id %in% company_network$from | id %in% company_network$to) %>% 
      ungroup() %>% 
      select(id, label, size, font.size)
    
    my_graph <- graph_from_data_frame(edges,
                                      directed = FALSE, 
                                      vertices = nodes
    )
    
    comp <- my_graph %>% 
      components()
    
    get_component_name <- function(ids){
      nodes %>% filter(id %in% ids) %>% 
        filter(size == max(size)) %>% 
        filter(str_length(label) == min(str_length(label))) %>%
        pull(label) %>% .[1]
    }
    
    get_component_names <- function(ids){
      names <- all_manufacturers %>% filter(Manufacturer_ID %in% ids) %>% 
        pull(simple_manufacturer)
      Reduce(intersect, str_split(names," ")) %>% 
        paste(collapse = " ")
    }
    
    name_mapping <- tibble(component = groups(comp)) %>% 
      mutate(mapped_name = map(component, get_component_name)) %>% 
      unnest(mapped_name) %>% 
      unnest(component) %>% 
      transmute(Manufacturer_ID = component %>% as.numeric(), mapped_name)

    all_manufacturers_named <- all_manufacturers %>%
      left_join(name_mapping,  by = "Manufacturer_ID") %>%
      group_by(mapped_name) %>%
      summarise(count = sum(count)) %>%
      left_join(all_manufacturers %>% select(Manufacturer, Manufacturer_ID),
                by = c("mapped_name" = "Manufacturer")) %>%
      arrange(-count) %>%
      transmute(Manufacturer = mapped_name, count, Manufacturer_ID)
    # 
    # write_csv(all_manufacturers_named, "data/companies/all_manufacturers_named.csv")
    
    # reports %>% left_join(all_manufacturers, by = "Manufacturer") %>% 
    #   left_join(name_mapping, by = "Manufacturer_ID") %>% 
    #   write_csv("data/all_reports/all_reports_mached_manufacturer_df.csv")
    
    all_manufacturers_named %>% 
      top_n(50, count) %>% 
      ggplot() + 
      geom_histogram(aes(x = reorder(stringr::str_wrap(Manufacturer, 60), count), y = count), stat = "identity") + 
      coord_flip() + 
      theme_minimal() + 
      labs(x = "Manufacturer", y = "Number of Products",
           title = "Number of Products by Manufacturer")
    
  }
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Controls", tabName = "dashboard", icon = icon("dashboard"))
    ),
    numericInput(inputId = "input_threshold",label = "Threshold distance",
                min = 0,max = 2,value = 0.1,width = "100%")
  )
  
  box_plot <- box(
    side = "right", height = "auto", width = "auto",
    visNetworkOutput("plot", height = "auto")
    
  )
  box_plot_hist <- box(
    side = "right", height = "auto", width = "auto",
    plotOutput("plot_hist",height = "1080px"),
  )
  
  box_dt <- box(
    side = "right", height = "auto", widht = "auto",
    DTOutput("table")
  )
  
  body <- dashboardBody(
    titlePanel("Manufacturer Name Matching"),
    fluidRow(
      box_plot,
      box_plot_hist,
      box_dt
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
        input_threshold = input$input_threshold
      )
    })
    
    output$plot_hist <- renderPlot({
      gen_graph_hist(
        input_threshold = input$input_threshold
      )
    })
    
    output$table <- renderDataTable(
      
      datatable(
        similar_companies %>%     
          transmute(Manufacturer.x, Manufacturer.y, Dissimilarity = d) %>% 
          top_n(10000,-Dissimilarity), 
        extensions = 'Buttons',
        filter = 'top',
        options = list(
          searching = TRUE,
          searchHighlight = TRUE,
          dom = 'Bfrtip',
          buttons = c('csv'),
          order = list(list(3, 'asc'))
        ))
    )
  } 
  shinyApp(ui, server)
}

