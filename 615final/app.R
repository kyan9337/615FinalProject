library(shinydashboard)
library(shiny)
library(leaflet)
library(tidyverse)
library(plotly)
# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Manhattan Property Sales"),
  dashboardSidebar(
    sidebarMenu(
      
      id = "sidebarmenu",
      menuItem("Welcome", tabName = "a", icon = icon("certificate",lib = "glyphicon")),
      menuItem("Exploration",
               tabName = "Exploration", icon = icon("venus"),
               menuItem("Map",
                        tabName = "map",
                        icon = icon("bed")
               ),
               menuItem("Visualization",
                        tabName = "vis",
                        icon = icon("bar-chart-o")
               ),
               menuItem("Data Table",
                        tabName = "data",
                        icon = icon("refresh")
               )
      ),
      menuItem("About", tabName = "about", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
    tabItem(
      tabName = "map",
      
      fluidRow(
        
        column(width = 12,
               box(width = NULL, solidHeader = TRUE,
                   leafletOutput("busmap", height = 500)
               ),
        
       column(width = 12,
              box(title = "Controls", width = NULL, status = "warning",
                  sliderInput("slider", "Sales Price Range:", 1, 100, value = c(40, 60)),
                  selectInput("Month","Select Month", 
                              choices = 1:12)
          
        )
        
       )
      )
    )
    ),#First Tab
    tabItem(
      tabName = "vis",
      
      fluidRow(
        box(title = "Heart Rate",status = "primary", solidHeader = TRUE,
            plotlyOutput("team"))
      )
    )
    
  )
  )
)
# Define server logic required to draw a histogram

server <- function(input, output) {
  output$busmap <- renderLeaflet({
    
  ASDF <- inner_join(Ne_sum, Manhattan_zip,by = "NEIGHBORHOOD.")
  ASDF <- distinct(ASDF,NEIGHBORHOOD.,.keep_all = TRUE)
  
  m <- leaflet(data =ASDF ) %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers( ~longitude,~latitude
                , popup = ~as.character(SALE.PRICE..x), label = ~as.character(NEIGHBORHOOD.))
  m
  })
  
  output$team <- renderPlotly({
  p <-  Midtown%>%
    filter(SALE.PRICE.<2210000000) %>% ggplot( aes(GROSS.SQUARE.FEET., SALE.PRICE., size = SALE.PRICE., color=NEIGHBORHOOD.)) +
    geom_point() +
    scale_y_continuous(labels = comma)+
    theme_bw()
  ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

