library(shinydashboard)
library(shiny)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
if (!require(plotly)) install.packages('plotly')
library(plotly)
if (!require(scales)) install.packages('scales')
library(scales)
if (!require(zipcode)) install.packages('zipcode')
library(zipcode)
if (!require(devtools)) install.packages('devtools')
library(devtools)
if (!require(mapdata)) install.packages('mapdata')
library(mapdata)
if (!require(ggmap)) install.packages('ggmap')
library(ggmap)
if (!require(benford.analysis)) install.packages('benford.analysis')
library(benford.analysis)
if (!require(leaflet)) install.packages('leaflet')
library(leaflet)
Manhattan <- read.csv("2017_manhattan.csv") %>% 
  select(2,9,11,12,13,15,16,17,19,20,21)
### Convert data format into correct format
Manhattan$SALE.PRICE. <- as.numeric(gsub(",","",as.character(Manhattan$SALE.PRICE.),fixed=TRUE)) #Convert format to numeric for price 
Manhattan$LAND.SQUARE.FEET. <- as.numeric(gsub(",","",as.character(Manhattan$LAND.SQUARE.FEET.),fixed=TRUE))#Convert format to numeric for land sqft
Manhattan$GROSS.SQUARE.FEET. <- as.numeric(gsub(",","",as.character(Manhattan$GROSS.SQUARE.FEET.),fixed=TRUE))#Convert format to numeric for gross sqft
Manhattan$SALE.DATE. <- as.Date(Manhattan$SALE.DATE.,"%m/%d/%Y")#conver date format
M <- data.frame(Manhattan$SALE.DATE.,
                year = as.numeric(format(Manhattan$SALE.DATE., format = "%Y")),
                month = as.numeric(format(Manhattan$SALE.DATE., format = "%m")),
                day = as.numeric(format(Manhattan$SALE.DATE., format = "%d"))) %>% 
  select(3)
Manhattan <- cbind(Manhattan,M)
colnames(Manhattan)[12] <- "Month"
Manhattan$Month <- month.abb[Manhattan$Month]
#Create a new column indicate Sale Month
Manhattan <- filter(Manhattan,YEAR.BUILT.>1800)
Manhattan$YEAR.BUILT.  <- ifelse(Manhattan$YEAR.BUILT.  == "0", NA, Manhattan$YEAR.BUILT) 
Manhattan$YEAR.BUILT. <- as.Date(as.character(Manhattan$YEAR.BUILT.), format = "%Y")
Manhattan$ZIP.CODE. <- as.character(Manhattan$ZIP.CODE.)
Manhattan$BUILDING.CLASS.AT.TIME.OF.SALE. <- as.character(Manhattan$BUILDING.CLASS.AT.TIME.OF.SALE.)
Manhattan$RESIDENTIAL.UNITS. <- as.numeric(as.character(Manhattan$RESIDENTIAL.UNITS.))
data("zipcode")
colnames(Manhattan)[3] <- "zip"
Manhattan$zip <- as.character(Manhattan$zip)
Manhattan_zip <- inner_join(Manhattan,zipcode,by="zip")
Manhattan_zip$latitude <- as.numeric(Manhattan_zip$latitude)
Manhattan_zip$longitude <- as.numeric(Manhattan_zip$longitude)

Ne_list<- c(unique(as.character(Manhattan$NEIGHBORHOOD.)))
month_list <- c(unique(as.character(Manhattan$Month)))
# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Manhattan Property Sales"),
  dashboardSidebar(
    sidebarMenu(
      
      id = "sidebarmenu",
      menuItem("Welcome", tabName = "a", icon = icon("gift",lib = "glyphicon")),
      menuItem("Exploration",
               tabName = "Exploration", icon = icon("bomb"),
               menuItem("Map",
                        tabName = "map",
                        icon = icon("map-marker",lib = "glyphicon")
               ),
               menuItem("Visualization",
                        tabName = "vis",
                        icon = icon("thumbs-up", lib = "glyphicon")
               ),
               menuItem("Data Table",
                        tabName = "data",
                        icon = icon("bar-chart-o")
               ),
               menuItem("Benford Analysis",
                        tabName = "bfd",
                        icon = icon("thumbs-down", lib = "glyphicon")
               )
      ),
      menuItem("About", tabName = "about", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "a",
        
        fluidRow( column(6,
                         
                         box(width=11, solidHeader = TRUE, status = "success",
                             title="Overview",
                             h4("In Explorationtab, users can be directed to:",style = "font-family: 'Arial'," ),
                             h5("  1) Map"),
                             h5("  2) Visualization"),
                             h5("  3) Data Table"),
                             h5("  4) Benford Analysis")
                         ))
                  
               
                         
                  ),mainPanel(imageOutput("row1"))
        )
    
       

        
      ,
    tabItem(
      tabName = "map",
      
      fluidRow(
        
        column(width = 12,
               box(title = "Total sale price based on location",width = NULL, solidHeader = TRUE,
                   leafletOutput("busmap", height = 500)
               ),
        
        column(width = 12,
              box(title = "Controls", width = NULL, status = "warning",
                  "Select date only within year 2017",
                  dateRangeInput("dateRange1",
                                 label = "Select Date",
                                 start = "2017-01-01", end = "2017-12-31"
                                ),
                  "Total sale price during selected date range",
                  sliderInput("slider1", "Sales Price Range:", 1, 4000000000, value = c(600000000, 1500000000))
                  # selectInput("Month","Select Month", 
                  #             choices = 1:12),
                  
                  )
        
              )
      )
    )
    ),#First Tab
    tabItem(
      tabName = "vis",
      
      fluidRow(
        tabBox(
          height = 400 ,
          tabPanel("Total Sales for Each Neighborhood", plotlyOutput("plot1"), width = 200, height = 500),
          tabPanel("Sales Price Info", plotlyOutput("plot2",height = 800), width = 200, height =800),
          tabPanel("More Info", plotlyOutput("plot3",height = 500),plotlyOutput("plot4",height = 500),width = 200, height =1000)
        ),
        column(width = 6,
               box(title = "Controls for Sales Info",width = NULL, solidHeader = TRUE,
                   selectInput("var", "Total sale price for:",
                                c("Month" = "Month",
                                  "NEIGHBORHOOD." = "NEIGHBORHOOD.",
                                  "Building Class" = "BUILDING.CLASS.AT.TIME.OF.SALE.")
                                ),
                   radioButtons("fun","Interested in:",
                                c("median","mean","sum"))
                  ),
               box(title = "Controls for More Info",width = NULL, solidHeader = TRUE,
                   checkboxGroupInput("nei","Neighborhoods to shows:",
                                      choices = Ne_list, selected = c("MIDTOWN CBD","MIDTOWN EAST","MIDTOWN WEST","CHINATOWN")))
               )
      )
    ),
    tabItem(
      tabName = "data",
      fluidRow(
        
        
        box(title = "Original Dataset", status = "success", collapsible = TRUE,
            solidHeader = TRUE, DT::dataTableOutput("table"), width = 12, height = 600)
      
        
        
      ),
      box(
        title = "Select Date", status = "success", solidHeader = TRUE,
        selectInput("selectmonth","Choose Month", choices = month_list )
      )
    ),
    tabItem(
      tabName = "bfd",
      plotOutput("plot5",height = 600)
    ),
    tabItem(
      tabName = "about",
      h3("This app includes data from NewYork Department of Finance.The dataset is Neighborhood Sales Data 
of Manhattan through 2017 .The primary purpose of this app is to Visualize the dataset better and 
         present the result of benford analysis for this dataset.",size = 10,style = "font-family: 'Arial'," ),
      h3("This shiny app developed by Mark Yan."),
      br(),
      br(),
      valueBoxOutput("userguide")
    )
  )
  )
)
# Define server logic required to draw a histogram

server <- function(input, output) {
  output$busmap <- renderLeaflet({
    
    Data1  <- Manhattan_zip[Manhattan_zip$SALE.DATE.>=input$dateRange1[1] & Manhattan_zip$SALE.DATE. <= input$dateRange1[2],]
    
    map_sum <- aggregate(SALE.PRICE.~NEIGHBORHOOD.,data=Data1,sum)
    draw <- inner_join(map_sum,Manhattan_zip,by = "NEIGHBORHOOD.")
    draw <- distinct(draw,NEIGHBORHOOD.,.keep_all = TRUE)
    map_data <- draw[draw$SALE.PRICE..x>= input$slider1[1] & draw$SALE.PRICE..x <= input$slider1[2],]
  
  m <- leaflet(data =map_data ) %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers( ~longitude,~latitude
                , popup = ~as.character(SALE.PRICE..x), label = ~as.character(NEIGHBORHOOD.))
  m
  })
  
  output$plot1 <- renderPlotly({
  p <- ggplot(Manhattan)+
    geom_bar(mapping = aes(x = NEIGHBORHOOD.))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggplotly(p)
  })
  
  output$plot2 <- renderPlotly({
  pp <- ggplot(data = Manhattan,
               aes_string(x=input$var, y = "SALE.PRICE.")) +
    scale_y_continuous(labels = comma)+
    stat_summary(fun.y = input$fun, # adds up all observations for the month
                 geom = "bar") + # or "line"
    # custom x-axis labels
    theme(axis.text.x = element_text(angle =60, hjust = 1))
    ggplotly(pp)
  })
    
  output$plot3 <- renderPlotly({
    Data_2 <- filter(Manhattan, Manhattan$NEIGHBORHOOD.== input$nei)
    B <- ggplot(data = Data_2,aes(x=log(SALE.PRICE.),fill = NEIGHBORHOOD.))+
      geom_histogram()
    ggplotly(B)
  })
  
  output$plot4 <- renderPlotly({
    Data_2 <- filter(Manhattan, Manhattan$NEIGHBORHOOD.== input$nei)
    C <- ggplot(data = Data_2,
           aes(Month, fill = NEIGHBORHOOD.)) +
      theme(axis.text.x = element_text(angle =60, hjust = 1))+
      geom_bar(position = "dodge")
    ggplotly(C)
  })
  
  output$table<- DT::renderDataTable({
    tabledata <- Manhattan[Manhattan$Month == input$selectmonth,]
    DT::datatable(tabledata, options = list(searching = TRUE,pageLength = 50,lengthMenu = c( 50, 100, 500), scrollX = T,scrollY = "300px"),rownames= FALSE
    )
  })
  
  output$plot5 <- renderPlot({
    bfd <- benford(Manhattan$SALE.PRICE.)
    plot(bfd)
  })
  
  output$userguide <- renderUI({
    url <- a("Webpage", href="https://www1.nyc.gov/site/finance/taxes/property-annualized-sales-update.page")
    
    
    tagList("You can find the data from this", url)
  })
  
  output$row1<- renderImage({
    Leg<-"www/picture1.png"
    list(src=Leg)
  },deleteFile = FALSE)  
  
 
}

# Run the application 
shinyApp(ui = ui, server = server)

