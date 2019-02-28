library(shiny)
library(tidyverse)
library(shinythemes)
library(leaflet)
library(lubridate)
library(scales)
library(tmap)
library(sf)

# Read in data
gulls <- read_csv("seagulls_tiny.csv")

# Read in map information

gulls_sf <- st_as_sf(gulls, coords = c("longitude", "latitude"), crs = 4326)
ca <- read_sf(dsn = ".", layer = "california_county_shape_file") # Read data
st_crs(ca) = 4326 # Set CRS
tmap_mode("view")


####UI####
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("California Seagull Frequency and Distribution"),
  
  navbarPage("",
             
             tabPanel("Information",
                      #h1("A header!"),
                      h2("Gull information "),
                      h2("The tabs"),
                      p("The frequency plot will tell you the probability of finding a species given the parameters you set"),
                      p("The interactive map will allow you to see temportal and spacial changes for each species")
                      
             ),
        
             
             tabPanel("Frequency Plot", 
                      
                       
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("county", 
                                      "Select County",
                                      choices = NULL,
                                      multiple = FALSE),
                         
                          
                          checkboxGroupInput("name", "Exclude Species", choices = NULL)
                      
                        ),
                        
                        # Show a map of selected area 
                        mainPanel(
                          plotOutput("FreqPlot")
                        )
                      ))
             ,
             tabPanel("Map",
             
                      sidebarLayout(
                        sidebarPanel(
                         selectInput("common_name", 
                             "Select Species",
                             choices = NULL,
                             multiple = FALSE)
                 
                 
                 
               ),
               
               # Show a map of selected area 
               mainPanel(
                 leafletOutput("Map")
               )
             ))
             
             
  )
  
) 








####SERVER####
# Define server logic required to draw a histogram
server <- function(session, input, output) {
  
  observe({
    w <- gulls %>% 
      select(county)
    updateSelectInput(session, "county", "Select County", choices = unique(w))
  })
  
  
  observe({
    print(input$county)
    x <- gulls %>% 
      filter(county == input$county) %>% 
      pull(common_name)
    updateCheckboxGroupInput(session, "name", "Exclude Species", choices = unique(x))
  })
  

gulls_final <- reactive({ 
  
  gulls %>% 
    filter(
      county == input$county &
        !common_name %in% input$name
    )  %>% 
    group_by(common_name) %>% 
    summarize(
      n = sum(observation_count)
    ) %>% 
    mutate(
      total = sum(n),
      prop = n / total
    ) %>% 
    arrange(-prop) %>% 
    mutate(common_name = factor(common_name, levels = common_name))
  
  })


output$FreqPlot <- renderPlot({
 
  ggplot(data = gulls_final(), aes(x = common_name, y = prop)) +
    geom_col() +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(labels=percent_format(),
                       expand = c(0,0)) +
    theme_bw() +
    coord_flip()
  
})


observe({
  m <- gulls %>% 
    select(common_name)
  updateSelectInput(session, "common_name", "Select Species", choices = unique(m))
})


output$Map <- renderLeaflet({
  
  tm_shape(ca) +
  tm_polygons()
  
})    
    
  }

  

# Run the application 
shinyApp(ui = ui, server = server)

