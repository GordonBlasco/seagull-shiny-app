library(shiny)
library(tidyverse)
library(shinythemes)
library(leaflet)
library(lubridate)
library(scales)
library(tmap)
library(sf)

# Read in data
gulls <- read_csv("seagulls_mean_raw.csv")

# Read in map information

#gulls_sf <- st_as_sf(gulls, coords = c("longitude", "latitude"), crs = 4326)
ca <- read_sf(dsn = ".", layer = "california_county_shape_file") # Read data
st_crs(ca) = 4326 # Set CRS
#tmap_mode("view")


#labels <- sprintf(
#  "<strong>%s</strong><br/>%g Seagulls per Sighting",
#  ca_gull$NAME, ca_gull$Jan
#) %>% lapply(htmltools::HTML)


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
                         
                          selectInput("month", 
                                      "Select Month",
                                      choices = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ),
                                      multiple = FALSE),
                          
                          checkboxGroupInput("name", "Exclude Species", choices = NULL)
                      
                        ),
                        
                        
                        mainPanel(
                          plotOutput("FreqPlot")
                        )
                      ))
             ,
             tabPanel("Map",
             
                      sidebarLayout(
                      sidebarPanel(
                      selectInput("month", 
                                  "Select Month",
                                  choices = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ),
                                  multiple = FALSE),
                      
                       
                         selectInput("common_names", 
                             "Select Species",
                             choices = NULL,
                             multiple = FALSE)
                         
                 
                 
               ),
               
                #Show a map of selected area 
               mainPanel(
                 leafletOutput("Map")
               )
            ))
             
             
  )
  
) 








####SERVER####

####PropTable####
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
        !common_name %in% input$name &
        month == input$month
    ) %>% 
    mutate(
      total = sum(mean),
      prop = mean / total
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

####MAPS####

observe({
  m <- gulls %>% 
    select(common_name)
  updateSelectInput(session, "common_names", "Select Species", choices = unique(m))
})



gull_choice <- reactive({ 
  
    gulls %>% 
    filter(common_name == input$common_names) %>% 
    spread(key = "month", value = "mean") %>% 
    rename('NAME' = 'county') %>% 
    full_join(ca, gull_choice)

gull_choice[ , 10][is.na(gulls[ , 10] ) ] = "Western Gull"
gull_choice[ , 11:22][is.na(gulls[ , 11:22] ) ] = 0 
  
  
  })

observe({
  p <- input$month
  
})

ca_gull <- reactive({ 
  
  gull_choice %>% 
    dplyr::select(1:10, input$month)

 
max_val <- max(ca_gull$p)
bins <- c(seq(0, max_val, length.out = 7))
pal <- colorBin("YlOrRd", domain = ca_gull$Jan, bins = bins)

})


output$Map <- renderLeaflet({
  leaflet(ca_gull()) %>% 
    setView(-120.74, 37.61, 5) %>%
    addPolygons(
      fillColor = ~pal(Jan),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE)#,
     # label = labels,
      #labelOptions = labelOptions(
      #  style = list("font-weight" = "normal", padding = "3px 5px"),
      #  textsize = "13px",
      #  direction = "auto")
     )
  
})    
    
  }

  

# Run the application 
shinyApp(ui = ui, server = server)

