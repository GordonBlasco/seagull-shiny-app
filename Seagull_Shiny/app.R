library(shiny)
library(tidyverse)
library(shinythemes)
library(leaflet)
library(lubridate)
library(scales)

# Read in data
gulls <- read_csv("seagulls_tiny.csv")

####UI####
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("I don't do gulls..."),
  
  navbarPage("",
             
             tabPanel("Information",
                      h1("A header!"),
                      h2("Gull information "),
                      h2("The tabs"),
                      p("The frequency plot will tell you the probability of finding a species given the parameters you set"),
                      p("The interactive map will allow you to see temportal and spacial changes for each species"),
                      p("Followed by another paragraph of text..."),
                      h1("Then another header"),
                      p("You get the idea...)")
                      
             ),
        
             
             tabPanel("Frequency Plot", # Tester
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("county", 
                                      "Select County",
                                      choices = c("Santa Barbara",
                                                  "Los Angeles",
                                                  "Orange")),
                          
                          checkboxGroupInput("name",
                                             "Exclude a Species",
                                             c(
                                               "Western Gull" = "Western Gull",
                                               "California Gull" = "California Gull"
                                             ))
                      
                        ),
                        
                        # Show a map of selected area 
                        mainPanel(
                          plotOutput("FreqPlot")
                        )
                      ))
             
  )
  
)








####SERVER####
# Define server logic required to draw a histogram
server <- function(input, output) {
  
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
    
    
  }

  

# Run the application 
shinyApp(ui = ui, server = server)

