# Final App Version
library(shiny)
library(tidyverse)
library(shinythemes)
library(leaflet)
library(lubridate)
library(scales)
library(tmap)
library(sf)


# Read in data
gulls <- read_csv("seagulls_mean_raw.csv") %>% 
  mutate(month = factor(month, 
                        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" )))
ca <- read_sf(dsn = ".", layer = "california_county_shape_file") # Read data
st_crs(ca) = 4326 # Set CRS

# Set Breaks for Map
breaks <- c(0,10,20,30,40,50,100,200,800)




ui <- fluidPage(
  # Map Background color
  tags$head(
    tags$style(HTML(".leaflet-container { background: #ECF0F1; }"))
  ),
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
                          
                          selectInput("months", 
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
                          # Select species first
                          selectInput("common_names", 
                                      "Select Species",
                                      choices = c("Black-headed Gull",
                                                  "Black-tailed Gull",
                                                  "Bonaparte's Gull",      
                                                  "California Gull",
                                                  "Franklin's Gull",
                                                  "Glaucous-winged Gull",    
                                                  "Glaucous Gull",
                                                  "Great Black-backed Gull",
                                                  "Heermann's Gull",         
                                                  "Herring Gull",
                                                  "Iceland Gull",
                                                  "Ivory Gull",             
                                                  "Kelp Gull",
                                                  "Laughing Gull",
                                                  "Lesser Black-backed Gull",
                                                  "Little Gull",
                                                  "Mew Gull",
                                                  "Ring-billed Gull",
                                                  "Ross's Gull",
                                                  "Sabine's Gull",
                                                  "Slaty-backed Gull",       
                                                  "Western Gull",
                                                  "Yellow-footed Gull"),
                                      selected = "Western Gull",
                                      multiple = FALSE),
                          
                          # Select month second
                          selectInput("month", 
                                      "Select Month",
                                      choices = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ),
                                      multiple = FALSE),
                          plotOutput("overview")
                          
                        ),
                        
                        # Display map
                        mainPanel(
                          leafletOutput("Map", height="600px")
                        )
                      ))
  )
  
)

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
          month == input$months
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
      coord_flip() +
      theme_classic() +
      labs(
        x = "", y = "Proportion"
      ) +
      theme(
        panel.background = element_rect(fill = "#ECF0F1",
                                        colour = "#ECF0F1"),
        plot.background = element_rect(fill = "#ECF0F1"),
        axis.text.y =element_text(size=rel(1.4))
      )
    
  })
  ###overview output
  
  gulls_over <- reactive({ 
    
    gulls %>% 
      filter(common_name == input$common_names) 
    
  })
  
  output$overview <- renderPlot({
    
    ggplot(data = gulls_over(),
           aes(x = month, y = mean)) +
      geom_line() +
      geom_point() +
      theme_classic() +
      labs(x = "", y = "County Means") +
      ggtitle("Monthly Abundances")+
      theme(
        panel.background = element_rect(fill = "#ECF0F1",
                                        colour = "#ECF0F1"),
        plot.background = element_rect(fill = "#ECF0F1"),
        axis.text.y =element_text(size=rel(1.3))) 
    
  })
  
  
  
  gull_choice <- reactive({ 
    
    gulls %>% 
      filter(common_name == input$common_names) %>% 
      spread(key = "month", value = "mean") %>% 
      rename('NAME' = 'county') %>% 
      full_join(ca, by = 'NAME') %>% 
      select(-contains(".y")) %>% 
      rename_at(.vars = vars(ends_with(".x")),
                .funs = funs(sub("[.]x$", "", .))) %>% 
      mutate(common_name2 = input$common_names) %>% 
      replace(is.na(.), 0) %>% 
      st_as_sf(crs = 4326) %>% 
      select(
        "common_name",  "NAME", "Jan", "Feb", "Mar", "Apr",
        "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",                  
        "PERIMETER", "LSAD_TRANS", "CO06_D00_I", "LSAD", "STATE",        
        "AREA", "CO06_D00_", "COUNTY", "common_name2", "geometry" 
      )%>% 
      mutate_at(vars(3:14), as.numeric) %>% 
      select(23, 2, input$month, 15:22) %>%
      rename('gull_value' = 3)
    
  })
  
  pal <- reactive({
    colorBin("YlOrRd", domain = gull_choice()$gull_value , bins = breaks)
  })
  
  
  labels <- reactive({
    sprintf(
      "<strong>%s</strong><br/>%g Seagulls per Sighting",
      gull_choice()$common_name2, gull_choice()$gull_value
    ) %>% lapply(htmltools::HTML)
  })
  
  
  output$Map <- renderLeaflet({
    leaflet(ca) %>% 
      setView(-120.74, 37.61, 6) %>%
      addPolygons(
        data = gull_choice(),
        fillColor = ~pal()(gull_choice()$gull_value),
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
          bringToFront = TRUE),
        label = labels(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 5px"),
          textsize = "13px",
          direction = "auto")
      ) %>% 
      addLegend(pal = pal(), values = breaks, opacity = 0.7, title = NULL,
                position = "topright")
    
  })
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)