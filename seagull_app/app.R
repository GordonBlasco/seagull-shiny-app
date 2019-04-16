# LIVE version!
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
  mutate(
    months = case_when(
      month == "Jan" ~ "January",
      month == "Feb" ~ "Febuary",
      month == "Mar" ~ "March",
      month == "Apr" ~ "April",
      month == "May" ~ "May",
      month == "Jun" ~ "June",
      month == "Jul" ~ "July",
      month == "Aug" ~ "August",
      month == "Sep" ~ "September",
      month == "Oct" ~ "October",
      month == "Nov" ~ "November",
      month == "Dec" ~ "December")) %>% 
  select(common_name, months, county, mean) %>% 
  rename('month' = 'months') %>% 
  mutate(month = factor(month, 
                        levels = c("January", "Febuary", "March", "April", "May", "June", 
                                   "July", "August", "September", "October", "November", "December" )))
ca <- read_sf(dsn = ".", layer = "california_county_shape_file") # Read data
st_crs(ca) = 4326 # Set CRS

# Set Breaks for Map
breaks <- c(0,1,10,20,30,40,50,100,200,800)




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
                      #sidebarPanel(
                        #img(src='Gull2.png', height = 350, width = 350)
                      #),
                      mainPanel(
                      h2("Information"),
                      p("The data used in this project comes from the citizen science project eBird that was started by the Cornell Lab of Ornithology and the National Audubon Society. eBird is the dominant online birding platform where anyone can upload their birding checklists. Data was taken from eBird* covering all seagull species seen in California from 2002 – 2018."),
                      h2("County Frequency Plot"),
                      p("The frequency plot is designed to show the relative proportions of different seagull species in each county.  This is intended to help birders learn the most abundant species in their county and what rarities they can expect. More abundant species will make up a greater proportion of the reported gulls, while rare birds will make up the smallest abundance. It is possible to exclude species from this chart to help narrow down possible gulls for tricky ID's."),
                      h2("Species Map"),
                      p("The species map will show the average number of each seagull per checklist in each of the California counties. This can help any birder hoping to track down any rare gull or understand migration patterns. When a gull species is selected it will also show a chart that will provide an overview of its monthly distribution in all California counties."),
                      h2(" "),
                      p(" "),
                      h2("Data Citation"),
                      p("eBird Basic Dataset. Version: EBD_relFeb-2019. Cornell Lab of Ornithology, Ithaca, New York. May 2013.")
                      )   
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
                                      choices = c("January", "Febuary", "March", "April", "May", "June", 
                                                  "July", "August", "September", "October", "November", "December" ),
                                      multiple = FALSE),
                          
                          checkboxGroupInput("name", "Exclude Species", choices = NULL)
                          
                        ),
                        
                        
                        mainPanel(
                          plotOutput("FreqPlot")
                        )
                      ))
             ,
             tabPanel("Species Map",
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
                                      selected = "California Gull",
                                      multiple = FALSE),
                          
                          # Select month second
                          selectInput("month", 
                                      "Select Month",
                                      choices = c('January', 
                                                  "Febuary", 
                                                  "March", 
                                                  "April", 
                                                  "May", 
                                                  "June", 
                                                  "July", 
                                                  "August", 
                                                  "September", 
                                                  "October", 
                                                  "November", 
                                                  "December" ),
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
    print(input$months)
    x <- gulls %>% 
      filter(county == input$county &
               month == input$months) %>% 
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
      geom_col(fill = "skyblue4") +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(labels=percent_format(),
                         expand = c(0,0)) +
      theme_bw() +
      coord_flip() +
      theme_classic() +
      labs(
        x = "", y = "Proportion of Monthly Gull Sightings"
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
        axis.text.y =element_text(size=rel(1.3))) +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_discrete(labels=c("January" = "J", 
                                "Febuary" = "F",
                                "March" = "M",
                                "April" = "A",
                                "May" = "M",
                                "June" = "J",
                                "July" = "J",
                                "August" = "A",
                                "September" = "S",
                                "October" = "O",
                                "November" = "N",
                                "December" = "D"
                                ))
    
  })
  
  
  
  gull_choice <- reactive({ 
    
    gulls %>% 
      complete(common_name, month, county) %>% 
      replace(is.na(.), 0.0) %>% 
      rename('NAME' = 'county',
             'gull_value' = 'mean') %>% 
      full_join(ca, by = 'NAME') %>% 
      st_as_sf(crs = 4326) %>% 
      filter(common_name == input$common_names) %>%
      filter(month == input$month)
    
  })
  
  pal <- reactive({
    colorBin("YlOrRd", domain = gull_choice()$gull_value , bins = breaks)
  })
  
  
  labels <- reactive({
    sprintf(
    "<strong>%s</strong><br/>%g gulls per sighting",
    gull_choice()$NAME, round(gull_choice()$gull_value, 0)
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