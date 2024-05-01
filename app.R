library(rstudioapi)
library(shiny)
library(sf)
library(tidyr)
library(mapview)
library(magick)
library(bslib)
library(bsicons)
library(ggplot2)
library(plotly)

# source("process_data.R")

entities <- c("All",unique(map_df$Entity))

ui <- fluidPage(
  titlePanel("SEACAR Continuous WQ Dashboard"),
  fluidRow(
    column(8,
           leafletOutput("map")),
    column(4,
           wellPanel(
             selectInput(inputId = "entitySelect",
                         label = "Select Entity to view",
                         choices = entities,
                         selected = "All")
           ),
    ),
  )
)

server <- function(input, output, session){
  
  e <- reactive({input$entitySelect})
  
  output$map <- renderLeaflet({
    leaflet_map
  })
  
  observe({
    if(e()=="All"){
      leafletProxy("map") %>%
        showGroup(unique(map_df$Entity))
    } else {
      leafletProxy("map") %>%
        showGroup(e()) %>% 
        hideGroup(entities[!entities %in% e()])
    }
  })
}

shinyApp(ui = ui, server = server)
