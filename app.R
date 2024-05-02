library(data.table)
library(dplyr)
library(stringr)
library(leaflet)
library(xlsx)
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

entities <- c("All",highlights,
              "National Water Information System",
              "Water Temperature on Coral Reefs in the Florida Keys",
              "Atlantic Oceanographic and Meteorological Laboratory (AOML) South Florida Program Moored Instrument Array",
              "Florida Keys National Marine Sanctuary Seagrass Monitoring Project",
              "FDEP Bureau of Survey and Mapping Continuous Water Quality Program",
              "National Data Buoy Center",
              "St. Johns River Water Management District Continuous Water Quality Programs",
              "USGS Coral Reef Ecosystem Studies (CREST) Project",
              "Pensacola Bay Water Quality Monitoring Program",
              "Continuous Bottom Temperature Measurements along the Florida Reef Tract")

ui <- fluidPage(
  titlePanel("SEACAR Continuous WQ Dashboard"),
  fluidRow(
    column(10,
           leafletOutput("map")),
    column(2,
           wellPanel(
             selectInput(inputId = "entitySelect",
                         label = "Select Entity to view",
                         choices = entities,
                         selected = "All")
           ),
    ),
  ),
  tabsetPanel(
    tabPanel("All",
             fluidRow(
               column(10,
                      plotOutput("gantt_plot")),
               column(2, "hey")
             ))
  )
)

server <- function(input, output, session){
  
  e <- reactive({input$entitySelect})
  
  type <- reactive({if(e()=="All"){"All"}else{"Entity"}})
  
  output$map <- renderLeaflet({
    leaflet_map
  })
  
  output$gantt_plot <- renderPlot(plot_gantt(type=type(), ent=e()))
  
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