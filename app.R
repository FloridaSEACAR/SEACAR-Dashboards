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
library(forcats)
library(DT)

# source("process_data.R")

entities <- c("All",highlights,
              "National Water Information System",
              "Water Temperature on Coral Reefs in the Florida Keys",
              "AOML South Florida Program Moored Instrument Array",
              "FKNMS Seagrass Monitoring Project",
              "FDEP Bureau of Survey and Mapping Continuous WQ Program",
              "National Data Buoy Center",
              "St. Johns River Water Management District Continuous WQ Programs",
              "USGS Coral Reef Ecosystem Studies (CREST) Project",
              "Pensacola Bay WQ Monitoring Program",
              "Continuous Bottom Temperature Measurements along the Florida Reef Tract")

## Functions for use within Shiny
# Variable table display
display_table <- function(entity){
  if(entity=="All"){
    table_display %>% ungroup() %>%
      select(-c(Entity, link2)) %>%
      rename(Entity = EntityLink) %>%
      select(Entity, NumStations, Data_N, includedParams)
  } else {
    table_display_by_entity %>% filter(Entity==entity) %>% 
      ungroup() %>% select(-c(Entity, ProgramName, link)) %>%
      rename(ProgramName = ProgramNameLink) %>%
      select(ProgramName, ProgramLocationID, Data_N, includedParams)
  }
}

ui <- fluidPage(
  titlePanel("SEACAR Continuous WQ Dashboard"),
  fluidRow(
    column(4),
    column(4,
           wellPanel(
             selectInput(inputId = "entitySelect",
                         label = "Select Entity to view",
                         choices = entities,
                         selected = "All"))
           ),
    column(4)
  ),
  fluidRow(
    column(4,
           leafletOutput("map")),
    column(8,
           # actionButton("show_plot", "Show plot"),
           plotOutput("gantt_plot")),
  ),
  fluidRow(
    column(12,
           DT::DTOutput("entity_overview_table"))
  )
)

entities_plus <- paste0(entities, "_by_entity")

server <- function(input, output, session){
  
  e <- reactive({input$entitySelect})
  
  type <- reactive({if(e()=="All"){"All"}else{"Entity"}})
  
  output$map <- renderLeaflet({
    leaflet_map
  })
  
  output$gantt_plot <- renderPlot(plot_gantt(type=type(), ent=e()))
  
  output$entity_overview_table <- DT::renderDT({
    datatable(display_table(e()),
              escape=FALSE,
              options = list(
                paging=TRUE,
                pageLength=nrow(display_table(e()))
              ))
  })
  
  observe({
    if(e()=="All"){
      leafletProxy("map") %>%
        showGroup(unique(map_df$Entity)) %>%
        hideGroup(entities_plus)
    } else {
      leafletProxy("map") %>%
        showGroup(paste0(e(), "_by_entity")) %>% 
        hideGroup(entities_plus[!entities_plus %in% paste0(e(), "_by_entity")]) %>%
        hideGroup(entities)
    }
  })
  
  observeEvent(input$show_plot, {
    showModal(modalDialog(title = "Enlarged plot",
                          renderPlot(plot_gantt(type=type(), ent=e())),
                          easyClose = TRUE,
                          size="l"))
  })
}

shinyApp(ui = ui, server = server)
