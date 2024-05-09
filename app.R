library(data.table)
library(dplyr)
library(stringr)
library(leaflet)
library(leaflet.providers)
library(rstudioapi)
library(shiny)
library(tidyr)
library(ggplot2)
library(DT)
library(forcats)

# process_data.R creates .RDS objects which are loaded within Shiny
# source("process_data.R")

# Custom SEACAR palette
seacar_palette <- c(
  "#964059",
  "#E05E7B",
  "#E98C86",
  "#F1B8AB",
  "#F8CAAA",
  "#F8E6B9",
  "#FEEEE1",
  "#DAE9DA",
  "#8BE4C2",
  "#7EE7E8",
  "#8FD0EC",
  "#6FA1DD",
  "#889BD1",
  "#8F83D3",
  "#6B59AB"
)

# use the following lines to load objects created in process_data.R
files_to_load <- c("df_gaps", "df_gaps_by_entity", "map_df",
                   "table_display", "table_display_by_entity", "pal")

for(file in files_to_load){
  eval(call("<-", as.name(file), readRDS(paste0("rds/",file,".rds"))))
}

# Entities to highlight
highlights <- c("Aquatic Preserve Continuous Water Quality Program", 
                "National Estuarine Research Reserve SWMP")

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
      select(Entity, NumStations, Data_N, IncludedParams)
  } else {
    table_display_by_entity %>% filter(Entity==entity) %>% 
      ungroup() %>% select(-c(Entity, ProgramName, link)) %>%
      rename(ProgramName = ProgramNameLink) %>%
      select(ProgramName, ProgramLocationID, Data_N, IncludedParams)
  }
}

df_gaps$Entity <- fct_rev(factor(df_gaps$Entity))

# Function to serve gannt plots
plot_gantt <- function(type, ent="Aquatic Preserve Continuous Water Quality Program"){
  if(type=="All"){
    
    min_year <- min(df_gaps$startYear)
    max_year <- max(df_gaps$endYear)
    
    plot <- ggplot(df_gaps, aes(x=startYear, xend=endYear, y=Entity, yend=Entity)) +
      geom_segment(linewidth=4, colour=pal(df_gaps$Entity)) +
      labs(title="Years of data for each Entity",
           x="Years",
           y="Entity") + 
      scale_x_continuous(limits=c(min_year, max_year),
                         breaks=seq(max_year, min_year, -2)) +
      plot_theme
  } else if(type=="Entity"){
    filtered_df <- df_gaps_by_entity %>% 
      filter(Entity==ent)
    
    pal2 <- colorFactor(seacar_palette, unique(filtered_df$ProgramName))
    
    if(ent %in% highlights){
      program_pal <- pal2(unique(filtered_df$ProgramName))
    } else {
      program_pal <- pal(unique(filtered_df$Entity))
    }
    
    names(program_pal) <- unique(filtered_df$ProgramName)
    
    filtered_df$ProgramName <- factor(filtered_df$ProgramName)
    
    min_year <- min(filtered_df$startYear)
    max_year <- max(filtered_df$endYear)
    
    plot <- ggplot(filtered_df,
                   aes(x=startYear, xend=endYear, 
                       y=ProgramLocationID, yend=ProgramLocationID)) +
      geom_segment(linewidth=4, aes(color=filtered_df$ProgramName)) +
      labs(title=paste0(ent, " - Years of data for each Station"),
           x="Years",
           y="ProgramLocationID",
           color="Program") +
      scale_y_discrete(limits = unique(filtered_df$ProgramLocationID)) +
      scale_color_manual(values=program_pal) +
      scale_x_continuous(limits=c(min_year, max_year),
                         breaks=seq(max_year, min_year, -2)) +
      plot_theme
    
  }
  return(plot)
}

# Create leaflet map
# Create map object to load in shiny app
leaflet_map <- leaflet(map_df) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels,
                   group = "Positron by CartoDB")

ents <- unique(map_df$Entity)

for(e in ents){
  
  filtered_data <- map_df %>% filter(Entity==e)
  
  leaflet_map <- leaflet_map %>%
    addCircleMarkers(data = filtered_data, 
                     lat=~Lat, lng=~Lon, fillColor=~pal(Entity),
                     weight=0.5, radius=~rad, 
                     fillOpacity=0.4, opacity=0.4, color="black",
                     popup = ~popup,
                     label = ~label,
                     group = e)
  
  
  pal2 <- colorFactor(seacar_palette, unique(filtered_data$ProgramName))
  program_pal <- pal2(unique(filtered_data$ProgramName))
  names(program_pal) <- unique(filtered_data$ProgramName)
  
  if(e %in% highlights){
    leaflet_map <- leaflet_map %>%
      addCircleMarkers(data = filtered_data[Entity %in% highlights, ],
                       lat=~Lat, lng=~Lon, fillColor=~pal2(ProgramName),
                       weight=0.5, radius=~rad, 
                       fillOpacity=0.6, opacity=0.6, color="black",
                       popup = ~popup,
                       label = ~label, 
                       group=paste0(e, "_by_entity"))
  } else if(!e %in% highlights){
    leaflet_map <- leaflet_map %>%
      addCircleMarkers(data = filtered_data[!Entity %in% highlights, ],
                       lat=~Lat, lng=~Lon, fillColor=~pal(Entity),
                       weight=0.5, radius=~rad, 
                       fillOpacity=0.6, opacity=0.6, color="black",
                       popup = ~popup,
                       label = ~label, 
                       group=paste0(e, "_by_entity"))
  }
}

leaflet_map <- leaflet_map %>%
  addLayersControl(baseGroups = c("Positron by CartoDB"),
                   overlayGroups = c(entities, paste0(entities, "_by_entity")),
                   options = layersControlOptions(collapsed=TRUE))

# Define all_entities plot beforehand to save processing
# all_entities_gantt_plot <- plot_gantt(type="All")

ui <- fluidPage(
  tags$head(tags$style('body {font-family: Arial;}')),
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
        showGroup(unique(df_gaps$Entity)) %>%
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
