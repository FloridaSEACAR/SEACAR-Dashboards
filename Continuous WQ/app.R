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
library(lubridate)
library(shinyjs)

# process_data.R creates .RDS objects which are loaded within Shiny
# source("process_data.R")

# SEACAR Plot theme
plot_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="#FFFFFF"),
        text=element_text(family="Arial"),
        plot.title=element_text(hjust=0.5, size=12, color="#314963"),
        plot.subtitle=element_text(hjust=0.5, size=10, color="#314963"),
        legend.title=element_text(size=14),
        legend.text = element_text(size=12),
        legend.text.align = 0,
        axis.title.x = element_text(size=12, margin = margin(t = 5, r = 0,
                                                             b = 10, l = 0)),
        axis.title.y = element_text(size=12, margin = margin(t = 0, r = 10,
                                                             b = 0, l = 0)),
        axis.text=element_text(size=11),
        axis.text.x=element_text(angle = -45, hjust = 0))

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

seacar_sp_palette <- c("#005396","#0088B1","#00ADAE","#65CCB3","#AEE4C1",
                       "#FDEBA8","#F8CD6D","#F5A800","#F17B00")

# use the following lines to load objects created in process_data.R
files_to_load <- c("df_gaps", "df_gaps_by_entity", "map_df",
                   "table_display", "table_display_by_entity", "pal",
                   "species_sample_locations_pt")

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
      select(-c(link2)) %>%
      select(Entity, NumStations, Data_N, Status, IncludedParams)
  } else {
    table_display_by_entity %>% filter(Entity==entity) %>%
      ungroup() %>% select(-Entity) %>%
      group_by(ProgramLocationID, ProgramNameLink, Status, N_Years) %>%
      summarise(IncludedParams = paste(sort(unique(Button),
                                            decreasing=FALSE),
                                       collapse=", "),
                .groups = "keep") %>%
      rename(ProgramName = ProgramNameLink,
             Years_N = N_Years) %>%
      select(ProgramName, ProgramLocationID, Status, Years_N, IncludedParams)
  }
}

display_long_table <- function(entity){
  table_display_by_entity %>% filter(Entity==entity) %>% 
    ungroup() %>% select(-c(Entity, ProgramName, link)) %>%
    rename(ProgramName = ProgramNameLink) %>%
    select(ProgramName, ProgramLocationID, Data_N, Status, Parameter, SufficientData)
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
      plot_theme + theme(panel.background = element_rect(fill="gray97"))
    
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
      plot_theme + theme(panel.background = element_rect(fill="gray97"))
    
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

habs <- c("SAV", "NEKTON", "CORAL", "Oyster", "CW")

hab_pal <- colorFactor(seacar_sp_palette, habs)

for(hab in habs){
  data <- species_sample_locations_pt %>% filter(habitat==hab)
  leaflet_map <- leaflet_map %>% addCircleMarkers(
    data=data,
    lng = data$Longitude_, lat = data$Latitude_D,
    fillColor = hab_pal(hab), rad = 2.5,
    weight=0.6, fillOpacity=0.6, group=hab, color="black",
    opacity = 0.4, popup = data$popup, label = hab)
}

# leaflet_map <- leaflet_map %>%
#   addLayersControl(baseGroups = c("Positron by CartoDB"),
#                    overlayGroups = c(entities, paste0(entities, "_by_entity"),
#                                      habs),
#                    options = layersControlOptions(collapsed=TRUE))

# Load in necessary files for continuous SKT plots
YM_combined <- readRDS("data/YM_combined.rds")
skt_combined <- readRDS("data/skt_combined.rds")

# Creating units datatable for display in plots
cont_param_df <- data.table(
  ParameterName = c("Dissolved Oxygen","Dissolved Oxygen Saturation","pH",
                    "Salinity","Turbidity","Water Temperature"),
  unit = c("mg/L","%","pH","ppt","NTU","Degrees C"))

# Function to plot continuous plots
plot_cont <- function(plid, param){
  
  # Defining labels for y-axis
  unit <- cont_param_df[ParameterName==param, unit]
  y_labels <- ifelse(param=="pH", unit, paste0(param, " (", unit, ")"))
  
  skt_stats <- skt_combined %>% 
    filter(ProgramLocationID==plid,
           ParameterName==param)
  # Exception for when multiple results are return (AP & NERR), select 1st val
  if(nrow(skt_stats>2)){
    skt_stats <- skt_stats[1]
  }
  
  # Checking for sufficient data
  if(skt_stats$SufficientData==FALSE){
    next
    print("Insufficient data to plot")
  } else {
    
    # Gets x and y values for starting point for trendline
    KT.Plot <- skt_stats %>%
      mutate(x=decimal_date(EarliestSampleDate),
             y=(x-EarliestYear)*SennSlope+SennIntercept)
    # Gets x and y values for ending point for trendline
    KT.Plot2 <- skt_stats %>%
      mutate(x=decimal_date(LastSampleDate),
             y=(x-EarliestYear)*SennSlope+SennIntercept)
    # Combines the starting and endpoints for plotting the trendline
    KT.Plot <- bind_rows(KT.Plot, KT.Plot2)
    rm(KT.Plot2)
    KT.Plot <- as.data.table(KT.Plot[order(KT.Plot$MonitoringID), ])
    KT.Plot <- KT.Plot[!is.na(KT.Plot$y),]
    
    plot_data <- YM_combined %>% filter(ProgramLocationID==plid,
                                        ParameterName==param)
    
    t_min <- min(plot_data$Year)
    t_max <- max(plot_data$YearMonthDec)
    t_max_brk <- as.integer(round(t_max, 0))
    t <- t_max - t_min
    min_RV <- min(plot_data$Mean)
    if(t>=30){
      # Set breaks to every 10 years if more than 30 years of data
      brk <- -10
    }else if(t<30 & t>=10){
      # Set breaks to every 4 years if between 30 and 10 years of data
      brk <- -4
    }else if(t<10 & t>=4){
      # Set breaks to every 2 years if between 10 and 4 years of data
      brk <- -2
    }else if(t<4 & t>=2){
      # Set breaks to every year if between 4 and 2 years of data
      brk <- -1
    }else if(t<2){
      # Set breaks to every year if less than 2 years of data
      brk <- -1
      # Sets t_max to be 1 year greater and t_min to be 1 year lower
      # Forces graph to have at least 3 tick marks
      t_max <- t_max+1
      t_min <- t_min-1
    }
    
    p1 <- ggplot(data=plot_data,
                 aes(x=YearMonthDec, y=Mean)) +
      geom_point(shape=21, size=3, color="#333333", fill="#cccccc",
                 alpha=0.75) +
      geom_line(data=KT.Plot, aes(x=x, y=y),
                color="#000099", linewidth=1.2, alpha=0.7) +
      labs(title=plid,
           subtitle=param,
           x="Year", y=y_labels) +
      scale_x_continuous(limits=c(t_min-0.25, t_max+0.25),
                         breaks=seq(t_max_brk, t_min, brk)) +
      plot_theme
    
    return(p1)
    
  }
}

explanatory_text <- paste(
  "<b>Seasonal Kendall-Tau</b> analysis has been used to create 
  trends for each Water Quality parameter where applicable.",
  
  "<i>Click on the parameters below</i> for each station to 
  view their respective plots.",

  "Indicators must have a minumum of 5 years of data within the geographic range 
  of the analysis to be included in the analysis. In addition, there must be 
  data from at least two months in common across at least two consecutive years 
  within the RCP Managed Area being analyzed. Values that pass both tests 
  will have plots generated.",
  
  sep="<br><br>"
)

#### BEGIN SHINY UI AND SERVER SETTINGS ####

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style('body {font-family: Arial;}')),
  titlePanel("SEACAR Continuous WQ Dashboard"),
  fluidRow(
    column(4,
           leafletOutput("map")),
    column(8,
           plotOutput("gantt_plot")),
  ),
  fluidRow(
    column(5,
      checkboxGroupInput("habitat", 
                         label = "Toggle species habitat sampling",
                         choiceValues = habs,
                         choiceNames = c("Submerged Aquatic Vegetation","Nekton",
                                         "Coral/Coral Reef","Oyster/Oyster Reef",
                                         "Coastal Wetlands"),
                         selected = NULL, inline = TRUE)
      )
  ),
  fluidRow(
    column(4,
           wellPanel(
             selectInput(inputId = "entitySelect",
                         label = "Select Entity to view",
                         choices = entities,
                         selected = "All"))
    ),
    column(8,
           shinyjs::hidden(
             wellPanel(id="hide_text",
               htmlOutput("skt_text")
             ))
           )
  ),
  
  fluidRow(
    column(12,
           DT::DTOutput("entity_overview_table")),
  )
)

entities_plus <- paste0(entities, "_by_entity")

server <- function(input, output, session){
  
  e <- reactive({input$entitySelect})
  
  type <- reactive({if(e()=="All"){"All"}else{"Entity"}})
  
  stations <- reactive({
    if(e()!="All"){
      unique(display_long_table(e())$ProgramLocationID)
    }
  })
  
  params <- reactive({
    if(e()!="All"){
      display_long_table(e()) %>% 
        filter(ProgramLocationID==input$stationSelect,
               SufficientData==TRUE) %>% 
        pull(Parameter)
    }
  })
  
  output$map <- renderLeaflet({
    leaflet_map %>%
      hideGroup(habs)
  })
  
  output$gantt_plot <- renderPlot(plot_gantt(type=type(), ent=e()))
  
  output$entity_overview_table <- DT::renderDT({
    
    data <- setDT(display_table(e()))
    
    DT::datatable(
      data,
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      style = "bootstrap",
      options = list(
                paging=TRUE,
                pageLength=nrow(data)
              ))
  })
  
  observeEvent(input$select_button, {
    param <- strsplit(input$select_button, "_")[[1]][[2]]
    plid <- strsplit(input$select_button, "_")[[1]][[3]]
    
    # Exception for station names that contain "_" within their names (1 prog)
    if(plid=="FKNMS"){
      plid <- paste0("FKNMS",strsplit(input$select_button, "FKNMS")[[1]][2])
    }
    
    showModal(modalDialog(
      title = paste0(
        "Seasonal Kendall-Tau trend analysis for ", param, " - ", plid),
      renderPlot(plot_cont(plid = plid,
                           param = param)),
      easyClose = TRUE,
      size="l"))
    
  })
  
  output$skt_text <- renderUI({
    HTML(explanatory_text)
  })
  
  observe({
    if(e()=="All"){
      leafletProxy("map") %>%
        showGroup(unique(df_gaps$Entity)) %>%
        hideGroup(entities_plus)
      shinyjs::hideElement(id="hide_text")
    } else {
      leafletProxy("map") %>%
        showGroup(paste0(e(), "_by_entity")) %>% 
        hideGroup(entities_plus[!entities_plus %in% paste0(e(), "_by_entity")]) %>%
        hideGroup(entities)
      shinyjs::showElement(id="hide_text")
    }
  })
  
  observe({
    leafletProxy("map") %>%
      showGroup(input$habitat) %>%
      hideGroup(habs[!habs %in% input$habitat])
  })
  
}

shinyApp(ui = ui, server = server)

# deployApp(appFiles = c("app.R","data/skt_combined.rds", "data/YM_combined.rds",
#                        "rds/df_gaps.rds", "rds/df_gaps_by_entity.rds",
#                        "rds/map_df.rds", "rds/pal.rds",
#                        "rds/species_sample_locations_pt.rds",
#                        "rds/table_display.rds",
#                        "rds/table_display_by_entity.rds",
#                        "README.md"))