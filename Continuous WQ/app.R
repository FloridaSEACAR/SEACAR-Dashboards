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

# Uncomment rsconnect when deploying app
# library(rsconnect)

# process_data.R creates .RDS objects which are loaded within app.R when deployed
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
                   "species_sample_locations_pt", "publish_date","YM_combined", 
                   "skt_combined")

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
display_table <- function(entity,activeStatus){
  if(entity=="All"){
    table_display %>% filter(Status %in% activeStatus) %>% ungroup() %>%
      select(-c(link2)) %>%
      select(Entity, NumStations, Data_N, Status, IncludedParams) %>%
      rename(Parameters = IncludedParams,
             "Number of Stations" = NumStations,
             "Number of Observations" = Data_N)
  } else {
    table_display_by_entity %>% filter(Entity==entity,
                                       Status %in% activeStatus) %>%
      ungroup() %>% select(-Entity) %>%
      group_by(ProgramLocationID, ProgramNameLink, Status) %>%
      summarise(
        "Parameters (Number of Years)" = paste(sort(unique(Button),
                                                    decreasing=FALSE), 
                                               collapse=", "),
        .groups = "keep") %>%
      rename("Program Name" = ProgramNameLink,
             "Station ID" = ProgramLocationID) %>%
      select("Program Name", "Station ID", Status, 
             "Parameters (Number of Years)")
  }
}

display_long_table <- function(entity, activeStatus){
  table_display_by_entity %>% filter(Entity==entity,
                                     Status %in% activeStatus) %>% 
    ungroup() %>% select(-c(Entity, ProgramName, link)) %>%
    rename(ProgramName = ProgramNameLink) %>%
    select(ProgramName, ProgramLocationID, Data_N, Status, Parameter, SufficientData)
}

# Ensure proper order of Entity display
df_gaps$Entity <- fct_rev(factor(df_gaps$Entity))

# Function to serve gannt plots
plot_gantt <- function(type, ent="Aquatic Preserve Continuous Water Quality Program",
                       activeStatus = "Active"){
  
  if(type=="All"){
    
    if(length(activeStatus)>0){
      df_gaps <- df_gaps %>% filter(Status %in% activeStatus)
    }
    
    min_year <- min(df_gaps$startYear)
    max_year <- max(df_gaps$endYear)
    
    plot <- ggplot(df_gaps, 
                   aes(x=startYear, xend=endYear, y=Entity, yend=Entity)) +
      geom_segment(linewidth=4, colour=pal(df_gaps$Entity)) +
      labs(title="Years of data for each Entity",
           x="Years",
           y="Entity") + 
      scale_x_continuous(limits=c(min_year, max_year),
                         breaks=seq(max_year, min_year, -2)) +
      plot_theme + theme(panel.background = element_rect(fill="gray97"))      
    
  } else if(type=="Entity"){
    
    if(length(activeStatus)>0){
      
      # Filter by entity
      # Shorten APCWQ and NERR SWMP for display in Gantt plot
      pal_df <- df_gaps_by_entity %>% 
        filter(Entity==ent) %>%
        mutate(ProgramName = str_replace_all(ProgramName, c(
          "Aquatic Preserves Continuous Water Quality Monitoring" = "APCWQ",
          "Aquatic Preserve Continuous Water Quality Monitoring" = "APCWQ",
          "National Estuarine Research Reserve System-Wide Monitoring Program" = "NERR SWMP")))
      
      # Filter further by Status
      # filtered_df and pal_df separate to keep palette intact
      filtered_df <- pal_df %>% 
        filter(Status %in% activeStatus)
      
      # Allows color palette to display for unique programs in APCWQ and NERR SWMP
      # Set secondary color palette (by Program instead of Entity)
      pal2 <- colorFactor(seacar_palette, unique(pal_df$ProgramName))
      
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
  
  filtered_data <- map_df[Entity==e, ]
  
  for(stat in unique(filtered_data$Status)){
    leaflet_map <- leaflet_map %>%
      addCircleMarkers(data = filtered_data[Status==stat, ], 
                       lat=~Lat, lng=~Lon, fillColor=~pal(Entity),
                       weight=0.5, radius=~rad,
                       fillOpacity=0.4, opacity=0.4, color="black",
                       popup = ~popup,
                       label = ~label,
                       group = paste0("All_",stat))
  }
  
  pal2 <- colorFactor(seacar_palette, unique(filtered_data$ProgramName))
  program_pal <- pal2(unique(filtered_data$ProgramName))
  names(program_pal) <- unique(filtered_data$ProgramName)
  
  if(e %in% highlights){
    # Active vs. Historical
    for(stat in unique(filtered_data$Status)){
      stat_data <- filtered_data[Entity %in% highlights & Status==stat, ]
      
      leaflet_map <- leaflet_map %>%
        addCircleMarkers(data = stat_data,
                         lat=~Lat, lng=~Lon, fillColor=~pal2(ProgramName),
                         weight=0.5, radius=~rad, 
                         fillOpacity=0.6, opacity=0.6, color="black",
                         popup = ~popup,
                         label = ~label, 
                         group=paste0(e, "_by_entity","_",stat))
    }
    
  } else if(!e %in% highlights){
    # Active vs. Historical
    for(stat in unique(filtered_data$Status)){
      stat_data <- filtered_data[!Entity %in% highlights & Status==stat, ]
      
      leaflet_map <- leaflet_map %>%
        addCircleMarkers(data = stat_data,
                         lat=~Lat, lng=~Lon, fillColor=~pal(Entity),
                         weight=0.5, radius=~rad, 
                         fillOpacity=0.6, opacity=0.6, color="black",
                         popup = ~popup,
                         label = ~label, 
                         group=paste0(e, "_by_entity","_",stat))
    }
  }
}

habs <- c("SAV", "NEKTON", "CORAL", "Oyster", "CW")
# Set habitat palette using unique SP palette
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

# Adds the control option in map to control layers
# Comment out to remove, layers are manipulated in-line within the Shiny app

# leaflet_map <- leaflet_map %>%
#   addLayersControl(baseGroups = c("Positron by CartoDB"),
#                    overlayGroups = c("All_Historical","All_Active",
#                                      paste0(entities[entities!="All"], "_by_entity_Active"),
#                                      paste0(entities[entities!="All"], "_by_entity_Historical"),
#                                      habs),
#                    options = layersControlOptions(collapsed=TRUE))

# UNCOMMENTED FOR EB01 FIX
# Load in necessary files for continuous SKT plots
# YM_combined <- readRDS("data/YM_combined.rds")
# skt_combined <- readRDS("data/skt_combined.rds")

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
  "<b>Seasonal Kendall-Tau</b> analysis has been used to test for significant 
  trends in each Water Quality parameter where applicable.",
  
  "To view a graphical plot of a parameter over time, choose a <b>Station ID</b> and 
  then click on a <b>Parameter</b> of interest.",
  
  "Indicators must have at least 5 years of data to be included in the analysis.",
  
  sep="<br><br>"
)

# "<img src='data/dep-logos.png' alt='Funding logos'>"

addResourcePath(prefix="www", directoryPath = "www")

funding_text <- paste(
  "<b>Funding Acknowledgement</b>",
  
  "SEACAR is funded, in part, through a grant agreement from the 
  Florida Department of Environmental Protection, Florida Coastal Management 
  Program, by a grant provided by the Office for Coastal Management 
  under the <a href='https://coast.noaa.gov/czm/act/' target='_blank'>
  Coastal Zone Management Act of 1972</a>, as amended, 
  National Oceanic and Atmospheric Administration. 
  The views, statements, findings, conclusions, and recommendations expressed 
  herein are those of the author(s) and do not necessarily reflect the views 
  of the State of Florida, NOAA or any of their subagencies.",
  
  tags$div(
    tags$img(
      src = "www/dep-logos.png",
      alt = "Funding logos",
      height = "100",
      width = "290"
    ),style="text-align:center;"
  ),
  
  tags$div(paste0("Published: ", publish_date), style="text-align:center;"),
  
  sep="<br><br>"
)

dep_colors <- c("#2E5270","#6D869B","#C8EAFB")

#### BEGIN SHINY UI AND SERVER SETTINGS ####

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")),
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
                         selected = "All"),
             checkboxGroupInput(inputId = "activeSelect",
                                label = "View Historical or Active sites",
                                choices = c("Active","Historical"),
                                selected = c("Active","Historical")),
             shinyjs::hidden(
               textOutput("activeSelectText"))
           )
           
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
  ),
  
  fluidRow(
    column(3),
    column(6,
           wellPanel(
             htmlOutput("funding")
           )),
    column(3)
    
  )
)

entities_plus <- c(paste0(entities[entities!="All"], "_by_entity_Active"),
                   paste0(entities[entities!="All"], "_by_entity_Historical"))

server <- function(input, output, session){
  
  e <- reactive({input$entitySelect})
  
  active <- reactive({input$activeSelect})
  
  type <- reactive({if(e()=="All"){"All"}else{"Entity"}})
  
  stations <- reactive({
    if(e()!="All"){
      unique(display_long_table(e(), activeStatus=active())$ProgramLocationID)
    }
  })
  
  params <- reactive({
    if(e()!="All"){
      display_long_table(e(), activeStatus=active()) %>% 
        filter(ProgramLocationID==input$stationSelect,
               SufficientData==TRUE) %>% 
        pull(Parameter)
    }
  })
  
  output$map <- renderLeaflet({
    leaflet_map %>%
      hideGroup(habs)
  })
  
  output$gantt_plot <- renderPlot(plot_gantt(type=type(), ent=e(),
                                             activeStatus=active()))
  
  output$entity_overview_table <- DT::renderDT({
    
    data <- setDT(display_table(e(), activeStatus=active()))
    
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
  
  output$funding <- renderUI({
    HTML(funding_text)
  })
  
  activeChoices <- reactive({
    unique(display_table(e(), activeStatus=c("Active","Historical"))$Status)
  })
  
  
  observeEvent(input$entitySelect, {
    updateCheckboxGroupInput(session = session,
                             inputId = "activeSelect",
                             choices = activeChoices(),
                             selected = activeChoices())
  })
  
  output$activeSelectText <- renderText(active())
  
  observe({
    if(e()=="All"){
      
      active_groups <- character()
      inactive_groups <- c(entities_plus,"All_Active","All_Historical")
      remove <- character()
      
      if("Active" %in% active()){
        active_groups <- c(active_groups, "All_Active")
        remove <- c(remove,"All_Active")
      }
      
      if("Historical" %in% active()){
        active_groups <- c(active_groups, "All_Historical")
        remove <- c(remove,"All_Historical")
      }
      
      if(length(active_groups) > 0){
        leafletProxy("map") %>% showGroup(active_groups)
      }
      
      if(length(inactive_groups) > 0){
        leafletProxy("map") %>% hideGroup(inactive_groups[!inactive_groups %in% remove])
      }

      shinyjs::hideElement(id="hide_text")
      shinyjs::showElement(id="activeSelect")
    } else {
      
      active_groups <- character()
      inactive_groups <- entities_plus
      
      if("Active" %in% active()){
        active_groups <- c(active_groups, paste0(e(), "_by_entity_Active"))
        inactive_groups <- setdiff(inactive_groups, paste0(e(), "_by_entity_Active"))
      }
      
      if("Historical" %in% active()){
        active_groups <- c(active_groups, paste0(e(), "_by_entity_Historical"))
        inactive_groups <- setdiff(inactive_groups, paste0(e(), "_by_entity_Historical"))
      }
      
      if(length(active_groups) > 0){
        leafletProxy("map") %>% showGroup(active_groups) %>% 
          hideGroup(c("All_Historical","All_Active"))
      }
      
      if(length(inactive_groups) > 0){
        leafletProxy("map") %>% hideGroup(inactive_groups) %>% 
          hideGroup(c("All_Historical","All_Active"))
      }
      
      shinyjs::showElement(id="hide_text")
      shinyjs::showElement(id="activeSelect")
    }
  })
  
  observe({
    leafletProxy("map") %>%
      showGroup(input$habitat) %>%
      hideGroup(habs[!habs %in% input$habitat])
  })
  
}

shinyApp(ui = ui, server = server)

# library(rsconnect)
## USE BELOW FOR EB01 FIX
# deployApp(appFiles = c("app.R","rds/skt_combined.rds", "rds/YM_combined.rds",
#                        "rds/df_gaps.rds", "rds/df_gaps_by_entity.rds",
#                        "rds/map_df.rds", "rds/pal.rds",
#                        "rds/species_sample_locations_pt.rds",
#                        "rds/table_display.rds",
#                        "rds/table_display_by_entity.rds",
#                        "rds/publish_date.rds",
#                        "README.md", "www/dep-logos.png",
#                        "www/style.css"))


# deployApp(appFiles = c("app.R","data/skt_combined.rds", "data/YM_combined.rds",
#                        "rds/df_gaps.rds", "rds/df_gaps_by_entity.rds",
#                        "rds/map_df.rds", "rds/pal.rds",
#                        "rds/species_sample_locations_pt.rds",
#                        "rds/table_display.rds",
#                        "rds/table_display_by_entity.rds",
#                        "rds/publish_date.rds",
#                        "README.md", "www/dep-logos.png",
#                        "www/style.css"))