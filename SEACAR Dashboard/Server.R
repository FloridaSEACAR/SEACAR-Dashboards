library(shiny)
library(dplyr)
library(data.table)
library(stringr)
library(ggplot2)
library(DT)
library(purrr)

# Functions ----
# returns number of programs for each parameter
plotProgramParams <- function(h, ret="plot"){
  data <- data_directory[[h]][["programParams"]]
  
  plot <- ggplot(data, aes(x=0, xend=n, y=ParameterName, yend=ParameterName)) +
    geom_segment(linewidth=14, colour="#4472C4") +
    geom_text(aes(x=n, label=n, hjust=-0.3), color="black") +
    labs(title="Number of Programs for each Parameter",
         x="Number of Programs",
         y="Parameter") +
    plot_theme
  if (ret == "plot"){
    return(plot)
  } else if(ret == "list"){
    return(data$ParameterName)
  } else if(ret =="data"){
    return(data)
  }
}

# retuns number of years of data for each ProgramID
plotProgramYears <- function(h){
  data <- data_directory[[h]][["programYears"]]
  pal <- data_directory[[h]][["pal"]]
  
  plot <- ggplot(data, aes(x=startYear-0.1, xend=endYear+0.1, y=ProgramID, yend=ProgramID)) +
    geom_segment(linewidth=4, colour=pal(data$ProgramID)) +
    labs(title="Years of data for each Program ID",
         x="Years",
         y="Program ID") +
    plot_theme
  return(plot)
}

displayOverviewTable <- function(h, id, type){
  if(type=="ma"){
    table <- data_directory[[h]][["maOverviewTable"]]
    if(!id=="All"){return(table %>% filter(ManagedAreaName == id))} else {return(table)}
  }
  
  if(type=="program"){
    table <- data_directory[[h]][["overviewTable"]]
    if(!id=="All"){return(table %>% filter(ProgramName == id))} else {return(table)}
  }
}

displaySummaryTable <- function(h, id, type){
  if(id=="All"){
    data_directory[[h]][["summTableAll"]]
  } else if(type=="program") {
    data_directory[[h]][["summTableByProgram"]] %>% filter(ProgramName == id) %>%
      ungroup() %>% select(-c(ProgramName,ProgramID))
  } else if(type=="ma"){
    data_directory[[h]][["maSummTable"]] %>% filter(ManagedAreaName == id) %>%
      ungroup() %>% select(-ManagedAreaName)
  }
}

# Trend tables for SAV plots
trendTables <- function(h, ma, plot_type){
  if(h=="Submerged Aquatic Vegetation"){
    sav_trends %>% filter(ManagedAreaName==ma) %>% select(-ManagedAreaName)
  }
  else if(h=="Oyster Reef"){
    param <- ifelse(plot_type=="Oyster_Dens", "Density",
                    ifelse(plot_type=="Oyster_SH", "Shell Height","Percent Live"))
    if(plot_type=="Oyster_SH"){
      oy_trends %>% filter(ManagedAreaName==ma, ParameterName==param, !SizeClass=="") %>% 
        select(-c(ManagedAreaName, ParameterName, HabitatType)) %>% 
        rename("Shell Type" = "ShellType",
               "Size Class" = "SizeClass",
               "Trend Status" = "TrendStatus",
               "Estimate" = "ModelEstimate",
               "Standard Error" = "StandardError",
               "Credible Interval" = "CredibleInterval")
    } else {
      oy_trends %>% filter(ManagedAreaName==ma, ParameterName==param) %>% 
        select(-c(ManagedAreaName, ParameterName, SizeClass)) %>%
        rename("Shell Type" = "ShellType",
               "Habitat Type" = "HabitatType",
               "Trend Status" = "TrendStatus",
               "Estimate" = "ModelEstimate",
               "Standard Error" = "StandardError",
               "Credible Interval" = "CredibleInterval")
    }
  }
}

# Shows modalDialog plots 
showPlot <- function(type, ma, ma_short, h){
  src <- MA_All[Abbreviation==ma_short, get(type)]
  hab_plot_types <- plot_df[habitat==h, unique(plot_type)]
  title <- paste0(plot_df[plot_type==type, title], ma)
  
  # List of plot types that don't need tables
  plots_sans_tables <- c("barplot_sp")
  
  if(type %in% hab_plot_types) {
    # Add trend tables for necessary plots only
    if(!type %in% plots_sans_tables){
      addTable <- tagList(
        tags$h4("Trend Results Table"),
        tags$div(tableOutput("trendTable"),style = "margin: 0 auto;")
      )
    } else {addTable <- ""}
    showModal(
      modalDialog(
        title = title,
        tags$div(
          tags$img(
            src = src,
            alt = plot_df[plot_type==type, alt],
            height = "100%",
            width = "100%"
          ),
          addTable,
          style = "text-align:center;"
        ),
        size = "l",
        easyClose = T
      )
    )
  }
}

# SEACAR Plot Theme
plot_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="Arial"),
        plot.title=element_text(hjust=0.5, size=12, color="#314963"),
        plot.subtitle=element_text(hjust=0.5, size=10, color="#314963"),
        legend.title=element_text(size=10),
        legend.text.align = 0,
        axis.title.x = element_text(size=10, margin = margin(t = 5, r = 0,
                                                             b = 10, l = 0)),
        axis.title.y = element_text(size=10, margin = margin(t = 0, r = 10,
                                                             b = 0, l = 0)),
        axis.text=element_text(size=10),
        axis.text.x=element_text(angle = -45, hjust = 0))
# SEACAR Palettes
seacar_palette <- c("#964059","#E05E7B","#E98C86","#F1B8AB","#F8CAAA","#F8E6B9",
                    "#FEEEE1","#DAE9DA","#8BE4C2","#7EE7E8","#8FD0EC","#6FA1DD",
                    "#889BD1","#8F83D3","#6B59AB")

seacar_sp_palette <- c("#005396","#0088B1","#00ADAE","#65CCB3","#AEE4C1",
                       "#FDEBA8","#F8CD6D","#F5A800","#F17B00")


rds_to_load <- c("data_directory", "allMapData", "MA_All", "plot_df",
                 "publish_date", "sav_trends")
for(file in rds_to_load){
  eval(call("<-", as.name(file), readRDS(paste0("rds/",file,".rds"))))
}

# Show SE Region only?
se_region <- MA_All[Region=="SE", unique(ManagedAreaName)]
allMapData <- allMapData[ManagedAreaName %in% se_region, ]

# Mapping ----
# create map objects at start to make maps more efficiently within dashboard
map <- leaflet() %>% addProviderTiles(providers$CartoDB.PositronNoLabels)
allMap <- map

# Setting up palettes for both PieChart and Map
habitats <- c("sav" = "Submerged Aquatic Vegetation", "oyster" = "Oyster Reef",
              "coral" = "Coral Reef", "cw" = "Coastal Wetlands", "nekton" = "Nekton")
habPal <- colorFactor(seacar_sp_palette, habitats, reverse = T)

# Generate data for Overview PieChart
pieData <- setDT(allMapData %>% group_by(habitat) %>% summarise(numPrograms = length(unique(ProgramID))))
setorder(pieData, -numPrograms)

# PieChart palette
piePal <- habPal(habitats)
names(piePal) <- habitats

for(h in habitats){
  mapData <- allMapData[habitat==h, ]
  pal <- data_directory[[h]][["pal"]]
  map <- map %>%
    addCircleMarkers(
      data = mapData,
      lat = mapData$lat, lng = mapData$lon, fillColor = pal(mapData$ProgramID),
      rad = mapData$rad, weight = 0.6, fillOpacity = 0.6, group = h, color = "black",
      opacity = 0.2, popup = mapData$popup, label = mapData$label)
  allMap <- allMap %>%
    addCircleMarkers(
      data = mapData %>% group_by(ProgramLocationID) %>%
        distinct(lat, lon, habitat),
      lat = mapData$lat, lng = mapData$lon, fillColor = habPal(h), rad = 2.5,
      weight = 0.6, fillOpacity = 0.6, group = h, color = "black",
      opacity = 0.4)
}

# Add map layer controls (for testing)
map <- map %>%
  addLayersControl(overlayGroups = unname(habitats),
                   options = layersControlOptions(collapsed=TRUE))

# Shiny Server ----
server <- function(input, output, session){
  plot_type <- reactiveVal()
  
  observeEvent(input$habitatSelect, {
    habitat <- input$habitatSelect
    
    leafletProxy("leafletMap") %>%
      showGroup(habitat) %>%
      hideGroup(unname(habitats[str_detect(habitats, habitat, negate = T)]))
    
    # ProgramName is ID used on back-end. names() displays format "PID - PName"
    progs <- unique(data_directory[[input$habitatSelect]][["overviewTable"]]$ProgramName)    
    names(progs) <- unique(data_directory[[input$habitatSelect]][["overviewTable"]]$pNameID)
    
    updateSelectizeInput(inputId = "programSelect",
                         choices = c("All",progs))
    
    updateSelectizeInput(inputId = "maSelect",
                         choices = c("All",unique(data_directory[[input$habitatSelect]][["maSummTable"]]$ManagedAreaName)))
  })
  
  observeEvent(input$habitatCheckBox, {
    leafletProxy("allMap") %>%
      showGroup(input$habitatCheckBox) %>%
      hideGroup(unname(habitats[!habitats %in% input$habitatCheckBox]))
  })
  
  observe({
    leafletProxy("allMap") %>%
      showGroup(input$habitatCheckBox) %>%
      hideGroup(unname(habitats[!habitats %in% input$habitatCheckBox]))
  })
  
  habitat <- reactive({input$habitatSelect})
  
  pid <- reactive({input$programSelect})
  
  ma <- reactive({input$maSelect})
  
  param <- reactive({input$paramSelect})
  
  output$paramPlot <- renderPlot(plotProgramParams(habitat()))
  
  output$programPlot <- renderPlot(plotProgramYears(habitat()))
  
  output$pieChart <- renderBillboarder(
    billboarder() %>% 
      bb_piechart(pieData) %>% 
      bb_pie(label = list(format = htmlwidgets::JS("function(value) {return (value);}"),
                          threshold = 0.01)) %>%
      bb_tooltip(format = list(
        name = htmlwidgets::JS("function(name, ratio, id, index) {return 'Number of programs';}"),
        value = htmlwidgets::JS("d3.format(',')")
      )) %>%
      bb_legend(position='bottom') %>%
      bb_labs(title = "Number of Programs by Habitat") %>%
      bb_colors_manual(piePal)
  )
  
  output$leafletMap <- renderLeaflet(
    map %>% 
      showGroup("Submerged Aquatic Vegetation") %>% 
      hideGroup(unname(habitats[!habitats %in% "Submerged Aquatic Vegetation"]))
  )
  
  output$allMap <- renderLeaflet(allMap %>% hideGroup(unname(habitats)))
  
  output$summTable <- renderTable(displaySummaryTable(habitat(), pid(), type="program")) 
  
  output$maSummTable <- renderTable(displaySummaryTable(habitat(), ma(), type="ma"))
  
  output$programOverviewTable <- DT::renderDT({
    
    data <- setDT(displayOverviewTable(habitat(), pid(), type="program"))
    
    DT::datatable(data[, -c("ProgramName", "pNameID")], escape = F, selection = "none", rownames = F, 
                  style = "bootstrap", options = list(paging = T))
  })
  
  output$maOverviewTable <- DT::renderDT({
    
    data <- setDT(displayOverviewTable(habitat(), ma(), type="ma") %>% 
                    filter(!is.na(ManagedAreaName)))
    
    if(!ma()=="All"){data <- data[, -c("ManagedAreaName")]}
    
    DT::datatable(data, escape = F, selection = "none", rownames = F, 
                  style = "bootstrap", options = list(paging = T))
  })
  
  output$programInfo <- renderUI({
    if(!pid()=="All"){
      programInfo <- data_directory[[habitat()]][["MAPrograms"]] %>% 
        filter(ProgramName==pid())
      tagList(
        tags$a(href=paste0("https://data.florida-seacar.org/programs/details/",
                           unique(programInfo$ProgramID)), pid(), target="_blank")
      )
    }
  })
  
  output$managedAreaInfo <- renderUI({
    if(!ma()=="All"){
      areaID <- MA_All[ManagedAreaName==ma(), AreaID]
      tagList(
        tags$a(href=paste0("https://dev.florida-seacar.org/managedareas/details/",
                           areaID), ma(), target="_blank")
      )
    }
  })
  
  output$maPrograms <- renderUI({
    if(!ma()=="All"){
      progs <- data_directory[[habitat()]][["MAPrograms"]] %>% 
        filter(ManagedAreaName==ma()) %>%
        pull(pNameID)
      div(tags$b("SEACAR ProgramID - ProgramName"), 
          tags$br(),
          tagList(tags$ul(purrr::map(progs, function(.x) tags$li(.x)))))
    }
  })
  
  output$programMAs <- renderUI({
    if(!pid()=="All"){
      div(tags$b("Office of Resilience and Coastal Protection Managed Areas"), tags$br(),
          paste(unique(data_directory[[habitat()]][["MAPrograms"]] %>% 
                         filter(ProgramName==pid()) %>%
                         pull(ManagedAreaName)),
          collapse=", "))
    }

  })
  
  output$habitatDescription <- renderUI({
    div(style='max-width: fit-content; margin-left: auto; margin-right: auto;',
        tags$h3(habitat()),
        tags$p(habitatText(habitat())))
  })
  
  output$programBoxes <- renderUI({
    params <- plotProgramParams(habitat(), "data")
    lapply(1:nrow(params), function(i){
      valueBox(value = paste0(params[i, "n"]),
               subtitle = params[i, "ParameterName"],
               width = 4)
    })
  })
  
  output$plotLinks <- renderUI({
    if(!ma() == "All"){
      #Abbreviated MA name
      ma_short <- MA_All[ManagedAreaName == ma(), Abbreviation]
      # Get list of plottypes
      hab_plot_types <- plot_df[habitat==habitat(), unique(plot_type)]
      # Filtered MA_All to check for plot availability
      filtered_MA_All <- MA_All[ManagedAreaName==ma(), ]
      # check plot_type availability for a given MA
      available_plot_types <- hab_plot_types[
        sapply(hab_plot_types, function(plot) {
          plot_column <- filtered_MA_All[[plot]]
          # Check if any value is not "FALSE"
          any(plot_column != "FALSE")
        })
      ]
      
      out_list <- 
        lapply(available_plot_types, function(plot) {
          plot_check <- MA_All[ManagedAreaName == ma(), get(plot)]
          if (plot_check != "FALSE") {
            actionButton(paste0(ma_short, "__", plot),
                         label = plot_df[plot_type==plot, label],
                         onclick = 'Shiny.onInputChange("select_button", this.id);')
          }
        })
      
      tagList(
        tags$h4("Trends and Visualizations"),
        tags$ul(
          purrr::map(out_list, function(.x) tags$li(.x))
          )
        )
    } else {
      renderText("Links displayed here")
    }
  })
  
  observeEvent(input$select_button, {
    ma_short <- str_split(input$select_button, "__")[[1]][[1]]
    type <- str_split(input$select_button, "__")[[1]][[2]]
    plot_type(type)
    
    showPlot(type=type, ma=ma(), ma_short=ma_short, h=habitat())
    
  })
  
  output$trendTable <- renderTable(
    trendTables(h=habitat(), ma=ma(), plot_type = plot_type())
  )
  
  observeEvent(input$pieChart_click, {
    updateCheckboxGroupInput(session = session,
                             inputId = "habitatCheckBox",
                             selected = input$pieChart_click$id)
    nav_select(id = "habInfo", selected = input$pieChart_click$id)
  })
  
  output$discretePrograms <- renderTable(discretePrograms[ParameterName==param(), ])
  
  output$discreteProgramPlot <- renderPlot({
    ggplot(discProgramParams, 
           aes(x=0, xend=n, y=ParameterName, yend=ParameterName)) +
      geom_segment(linewidth=10, colour="#4472C4") +
      geom_text(aes(x=n, label=n, hjust=-0.3), color="black") + 
      labs(title="Number of Programs for each Parameter",
           x="Number of Programs",
           y="Parameter") +
      plot_theme
  })
  
  output$funding <- renderUI({HTML(funding_text)})
  
}