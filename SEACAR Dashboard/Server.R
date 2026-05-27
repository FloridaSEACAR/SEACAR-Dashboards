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

# Trend tables display under plots for each habitat
trendTables <- function(h, ma, plot_type, hab_type = NULL){
  if(h=="Oyster Reef"){
    param <- ifelse(plot_type=="Oyster_Dens", "Density",
                    ifelse(plot_type=="Oyster_SH", "Shell Height","Percent Live"))
    df <- oy_trends
    if(plot_type=="Oyster_SH"){
      df %>% filter(ManagedAreaName==ma,
                    ParameterName==param,
                    !`Size Class`=="",
                    `Habitat Type`==hab_type) %>%
        select(-c(ManagedAreaName, ParameterName))
    } else {
      df %>% filter(ManagedAreaName==ma,
                    ParameterName==param,
                    `Habitat Type`==hab_type) %>%
        select(-c(ManagedAreaName, ParameterName, "Size Class"))
    }
  } else {
    if(h=="Submerged Aquatic Vegetation"){
      df <- sav_trends
    } else if(h=="Coral Reef"){
      if(plot_type=="Coral_SpeciesRichness"){
        df <- coral_sr_trends
      } else if(plot_type=="Coral_pc"){
        df <- coral_pc_trends
      }
    } else if(h=="Coastal Wetlands"){
      df <- cw_trends
    } else if(h=="Water Column (Nekton)"){
      df <- nekton_trends
    }
    df %>% filter(ManagedAreaName==ma) %>% select(-ManagedAreaName)
  }
}

get_tableDescription <- function(ma, h, plot_type){
  if(str_detect(h, "Oyster")){
    p <- ifelse(str_detect(plot_type, "_Dens"), "Density", ifelse(str_detect(plot_type, "_SH"), "Shell Height", "Percent Live"))
  } else if(str_detect(h, "Coastal")){
    p <- "Total/Canopy Percent Cover"
  } else if(str_detect(h, "Coral")){
    p <- ifelse(str_detect(plot_type, "_pc"), "Percent Cover", "Presence/Absence")
  } else if(str_detect(h, "Submerged")){
    if(str_detect(plot_type, "multiplot|trendplot")){
      p <- "Percent Cover"
    } else {return()}
  } else if(str_detect(h, "Nekton")){
    p <- "Presence/Absence"
  }
  TableDescriptions[ManagedAreaName==ma & HabitatName==h & ParameterName==p, Description]
}

# Shows modalDialog plots
showPlot <- function(type, ma, ma_short, h, hab_type = NULL){
  src <- MA_All[Abbreviation==ma_short, get(type)]
  hab_plot_types <- plot_df[habitat==h, unique(plot_type)]
  title <- paste0(plot_df[plot_type==type, title], ma)
  figCap <- plot_df[plot_type==type, FigureCaption]
  tableDesc <- get_tableDescription(ma = ma, h = h, plot_type = type)

  # List of plot types that don't need tables
  plots_sans_tables <- c("barplot_sp", "sav_wc")

  if(type %in% hab_plot_types) {
    # Add trend tables for necessary plots only
    if(!type %in% plots_sans_tables){
      addTable <- tagList(
        tags$h4("Trend Results Table"),
        tags$div(DT::DTOutput("trendTable"), style = "max-width: fit-content; margin-left: auto; margin-right: auto;")
      )
    } else {addTable <- ""}
    if(type=="sav_wc"){
      src <- str_split_1(MA_All[Abbreviation==ma_short, get(type)], ", ")
      nav_panels <- lapply(seq_along(src), function(i){
        p <- str_split_1(str_split_1(str_split_1(src[i], ma_short)[[2]], "_")[2], ".png")[1]
        # Convert from short-hand to full name
        if(p=="Chla"){p_long<-"Chlorophyll a"}
        else if(p=="Secchidepth"){p_long<-"Secchi depth"}
        else if(p=="CDOM"){p_long <- "Colored Dissolved Organic Matter"}
        else if(p=="TSS"){p_long <- "Total Suspended Solids"}
        else if(p=="Turbidity"){p_long <- p}

        nav_panel(p_long,
                  tags$img(src = src[i],
                           height = "100%",
                           width = "100%"))
      })
      showModal(
        modalDialog(
          title = title,
          tags$div(
            do.call(navset_card_tab, c(nav_panels)),
            style = "text-align:center;"
          ),
          size = "l",
          easyClose = T
        )
      )
    } else if(str_detect(type, "Oyster")){
      src <- str_split_1(MA_All[Abbreviation==ma_short, get(type)], ", ")

      reef_values <- sapply(src, function(x){
        if(str_detect(x, "Natural")){
          "Natural"
        } else if(str_detect(x, "Restored")){
          "Restored"
        } else {
          NA_character_
        }
      })

      reef_labels <- ifelse(reef_values == "Natural", "Natural Reef",
                            ifelse(reef_values == "Restored", "Restored Reef", reef_values))

      selected_hab <- reef_values[!is.na(reef_values)][1]

      if(!is.null(hab_type)){
        hab_type(selected_hab)
      }

      nav_panels <- lapply(seq_along(src), function(i){
        nav_panel(
          title = reef_labels[i],
          value = reef_values[i],
          tags$img(src = src[i], height = "100%", width = "100%"),
          tags$div(tags$p(style = "color: #6c757d; text-align: left; font-size: 1.3rem; line-height: 1.1; font-weight:bold;", figCap))
        )
      })

      showModal(
        modalDialog(
          title = title,
          tags$div(HTML(tableDesc)),
          tags$div(
            do.call(
              navset_card_tab,
              c(nav_panels, list(id = "oyster_hab_type", selected = selected_hab))
            ),
            addTable,
            style = "text-align:center;"
          ),
          size = "l",
          easyClose = T
        )
      )
    } else {
      showModal(
        modalDialog(
          title = title,
          tags$div(HTML(tableDesc)),
          tags$div(
            tags$img(
              src = src,
              alt = plot_df[plot_type==type, alt],
              height = "100%",
              width = "100%"
            ),
            tags$div(tags$p(style = "color: #6c757d; text-align: left; font-size: 1.3rem; line-height: 1.1; font-weight:bold;", figCap)),
            addTable,
            style = "text-align:center;"
          ),
          size = "l",
          easyClose = T
        )
      )
    }
  }
}

# SEACAR Plot Theme
plot_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="Arial"),
        plot.title=element_text(hjust=0.5, size=12, color="#314963"),
        plot.subtitle=element_text(hjust=0.5, size=10, color="#314963"),
        legend.title=element_text(size=10, hjust=0),
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
                 "publish_date", "allTrendTables","oimmp", "chimmp",
                 "Database_Thresholds", "FigureCaptions", "TableDescriptions")
for(file in rds_to_load){
  eval(call("<-", as.name(file), readRDS(paste0("rds/",file,".rds"))))
}

# Unpack allTrendTables, create distinct objects for each
for(file in names(allTrendTables)){
  eval(call("<-", as.name(file), allTrendTables[[file]]))
}

# Mapping ----
# create map objects at start to make maps more efficiently within dashboard
map <- leaflet() %>% addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addMapPane("background", zIndex = 400) %>%
  addMapPane("foreground", zIndex = 500)
allMap <- map

# Setting up palettes for both PieChart and Map
habitats <- c("sav" = "Submerged Aquatic Vegetation", "oyster" = "Oyster Reef",
              "coral" = "Coral Reef", "cw" = "Coastal Wetlands", "nekton" = "Water Column (Nekton)")
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
      opacity = 0.2, popup = mapData$popup, label = mapData$label,
      options = pathOptions(pane = "foreground"))
  allMap <- allMap %>%
    addCircleMarkers(
      data = mapData %>% group_by(ProgramLocationID) %>%
        distinct(lat, lon, habitat),
      lat = mapData$lat, lng = mapData$lon, fillColor = habPal(h), rad = 2.5,
      weight = 0.6, fillOpacity = 0.6, group = h, color = "black",
      opacity = 0.4)
}

# Add OIMMP and CHIMMP boundaries
# Create palette to display boundaries
oimmp_pal <- colorFactor("Set3", oimmp$Region)
chimmp_pal <- colorFactor("Paired", chimmp$Region)

map <- map %>%
  leaflet::addPolygons(data=oimmp, color = "#F0F0F0", weight = 1, smoothFactor = 0.5,
                       opacity = 1.0, fillOpacity = 0.4, fillColor = ~oimmp_pal(Region),
                       group="OIMMP Boundary", options = pathOptions(pane = "background")) %>%
  leaflet::addPolygons(data=chimmp, color = "#F0F0F0", weight = 1, smoothFactor = 0.5,
                       opacity = 1.0, fillOpacity = 0.4, fillColor = ~chimmp_pal(Region),
                       group="CHIMMP Boundary", options = pathOptions(pane = "background")) %>%
  leaflet.extras::addFullscreenControl()

# Add fullscreen control to allMap, with ability to toggle layers
allMap <- allMap %>%
  addLayersControl(overlayGroups = c(unname(habitats)),
                   options = layersControlOptions(collapsed=TRUE))

# Add map layer controls (for testing)
map <- map %>%
  addLayersControl(overlayGroups = c(unname(habitats), "OIMMP Boundary", "CHIMMP Boundary"),
                   options = layersControlOptions(collapsed=TRUE))

# Shiny Server ----
server <- function(input, output, session){
  plot_type <- reactiveVal()
  hab_type <- reactiveVal(NULL)

  observeEvent(input$oyster_hab_type, {
    hab_type(input$oyster_hab_type)
  }, ignoreInit = TRUE)

  # Habitat selection change
  observeEvent(input$habitatSelect, {
    habitat <- input$habitatSelect
    # Show or hide toggle box for OIMMP and CHIMMP boundaries
    if(!habitat %in% c("Oyster Reef", "Coastal Wetlands")){
      shinyjs::hideElement("OIMMP_checkbox")
      shinyjs::hideElement("CHIMMP_checkbox")
    } else if(habitat=="Oyster Reef"){
      shinyjs::showElement("OIMMP_checkbox")
      shinyjs::hideElement("CHIMMP_checkbox")
    } else if(habitat=="Coastal Wetlands"){
      shinyjs::showElement("CHIMMP_checkbox")
      shinyjs::hideElement("OIMMP_checkbox")
    }

    leafletProxy("leafletMap") %>%
      showGroup(habitat) %>%
      hideGroup(unname(habitats[str_detect(habitats, fixed(habitat), negate = T)]))

    # ProgramName is ID used on back-end. names() displays format "PID - PName"
    progs <- unique(data_directory[[input$habitatSelect]][["overviewTable"]]$ProgramName)
    names(progs) <- unique(data_directory[[input$habitatSelect]][["overviewTable"]]$pNameID)

    updateSelectizeInput(inputId = "programSelect",
                         choices = c("All",progs))

    updateSelectizeInput(inputId = "maSelect",
                         choices = c("All",sort(unique(data_directory[[input$habitatSelect]][["maSummTable"]]$ManagedAreaName))))

    updateCheckboxGroupInput(session = session,
                             inputId = "habitatCheckBox",
                             selected = input$habitatSelect)
  })

  # OIMMP Boundary check box
  observeEvent(input$OIMMP_checkbox,{
    if(input$OIMMP_checkbox){
      leafletProxy("leafletMap") %>%
        showGroup("OIMMP Boundary")
    } else {
      leafletProxy("leafletMap") %>%
        hideGroup("OIMMP Boundary")
    }
  })

  # CHIMMP Boundary check box
  observeEvent(input$CHIMMP_checkbox,{
    if(input$CHIMMP_checkbox){
      leafletProxy("leafletMap") %>%
        showGroup("CHIMMP Boundary")
    } else {
      leafletProxy("leafletMap") %>%
        hideGroup("CHIMMP Boundary")
    }
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

  output$paramPlot <- renderPlot(plotProgramParams(habitat())) %>%
    bindCache(habitat())

  output$programPlot <- renderPlot(plotProgramYears(habitat())) %>%
    bindCache(habitat())

  output$pieChart <- renderBillboarder(
    billboarder(bb_opts = list(legend = list(item = list(onclick = htmlwidgets::JS("function(id) { return false; }"))))) %>%
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
      hideGroup(c(unname(habitats[!habitats %in% "Submerged Aquatic Vegetation"]),
                  "OIMMP Boundary", "CHIMMP Boundary"))
  )

  output$allMap <- renderLeaflet(allMap %>% hideGroup(unname(habitats)))

  output$summTable <- renderTable(displaySummaryTable(habitat(), pid(), type="program")) %>%
    bindCache(habitat(), pid())

  output$maSummTable <- renderTable(displaySummaryTable(habitat(), ma(), type="ma")) %>%
    bindCache(habitat(), ma())

  output$programOverviewTable <- DT::renderDT({

    data <- data.table::setDT(displayOverviewTable(habitat(), pid(), type="program"))

    DT::datatable(data[, -c("ProgramName", "pNameID")], escape = F, selection = "none",
                  rownames = F, style = "bootstrap", options = list(paging = T))
  }, server = FALSE) %>% bindCache(habitat(), pid())

  output$maOverviewTable <- DT::renderDT({

    data <- data.table::setDT(displayOverviewTable(habitat(), ma(), type="ma") %>%
                                filter(!is.na(ManagedAreaName)))

    if(!ma()=="All"){data <- data[, -c("ManagedAreaName")]}

    DT::datatable(data, escape = F, selection = "none",
                  rownames = F, style = "bootstrap", options = list(paging = T))
  }, server = FALSE) %>% bindCache(habitat(), ma())

  output$programInfo <- renderUI({
    if(!pid()=="All"){
      programInfo <- data_directory[[habitat()]][["MAPrograms"]] %>%
        filter(ProgramName==pid()) %>% select(-ShortName)
      tagList(
        tags$a(href=paste0("https://data.florida-seacar.org/programs/details/",
                           unique(programInfo$ProgramID)), pid(), target="_blank")
      )
    }
  }) %>% bindCache(habitat(), pid())

  output$managedAreaInfo <- renderUI({
    if(!ma()=="All"){
      areaID <- MA_All[ManagedAreaName==ma(), AreaID]
      tagList(
        tags$a(href=paste0("https://dev.florida-seacar.org/managedareas/details/",
                           areaID), ma(), target="_blank")
      )
    }
  }) %>% bindCache(ma())

  output$maPrograms <- renderUI({
    if(!ma()=="All"){
      progs <- data_directory[[habitat()]][["MAPrograms"]] %>%
        filter(ManagedAreaName==ma()) %>%
        pull(pNameID)
      div(tags$b("SEACAR ProgramID - ProgramName"),
          tags$br(),
          tagList(tags$ul(purrr::map(progs, function(.x) tags$li(.x)))))
    }
  }) %>% bindCache(habitat(), ma())

  output$programMAs <- renderUI({
    if(!pid()=="All"){
      div(tags$b("Office of Resilience and Coastal Protection Managed Areas"), tags$br(),
          paste(unique(data_directory[[habitat()]][["MAPrograms"]] %>%
                         filter(ProgramName==pid()) %>%
                         pull(ShortName)),
          collapse=", "))
    }
  }) %>% bindCache(pid())

  output$habitatDescription <- renderUI({
    div(style='max-width: fit-content; margin-left: auto; margin-right: auto;',
        tags$h3(habitat()),
        tags$p(habitatText(habitat())))
  }) %>% bindCache(habitat())

  output$programBoxes <- renderUI({
    params <- plotProgramParams(habitat(), "data")
    vbs <- lapply(1:nrow(params), function(i){
      bslib::value_box(value = paste0(params[i, "n"]),
                       title = params[i, "ParameterName"],
                       theme = value_box_theme(bg = "#2E5270", fg = "#C8EAFB"),
                       class = "vbox")
    })
    layout_column_wrap(
      width = "30%",
      fixed_width = TRUE,
      !!!vbs
    )
  }) %>% bindCache(habitat())

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
      renderUI({
        tagList(
          tags$h4("Trends and Visualizations"),
          tags$p("Select a Managed Area to view available visualizations...")
        )
      })
    }
  }) %>% bindCache(ma(), habitat())

  observeEvent(input$select_button, {
    ma_short <- str_split(input$select_button, "__")[[1]][[1]]
    type <- str_split(input$select_button, "__")[[1]][[2]]
    plot_type(type)
    showPlot(type=type, ma=ma(), ma_short=ma_short, h=habitat(), hab_type = hab_type)
  })

  output$trendTable <- DT::renderDT({
    trendTables(
      h = habitat(),
      ma = ma(),
      plot_type = plot_type(),
      hab_type = hab_type())
    }, escape = FALSE) # %>% bindCache(habitat(), ma(), plot_type(), hab_type())

  observeEvent(input$pieChart_click, {
    updateCheckboxGroupInput(session = session,
                             inputId = "habitatCheckBox",
                             selected = input$pieChart_click$id)
    updateSelectizeInput(session = session,
                         inputId = "habitatSelect",
                         selected = input$pieChart_click$id)
    nav_select(id = "habInfo", selected = input$pieChart_click$id)
  })

  # output$discretePrograms <- renderTable(discretePrograms[ParameterName==param(), ])
  #
  # output$discreteProgramPlot <- renderPlot({
  #   ggplot(discProgramParams,
  #          aes(x=0, xend=n, y=ParameterName, yend=ParameterName)) +
  #     geom_segment(linewidth=10, colour="#4472C4") +
  #     geom_text(aes(x=n, label=n, hjust=-0.3), color="black") +
  #     labs(title="Number of Programs for each Parameter",
  #          x="Number of Programs",
  #          y="Parameter") +
  #     plot_theme
  # })

  output$funding <- renderUI({HTML(funding_text)})

}
