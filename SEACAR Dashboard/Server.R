showPlot <- function(type, ma, ma_short, h){
  src <- MA_All[Abbreviation==ma_short, get(type)]
  hab_plot_types <- plot_df[habitat==h, unique(plot_type)]
  title <- paste0(plot_df[plot_type==type, title], ma)
  
  if (type %in% hab_plot_types) {
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
          tags$p("Table caption text / trend results table here"),
          style = "text-align:center;"
        ),
        size = "m",
        easyClose = T
      )
    )
  }
}

# Shiny Server ----
server <- function(input, output, session){
  
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
                  style = "bootstrap", options = list(paging = T)
    )
  })
  
  output$programInfo <- renderText({if(!pid()=="All"){pid()}})
  
  output$managedAreaInfo <- renderText({if(!ma()=="All"){ma()}})
  
  output$maPrograms <- renderUI({
    if(!ma()=="All"){
      progs <- data_directory[[habitat()]][["MAPrograms"]] %>% 
        filter(ManagedAreaName==ma()) %>%
        mutate(pNameID = paste0(ProgramID, " - ", ProgramName)) %>%
        # mutate(link = paste0("tags$a(href='https://data.florida-seacar.org/programs/details/",ProgramID,"'",",",pNameID,")")) %>%
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
    ma_short <- MA_All[ManagedAreaName == ma(), Abbreviation]
    
    if(!ma() == "All"){
      hab_plot_types <- plot_df[habitat==habitat(), unique(plot_type)]
      
      out_list <- 
        lapply(hab_plot_types, function(plot) {
          plot_check <- MA_All[ManagedAreaName == ma(), get(plot)]
          if (plot_check != "FALSE") {
            actionButton(paste0(ma_short, "__", plot),
                         label = plot_df[plot_type==plot, label],
                         onclick = 'Shiny.onInputChange("select_button", this.id);')
          }
        })
      
      tagList(tags$ul(
        purrr::map(out_list, function(.x) tags$li(.x))
      ))
    } else {
      renderText("Links displayed here")
    }
  })
  
  observeEvent(input$select_button, {
    ma_short <- str_split(input$select_button, "__")[[1]][[1]]
    type <- str_split(input$select_button, "__")[[1]][[2]]
    
    showPlot(type=type, ma=ma(), ma_short=ma_short, h=habitat())
    
  })
  
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