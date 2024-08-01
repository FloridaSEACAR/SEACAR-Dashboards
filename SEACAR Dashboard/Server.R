# Shiny Server ----
server <- function(input, output, session){
  
  observeEvent(input$habitatSelect, {
    habitat <- input$habitatSelect
    
    leafletProxy("leafletMap") %>%
      showGroup(habitat) %>%
      hideGroup(unname(habitats[str_detect(habitats, habitat, negate = T)]))
    
    updateSelectizeInput(inputId = "programSelect",
                         choices = c("All",unique(data_directory[[input$habitatSelect]][["overviewTable"]]$ProgramName)))
    
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
  
  output$paramPlot <- renderPlot(plotProgramParams(habitat()))
  
  output$programPlot <- renderPlot(plotProgramYears(habitat()))
  
  output$pieChart <- renderBillboarder(
    billboarder() %>% 
      bb_piechart(pieData) %>% 
      bb_pie(label = list(format = htmlwidgets::JS("function(value) {return (value);}"))) %>%
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
    
    DT::datatable(data[, -c("ProgramName")], escape = F, selection = "none", rownames = F, 
                  style = "bootstrap", options = list(paging = T))
  })
  
  output$maOverviewTable <- DT::renderDT({
    
    data <- setDT(displayOverviewTable(habitat(), ma(), type="ma"))
    
    DT::datatable(data[, -c("ManagedAreaName")], escape = F, selection = "none", rownames = F, 
                  style = "bootstrap", options = list(paging = T)
    )
  })
  
  output$programInfo <- renderText({
    pid()
  })
  
  # output$paramBoxes <- renderUI({
  #   params <- plotProgramParams(habitat(), "data")
  #   lapply(1:nrow(params), function(i){
  #     valueBox(value = paste0(params[i, "n"]),
  #              subtitle = params[i, "ParameterName"],
  #              width = 2, icon = icon(icon_df[param==params[i, 1], unique(icon)]))
  #   })
  # })
  
  output$plotLinks <- renderUI({
    multiplot <- ifelse(MA_All[ManagedAreaName==ma(), multiplot]=="FALSE", FALSE, TRUE)
    if(multiplot){
      showModal(
        modalDialog(
          title = "Plots",
          renderPlot(readRDS(MA_All[ManagedAreaName==ma(), multiplot]))
        ))
      renderText("multiplot LINK")
    }
  })
  
  output$res_click <- renderPrint({
    input$pieChart_click$id
  })
  
  observeEvent(input$pieChart_click, {
    updateCheckboxGroupInput(session = session,
                             inputId = "habitatCheckBox",
                             selected = input$pieChart_click$id)
    nav_select(id = "habInfo", selected = input$pieChart_click$id)
  })
  
  # output$discretePrograms <- renderTable(grouped_df)
  
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