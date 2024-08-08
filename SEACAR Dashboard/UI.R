library(bslib)

# Enables recognition of images folder
addResourcePath(prefix="www", directoryPath = "www")

header <- dashboardHeader(disable=T)
sidebar <- dashboardSidebar(disable=T)

splashPage <- dashboardBody(
  fluidRow(
    column(4, 
           billboarderOutput("pieChart")
           # tags$p("Click on a bar to get the value:"),
           # verbatimTextOutput("res_click")
    ),
    column(8, 
           div(tags$b("Map of Sampling Locations by Habitat")),
           leafletOutput("allMap"),
           checkboxGroupInput("habitatCheckBox", 
                              label = "Toggle habitat sampling locations",
                              choiceValues = unname(habitats),
                              choiceNames = unname(habitats),
                              selected = "Submerged Aquatic Vegetation", 
                              inline = TRUE))
  )
)

habitatPage <-
  dashboardBody(
    # useShinyjs(),
    # shinyjs::hidden(),
    fluidRow(
      column(4,
             selectInput(inputId = "habitatSelect",
                         label = "Select Habitat to view",
                         choices = unname(habitats),
                         selected = "Submerged Aquatic Vegetation")),
      column(8, 
             uiOutput("habitatDescription"))

    ),
    fluidRow(
      column(4,
             plotOutput("programPlot")),
      column(4,
             tags$h4("Number of Programs by Parameter"),
             splitLayout(uiOutput("programBoxes"))
             ),
      column(4,
             leafletOutput("leafletMap"))
    ),
    fluidRow(
      tabsetPanel(
        tabPanel(
          "By Managed Area",
          column(4,
                 wellPanel(
                   selectizeInput(inputId = "maSelect",
                                   label = "Select a Managed Area",
                                   choices = "Select a Managed Area"),
                   textOutput("managedAreaInfo"),
                   tableOutput("maSummTable"),
                   uiOutput("maPrograms")
                  )),
          column(2,
                 uiOutput("plotLinks")),
          column(6,
                 DT::DTOutput("maOverviewTable"))),      
        tabPanel(
          "By Program",
           column(5,
                  wellPanel(
                    selectizeInput(inputId = "programSelect",
                                   label = "Select a Program",
                                   choices = "Select a Program",
                                   selected = "All"),
                    textOutput("programInfo"),
                    tableOutput("summTable"),
                    uiOutput("programMAs")
                  )),
           column(7,
                  DT::DTOutput("programOverviewTable"))
        )
      )
    )
  )

wqDiscretePage <-
  dashboardBody(
    # useShinyjs(),
    # shinyjs::hidden(),
    fluidRow(
      selectInput(inputId = "paramSelect",
                  label = "Select Parameter to view",
                  choices = unique(grouped_df$ParameterName),
                  selected = "Dissolved Oxygen")
    ),
    fluidRow(
      column(5,
             plotOutput("discreteProgramPlot"),
             tableOutput("discretePrograms")
             ),
      column(7,
             # leafletOutput("discreteMap")
             )
    ),
  )

# Shiny UI ----
ui <- navbarPage(
  "SEACAR Dashboard",
  tabPanel("Overview",
           dashboardPage(header, sidebar, splashPage)),
  tabPanel("Habitats",
           fluidPage(
             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")
             ),
             dashboardPage(header, sidebar, habitatPage))),
  navbarMenu("Water Quality / Clarity / Nutrients",
             tabPanel("Discrete WQ",
                      fluidPage(dashboardPage(header, sidebar, wqDiscretePage))),
             tabPanel("Continuous WQ")),
  nav_spacer(),
  nav_item(
    tags$a(icon("github"), " SEACAR GitHub", href="https://github.com/FloridaSEACAR/", target="_blank")),
  footer = funding()
)