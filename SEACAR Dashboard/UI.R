library(shiny)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(bslib)
library(dplyr)
library(data.table)
library(stringr)
library(ggplot2)
library(purrr)
library(sf)
library(billboarder)
library(shinythemes)
# library(DT)

source("UI_Snippets.R")

habitats <- c("sav" = "Submerged Aquatic Vegetation", "oyster" = "Oyster Reef",
              "coral" = "Coral Reef", "cw" = "Coastal Wetlands", "nekton" = "Water Column (Nekton)")

# Enables recognition of images folder
addResourcePath(prefix="www", directoryPath = "www")

# habitatPage <-
#   dashboardBody(
#     useShinyjs(),
#     fluidRow(
#       tags$h3("Overview", style = "padding-left:10px;"),
#       tags$p("Overview text below", style = "padding-left:10px;"),
#       column(4, 
#              billboarderOutput("pieChart")
#              # tags$p("Click on a bar to get the value:"),
#              # verbatimTextOutput("res_click")
#       ),
#       column(8, 
#              div(tags$b("Map of Sampling Locations by Habitat")),
#              leafletOutput("allMap"),
#              checkboxGroupInput("habitatCheckBox", 
#                                 label = "Toggle habitat sampling locations",
#                                 choiceValues = unname(habitats),
#                                 choiceNames = unname(habitats),
#                                 selected = "Submerged Aquatic Vegetation", 
#                                 inline = TRUE))
#     ),
#     fluidRow(
#       tags$h3("Habitats", style = "padding-left:10px;"),
#       column(4,
#              selectInput(inputId = "habitatSelect",
#                          label = "Select Habitat to view",
#                          choices = unname(habitats),
#                          selected = "Submerged Aquatic Vegetation")),
#       column(8, 
#              uiOutput("habitatDescription"))
# 
#     ),
#     fluidRow(
#       column(4,
#              plotOutput("programPlot")),
#       column(4,
#              tags$h4("Number of Programs by Parameter"),
#              splitLayout(uiOutput("programBoxes"))
#              ),
#       column(4,
#              leafletOutput("leafletMap"),
#              div(style = "max-width:fit-content;",
#                  shinyjs::hidden(checkboxInput("CHIMMP_checkbox",
#                                                label = HTML(paste0("Toggle CHIMMP", tags$sup("2"), " boundaries")), 
#                                                value = FALSE)),
#                  shinyjs::hidden(checkboxInput("OIMMP_checkbox",
#                                                label = HTML(paste0("Toggle OIMMP", tags$sup("1"), " boundaries")), 
#                                                value = FALSE))))
#     ),
#     fluidRow(
#       tabsetPanel(
#         tabPanel(
#           "By Managed Area",
#           column(4,
#                  wellPanel(
#                    selectizeInput(inputId = "maSelect",
#                                    label = "Select a Managed Area",
#                                    choices = "Select a Managed Area"),
#                    uiOutput("managedAreaInfo"),
#                    tableOutput("maSummTable"),
#                    uiOutput("maPrograms")
#                   )),
#           column(2,
#                  uiOutput("plotLinks")),
#           column(6,
#                  DT::DTOutput("maOverviewTable")
#                  # tableOutput("maOverviewTable")
#                  )), 
#         tabPanel(
#           "By Program",
#            column(5,
#                   wellPanel(
#                     selectizeInput(inputId = "programSelect",
#                                    label = "Select a Program",
#                                    choices = "Select a Program",
#                                    selected = "All"),
#                     uiOutput("programInfo"),
#                     tableOutput("summTable"),
#                     uiOutput("programMAs")
#                   )),
#            column(7,
#                   DT::DTOutput("programOverviewTable")
#                   # tableOutput("programOverviewTable")
#                   )
#         )
#       )
#     )
#   )

# Shiny UI ----
# ui <- navbarPage(
#   inverse = TRUE,
#   "SEACAR Dashboard",
#   # tabPanel("Overview",
#   #          dashboardPage(header, sidebar, splashPage)),
#   tabPanel("Habitats",
#            fluidPage(
#              tags$head(
#                tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")
#              ),
#              dashboardPage(header, sidebar, habitatPage))),
#   # navbarMenu("Water Quality / Clarity / Nutrients",
#   #            tabPanel("Discrete WQ",
#   #                     fluidPage(dashboardPage(header, sidebar, wqDiscretePage))),
#   #            tabPanel("Continuous WQ")),
#   nav_spacer(),
#   nav_item(tags$a("SEACAR Data Discovery", href="https://data.florida-seacar.org/", target="_blank")),
#   nav_item(tags$a(icon("github"), " SEACAR GitHub", href="https://github.com/FloridaSEACAR/", target="_blank")),
#   footer = funding()
# )

link_github <- tags$a(icon("github"), " SEACAR GitHub", href="https://github.com/FloridaSEACAR/", target="_blank")
link_ddi <- tags$a(icon("database"), " SEACAR Data Discovery", href="https://data.florida-seacar.org/", target="_blank")

overview_page <- list(
  fluidRow(
    fluidRow(
      column(4,
             tags$h3("Overview", style = "padding-top:0px;")),
      column(8,
             tags$p(overview_text, tags$h5("Quick Links:"), 
                    style = "padding-top:0px;"),
             tags$div(tags$ul(tags$li(link_github), tags$li(link_ddi)), style = "margin-left: 15px;"))
    ),
    tags$hr(),
    fluidRow(
      column(4,
             billboarderOutput("pieChart")),
      column(8,
             div(tags$b("Map of Sampling Locations by Habitat")),
             leafletOutput("allMap"),
             checkboxGroupInput(
               "habitatCheckBox",
               label = "Toggle habitat sampling locations",
               choiceValues = unname(habitats),choiceNames = unname(habitats),
               selected = "Submerged Aquatic Vegetation",
               inline = TRUE))
    ), 
    class = "bounding_box")
)

habitats_page <- list(
  fluidRow(
    fluidRow(
      tags$h3("Habitats", style = "padding-left:10px;"),
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
             leafletOutput("leafletMap"),
             div(style = "max-width:fit-content;",
                 shinyjs::hidden(checkboxInput("CHIMMP_checkbox",
                                               label = HTML(paste0("Toggle CHIMMP", tags$sup("2"), " boundaries")), 
                                               value = FALSE)),
                 shinyjs::hidden(checkboxInput("OIMMP_checkbox",
                                               label = HTML(paste0("Toggle OIMMP", tags$sup("1"), " boundaries")), 
                                               value = FALSE))))
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
                   uiOutput("managedAreaInfo"),
                   tableOutput("maSummTable"),
                   uiOutput("maPrograms")
                 )),
          column(2,
                 uiOutput("plotLinks")),
          column(6,
                 DT::DTOutput("maOverviewTable")
                 # tableOutput("maOverviewTable")
          )), 
        tabPanel(
          "By Program",
          column(5,
                 wellPanel(
                   selectizeInput(inputId = "programSelect",
                                  label = "Select a Program",
                                  choices = "Select a Program",
                                  selected = "All"),
                   uiOutput("programInfo"),
                   tableOutput("summTable"),
                   uiOutput("programMAs")
                 )),
          column(7,
                 DT::DTOutput("programOverviewTable")
                 # tableOutput("programOverviewTable")
          )
        )
      )
    ), class = "bounding_box_nocol")
)

ui <- fluidPage(
  title = "SEACAR Dashboard",
  # page_navbar(
  #   title = "SEACAR Dashboard",
  #   navbar_options = navbar_options(
  #     theme = "auto",
  #     bg = "#045273",
  #     underline = TRUE
  #   ),
  #   nav_spacer(),
  #   nav_menu(
  #     title = "Links",
  #     align = "right",
  #     nav_item(link_github),
  #     nav_item(link_ddi)
  #   )
  # ),
  useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")),
  # Include google analytics tracking
  tags$head(includeHTML("www/google-analytics.html")),
  fluidRow(
    column(4, titlePanel("SEACAR Dashboard")),
    column(7),
    column(1, 
           div(a(actionButton("feedbackButton", "Feedback", icon("plus"), class = "btn btn-primary"), 
                 href = "mailto:SEACAR@FloridaDEP.gov?subject=Feedback for SEACAR Dashboard"), style = "padding:20px; float: right;"))
  ),
  overview_page,
  habitats_page,
  fluidRow(
    funding()
  )
)