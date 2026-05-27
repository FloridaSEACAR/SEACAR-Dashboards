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

link_github <- tags$a(icon("github"), " Florida SEACAR GitHub", href="https://github.com/FloridaSEACAR/", target="_blank")
link_ddi <- tags$a(icon("database"), " Florida SEACAR Data Discovery", href="https://data.florida-seacar.org/", target="_blank")
link_wqcont <- tags$a(icon("chart-line"), " Florida SEACAR Continuous WQ Dashboard", href="https://floridaapdata.shinyapps.io/continuous_wq/", target="_blank")

overview_page <- list(
  fluidRow(
    fluidRow(
      column(4,
             tags$h3("Overview", style = "padding-top:0px;")),
      column(8,
             tags$p(overview_text, tags$h5("Quick Links:"),
                    style = "padding-top:0px;"),
             tags$div(tags$ul(tags$li(link_github), tags$li(link_ddi), tags$li(link_wqcont)), style = "margin-left: 15px;"))
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

ui <- tagList(
  # Pseudo-navbar
  tags$div(
    class = "pseudo-navbar",
    tags$div(
      class = "pseudo-navbar-left",
      tags$a(
        href = "https://floridaseacar.github.io/",
        target = "_blank",
        class = "brand",

        tags$img(
          src = "https://floridaseacar.github.io/images/logo-dep-color-small.png",
          class = "logo"
        ),

        tags$div(
          class = "brand-text",
          tags$span(class = "title", "Florida SEACAR"),
          tags$br(),
          tags$span(class = "subtitle", "Habitat Dashboard")
        )
      )
    ),
    tags$div(
      class = "pseudo-navbar-right",
      tags$a(actionButton("feedbackButton", "Feedback", icon("plus"), class = "btn btn-primary", style = "background-color:#53B7E8;"),
             href = "mailto:SEACAR@FloridaDEP.gov?subject=Feedback for SEACAR Dashboard", style = "padding-right:20px;"),
      tags$a(
        href = "https://github.com/FloridaSEACAR",
        target = "_blank",

        tags$img(
          src = "https://floridaseacar.github.io/images/GitHub_Invertocat_Light.png",
          class = "icon"
        )
      )
    )
  ),
  fluidPage(
    title = "SEACAR Dashboard",
    tags$style(
      HTML(
        "
          .pseudo-navbar {
            display: flex;
            justify-content: space-between;
            align-items: center;
            background-color: #045273;
            padding: 8px 16px;
            color: white;
            margin-bottom:20px;
          }

          .pseudo-navbar .brand {
            display: flex;
            align-items: center;
            text-decoration: none;
            color: white;
          }

          .pseudo-navbar .logo {
            height: 55px;
            margin-right: 10px;
          }

          .brand-text .title {
            font-size: 18px;
            font-weight: 500;
          }

          .brand-text .subtitle {
            font-size: 12px;
            color: #C8EAFB;
            font-weight: 200;
          }

          .pseudo-navbar .icon {
            height: 30px;
            width: 30px;
          }

          .pseudo-navbar a:hover {
            opacity: 0.85;
          }
        "
      )
    ),
    useShinyjs(),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")),
    # Include google analytics tracking
    tags$head(includeHTML("www/google-analytics.html")),
    overview_page,
    habitats_page,
    fluidRow(
      funding()
    )
  )
)
