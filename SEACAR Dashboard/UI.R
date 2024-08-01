library(bslib)

# Enables recognition of images folder
addResourcePath(prefix="www", directoryPath = "www")
# Text to declare funding
funding_text <- paste(
  tags$div("Funding Acknowledgement", style="text-align:center; font-weight:bold;"),
  
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

funding <- function(){
  tags$div(wellPanel(htmlOutput("funding")), 
           style = 'width=80%;')
}

# div(style='max-width: fit-content; margin-left: auto; margin-right: auto;', "")

savContent <- function(){
  div(style='max-width: fit-content; margin-left: auto; margin-right: auto;',
      h3("Submerged Aquatic Vegetation"),
      p("Submerged aquatic vegetation (SAV) refers to plants and plant-like 
        macroalgae species that live entirely underwater. The most species-rich 
        categories of SAV inhabiting Florida estuaries are benthic macroalgae and 
        seagrasses which often grow together in dense beds or meadows that carpet the 
        seafloor. Macroalgae include multicellular species of green, red and brown 
        algae that often live attached to the substrate by a root-like structure 
        called a holdfast. They grow quickly and tolerate relatively high nutrient levels, 
        making them a threat to seagrasses and other benthic habitats in areas with 
        poor water quality. One of the most common types of macroalgae in Florida SAV 
        communities are species of the genus Caulerpa.")
  )
}

habitatBadges <- function(){
  navset_card_tab(
    nav_panel(div(img(src="sav_badge.png")),
              "SAV Content"),
    nav_panel(div(img(src="wc_badge.png")),
              "WC Content",),
    nav_panel(div(img(src="coral_badge.png")),
              "Coral Content"),
    nav_panel(div(img(src="oyster_badge.png")),
              "Oyster Content"),
    nav_panel(div(img(src="cw_badge.png")),
              "CW Content")        
  )
}

habitatContent <- function(){
  navset_card_tab(id="habInfo",
    nav_panel("Submerged Aquatic Vegetation",
              savContent(),
              value = "Submerged Aquatic Vegetation"),
    nav_panel("Nekton",
              "Nekton Content",
              value = "Nekton"),
    nav_panel("Coral/Coral Reef",
              "Coral Content",
              value = "Coral/Coral Reef"),
    nav_panel("Oyster/Oyster Reef",
              "Oyster Content",
              value = "Oyster/Oyster Reef"),
    nav_panel("Coastal Wetlands",
              "CW Content",
              value = "Coastal Wetlands")
  )
}

header <- dashboardHeader(disable=T)
sidebar <- dashboardSidebar(disable=T)

splashPage <- dashboardBody(
  fluidRow(
    column(4, 
           billboarderOutput("pieChart"),
           # tags$p("Click on a bar to get the value:"),
           # verbatimTextOutput("res_click")
    ),
    column(8, 
           leafletOutput("allMap"),
           checkboxGroupInput("habitatCheckBox", 
                              label = "Toggle habitat sampling locations",
                              choiceValues = unname(habitats),
                              choiceNames = unname(habitats),
                              selected = "Submerged Aquatic Vegetation", 
                              inline = TRUE))
  ),
  fluidRow(
    column(3),
    column(6,
           # habitatBadges()),
           habitatContent()),
    column(3)
  ),
)

habitatPage <-
  dashboardBody(
    # useShinyjs(),
    # shinyjs::hidden(),
    fluidRow(
      selectInput(inputId = "habitatSelect",
                  label = "Select Habitat to view",
                  choices = unname(habitats),
                  selected = "Submerged Aquatic Vegetation")
    ),
    fluidRow(
      column(4,
             plotOutput("programPlot")),
      column(8,
             leafletOutput("leafletMap"))
    ),
    fluidRow(
      tabsetPanel(
        tabPanel("By Program",
                 column(5,
                        wellPanel(
                          selectizeInput(inputId = "programSelect",
                                         label = "Select a Program",
                                         choices = "Select a Program",
                                         selected = "All"),
                          textOutput("programInfo"),
                          tableOutput("summTable")
                        )),
                 column(7,
                        DT::DTOutput("programOverviewTable"))
        ),
        tabPanel("By Managed Area",
                 column(5,
                        wellPanel(
                          selectizeInput(inputId = "maSelect",
                                         label = "Select a Managed Area",
                                         choices = "Select a Managed Area"),
                          tableOutput("maSummTable")
                          # uiOutput("plotLinks")
                        )),
                 column(7,
                        DT::DTOutput("maOverviewTable")))
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
             plotOutput("discreteProgramPlot")
             # tableOutput("discretePrograms")
             ),
      column(7,
             # leafletOutput("discreteMap")
             )
    ),
  )

# Shiny UI ----
ui <- navbarPage("SEACAR Dashboard",
                 tabPanel("Overview",
                          dashboardPage(header, sidebar, splashPage)),
                 tabPanel("Habitats",
                          fluidPage(dashboardPage(header, sidebar, habitatPage))),
                 navbarMenu("Water Quality / Clarity / Nutrients",
                            tabPanel("Discrete WQ",
                                     fluidPage(dashboardPage(header, sidebar, wqDiscretePage))),
                            tabPanel("Continuous WQ")
                          ),
                 nav_spacer(),
                 nav_item(
                   tags$a(icon("github"), " SEACAR GitHub", href="https://github.com/FloridaSEACAR/", target="_blank")
                 ),
                 footer = funding()
                 )

# ui <- navbarPage("SEACAR Dashboard",
#                  tabPanel("Overview",
#                           dashboardPage(header, sidebar, splashPage)),
#                  tabPanel("Submerged Aqatic Vegetation",
#                           dashboardPage(header, sidebar, habitatPage)),
#                  tabPanel("Oyster/Oyster Reef",
#                           dashboardPage(header, sidebar, habitatPage)),
#                  tabPanel("Coral/Coral Reef",
#                           dashboardPage(header, sidebar, habitatPage)),
#                  tabPanel("Coastal Wetlands",
#                           dashboardPage(header, sidebar, habitatPage)),
#                  tabPanel("Nekton",
#                           dashboardPage(header, sidebar, habitatPage)))