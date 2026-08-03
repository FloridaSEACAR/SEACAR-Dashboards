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

source("UI.R", local = TRUE)
source("Server.R", local = TRUE)

shinyApp(ui = ui, server = server)

# rsconnect::deployApp(appFiles = c("app.R", "UI.R", "Server.R",
#                                   "rds/", "README.md", "www/"))