# source("process_data.R")

source("UI.R")
source("Server.R")

shinyApp(ui = ui, server = server)

# library(rsconnect)
# deployApp(appFiles = c("app.R", "UI.R", "Server.R", "UI_Snippets.R",
#                        "rds/",
#                        "www/style.css", "www/dep-logos.png", "www/figures/",
#                        "www/trendarrows/", "www/google-analytics.html"))