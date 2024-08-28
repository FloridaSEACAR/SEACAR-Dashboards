# source("load_shape_samples.R")
# source("seacar_data_location.R")
# source("process_data.R")

source("UI.R")
source("Server.R")

shinyApp(ui = ui, server = server)

# library(rsconnect)
# deployApp(appFiles = c("app.R", "UI.R", "Server.R", "UI_Snippets.R",
#                        "rds/allMapData.rds", "rds/data_directory.rds",
#                        "rds/MA_All.rds", "rds/plot_df.rds", "rds/publish_date.rds",
#                        "www/style.css", "www/dep-logos.png", "www/figures/"))