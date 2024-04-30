library(data.table)
library(dplyr)
library(stringr)
library(leaflet)
library(xlsx)

source("seacar_data_location.R") # import data location variable

files <- list.files(seacar_data_location, full.names = TRUE)
cont_files <- str_subset(files, "_cont_") # Locate continuous files only

data_directory <- list() # Create directory to store grouped results
# Loop through files and create summary frames
for(file in cont_files){
  # Import data
  data <- fread(file, sep='|', na.strings = "NULL")
  # Find parameter name, region
  param <- unique(data$ParameterName)
  region <- unique(data$Region)
  # Group data by PLID & Year, compute N_Data, necessary info
  grouped_df <- data %>%
    group_by(ProgramLocationID, Year) %>%
    reframe(N_Data = n(),
            ProgramID = unique(ProgramID), 
            ProgramName = unique(ProgramName),
            ManagedAreaName = unique(ManagedAreaName),
            Region = unique(Region),
            Parameter = unique(ParameterName),
            Units = unique(ParameterUnits),
            Lat = unique(OriginalLatitude),
            Lon = unique(OriginalLongitude))
  # Append to data directory
  data_directory[[param]][[region]] <- grouped_df
  print(paste0("Processing ", param, " - ", region, " completed"))
}
# Store final results table
data <- data.table()
# Loop through each parameter, binding each region, combine into single file
for(p in names(data_directory)){
  data <- bind_rows(data, bind_rows(data_directory[[p]]))
}

# Read in excel file of ProgramIDs to be grouped into "Entities"
# AP Cont. WQ vs. SWMP
entities <- read.xlsx("data/SEACAR Program Matrix_ContinuousWQ_ProgramGroups.xlsx",
                      sheetName = "Sheet1", header=TRUE, startRow = 2, 
                      colIndex = 1:3)
# Rename columns, place ProgramName into Entity column if not already designated
entities <- entities %>% 
  rename(Entity = Group,ProgramID = ID,ProgramName = Program.Name)
setDT(entities)
entities[is.na(Entity), `:=` (Entity = ProgramName)]

# Merge "Entity" into dataframe
df <- merge(data, entities[ , c("Entity", "ProgramID")], 
            by="ProgramID", all=TRUE)
df <- df[!is.na(ProgramLocationID)]

# Create summarised dataframe for use in map
map_df <- df %>% group_by(ProgramLocationID, ProgramID, ProgramName, Entity) %>%
  summarise(years = list(sort(unique(Year))),
            params = list(unique(Parameter)), 
            YearMin = min(Year),
            YearMax = max(Year),
            N_Data = sum(N_Data),
            Lat = unique(Lat),
            Lon = unique(Lon), .groups = "keep") %>%
  mutate(popup = paste("ProgramID: ", ProgramID,
                       "<br> ProgramName: ", ProgramName,
                       "<br> ProgLocID: ", ProgramLocationID,
                       "<br> N_Data: ", N_Data,
                       "<br> Years: ", years,
                       "<br> Params: ", params),
         label = paste(ProgramID, ": ", ProgramName, 
                        " - ProgLocID: ", ProgramLocationID))

# Set palette and radius (bubble size)
pal <- colorFactor("viridis", map_df$Entity)
rad <- sqrt(map_df$N_Data)/500

# Create map object to load in shiny app
leaflet_map <- leaflet(map_df) %>%
  addTiles(group = "Default") %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, 
                   group = "Positron by CartoDB") %>%
  addCircleMarkers(lat=~Lat, lng=~Lon, color=~pal(Entity),
                   weight=1, radius=rad, fillOpacity=0.4,
                   popup = ~popup,
                   label = ~label)