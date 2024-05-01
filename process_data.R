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
df[is.na(Entity), `:=` (Entity = ProgramName)]
df <- df[!is.na(ProgramLocationID)]

# Create summarised dataframe for use in map
map_df <- df %>% group_by(ProgramLocationID, ProgramID, ProgramName, Entity) %>%
  summarise(years = list(sort(unique(Year))),
            params = list(unique(Parameter)), 
            YearMin = min(Year),
            YearMax = max(Year),
            N_Data = sum(N_Data),
            Lat = unique(Lat),
            Lon = unique(Lon), .groups = "keep")
setDT(map_df)
map_df[is.na(Entity), `:=` (Entity = ProgramName)]

# Set palette and radius (bubble size)
pal <- colorFactor("viridis", map_df$Entity)

# Create popup labels to display metadata info
map_df <- map_df %>%
  mutate(popup = paste("ProgramID: ", ProgramID,
                       "<br> ProgramName: ", ProgramName,
                       "<br> ProgLocID: ", ProgramLocationID,
                       "<br> N_Data: ", N_Data,
                       "<br> Years: ", years,
                       "<br> Params: ", params),
         label = paste(ProgramID, ": ", ProgramName, 
                       " - ProgLocID: ", ProgramLocationID),
         rad = sqrt(N_Data)/100)

# Add commas to large numbers for easier viewing
map_df$N_Data <- formatC(map_df$N_Data, format="d", big.mark = ",")

# Create map object to load in shiny app
leaflet_map <- leaflet(map_df) %>%
  addTiles(group = "Default") %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, 
                   group = "Positron by CartoDB") %>%
  addLayersControl(baseGroups = c("Default","Positron by CartoDB"),
                   options = layersControlOptions(collapsed=TRUE))

entities <- unique(map_df$Entity)

for(e in entities){
  leaflet_map <- leaflet_map %>%
    addCircleMarkers(data = map_df %>% filter(Entity==e), 
                     lat=~Lat, lng=~Lon, color=~pal(Entity),
                     weight=1, radius=~rad, fillOpacity=0.4,
                     popup = ~popup,
                     label = ~label,
                     group = e)
}

leaflet_map <- leaflet_map %>%
  addLayersControl(baseGroups = c("Default","Positron by CartoDB"),
                   overlayGroups = entities,
                   options = layersControlOptions(collapsed=TRUE))




# Gantt chart to show Entity timeline
program_years <- df %>%
  group_by(Entity) %>%
  reframe(Years = unique(Year)) %>%
  arrange(Entity, Years)
setDT(program_years)

# Function to find gaps in years
find_gaps <- function(years) {
  if (length(years) == 1) {
    return(data.frame(startYear = years, endYear = years))
  }
  start_years <- c(years[1], years[which(diff(years) != 1)] + 1)
  end_years <- c(years[which(diff(years) != 1)] - 1, years[length(years)])
  return(data.frame(startYear = start_years, endYear = end_years))
}

# Apply function to each entity
df_gaps <- program_years %>%
  group_by(Entity) %>%
  summarize(gap_years = list(find_gaps(Years))) %>%
  unnest(cols = c(gap_years))

df_gaps$Entity <- factor(df_gaps$Entity,
                         levels = c("AP Continuous WQ Program", "NERR SWMP",
                                    unique(df_gaps$Entity[!df_gaps$Entity %in% c("AP Continuous WQ Program", "NERR SWMP")])))

program_years_plot <- ggplot(df_gaps, aes(x=startYear, xend=endYear, y=Entity, yend=Entity)) +
  geom_segment(linewidth=4, colour=pal(df_gaps$Entity)) +
  labs(title="Years of data for each Entity",
       x="Years",
       y="Entity")