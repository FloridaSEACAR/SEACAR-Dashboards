source("seacar_data_location.R") # import data location variable

# SEACAR Plot theme
plot_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="Arial"),
        plot.title=element_text(hjust=0.5, size=12, color="#314963"),
        plot.subtitle=element_text(hjust=0.5, size=10, color="#314963"),
        legend.title=element_text(size=10),
        legend.text.align = 0,
        axis.title.x = element_text(size=10, margin = margin(t = 5, r = 0,
                                                             b = 10, l = 0)),
        axis.title.y = element_text(size=10, margin = margin(t = 0, r = 10,
                                                             b = 0, l = 0)),
        axis.text=element_text(size=10),
        axis.text.x=element_text(angle = -45, hjust = 0))

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

# Entities to highlight
highlights <- c("Aquatic Preserve Continuous Water Quality Program", 
                "National Estuarine Research Reserve SWMP")

# Group all others as "Other"
# df <- df[!Entity %in% highlights, `:=` (Entity = "Other")]

# Create summarised dataframe for use in map
map_df <- df %>% group_by(ProgramLocationID, ProgramID, ProgramName, Entity) %>%
  summarise(years = list(sort(unique(Year))),
            params = list(unique(Parameter)), 
            YearMin = min(Year),
            YearMax = max(Year),
            N_Data = sum(N_Data),
            Lat = unique(Lat),
            Lon = unique(Lon), 
            Region = unique(Region), .groups = "keep")
setDT(map_df)
map_df[is.na(Entity), `:=` (Entity = ProgramName)]

# Set palette and radius (bubble size) - by Entity
pal <- colorFactor("viridis", map_df$Entity)
# New regional palette
regions <- c("NW", "NE", "SE", "SW")
pal2 <- colorFactor("Set2", regions)
region_pal <- pal2(regions)
names(region_pal) <- regions

# Create popup labels to display metadata info
map_df <- map_df %>%
  mutate(popup = paste("ProgramID: ", ProgramID,
                       "<br> ProgramName: ", ProgramName,
                       "<br> ProgLocID: <b>", ProgramLocationID,"</b>",
                       "<br> N_Data: ", N_Data,
                       "<br> Years: ", years,
                       "<br> Params: ", params),
         label = paste(ProgramID, ": ", ProgramName, 
                       " - ProgLocID: ", ProgramLocationID),
         rad = sqrt(N_Data)/100,
         region_color = pal2(Region))

# Add commas to large numbers for easier viewing
map_df$N_Data <- formatC(map_df$N_Data, format="d", big.mark = ",")

# Create map object to load in shiny app
leaflet_map <- leaflet(map_df) %>%
  addTiles(group = "Default") %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, 
                   group = "Positron by CartoDB")

entities <- unique(map_df$Entity)

for(e in entities){
  leaflet_map <- leaflet_map %>%
    addCircleMarkers(data = map_df %>% filter(Entity==e), 
                     lat=~Lat, lng=~Lon, color=~pal(Entity),
                     weight=1, radius=~rad, fillOpacity=0.4,
                     popup = ~popup,
                     label = ~label,
                     group = e)
  
  leaflet_map <- leaflet_map %>%
    addCircleMarkers(data = map_df %>% filter(Entity==e),
                     lat=~Lat, lng=~Lon, color=~region_color,
                     weight=1, radius=~rad, fillOpacity=0.4,
                     popup = ~popup,
                     label = ~label, 
                     group=paste0(e, "_by_entity"))
  
}

leaflet_map <- leaflet_map %>%
  addLayersControl(baseGroups = c("Default","Positron by CartoDB"),
                   overlayGroups = c(entities, paste0(entities, "_by_entity")),
                   options = layersControlOptions(collapsed=TRUE))

# Gantt chart to show Entity timeline
# Group by Entity to find gaps in coverage by year
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

# Apply function to each entity (For combined gantt plot)
df_gaps <- program_years %>%
  group_by(Entity) %>%
  summarize(gap_years = list(find_gaps(Years))) %>%
  unnest(cols = c(gap_years))

df_gaps$Entity <- factor(df_gaps$Entity,
                         levels = c(highlights,
                                    unique(df_gaps$Entity[
                                      !df_gaps$Entity %in% highlights])))

# Summary stats for each ProgramLocationID within each Entity
site_years <- df %>%
  group_by(Entity, ProgramLocationID, Region) %>%
  reframe(Years = unique(Year)) %>%
  arrange(ProgramLocationID, Years)
setDT(site_years)

# Apply function to individual stations within each entity (individual gantt plot)
df_gaps_by_entity <- site_years %>%
  group_by(Entity, ProgramLocationID, Region) %>%
  summarize(gap_years = list(find_gaps(Years))) %>%
  unnest(cols = c(gap_years)) %>%
  arrange(Region, ProgramLocationID)

# Set factors
df_gaps_by_entity$Region <- factor(df_gaps_by_entity$Region, levels=regions)

# Function to serve gannt plots
plot_gantt <- function(type, ent="Aquatic Preserve Continuous Water Quality Program"){
  if(type=="All"){
    
    min_year <- min(df_gaps$startYear)
    max_year <- max(df_gaps$endYear)
    
    plot <- ggplot(df_gaps, aes(x=startYear, xend=endYear, y=Entity, yend=Entity)) +
      geom_segment(linewidth=4, colour=pal(df_gaps$Entity)) +
      labs(title="Years of data for each Entity",
           x="Years",
           y="Entity") + 
      scale_x_continuous(limits=c(min_year, max_year),
                         breaks=seq(max_year, min_year, -1)) +
      plot_theme
  } else if(type=="Entity"){
    filtered_df <- df_gaps_by_entity %>% 
      filter(Entity==ent)
    
    min_year <- min(filtered_df$startYear)
    max_year <- max(filtered_df$endYear)
    
    plot <- ggplot(filtered_df,
                   aes(x=startYear, xend=endYear, 
                       y=ProgramLocationID, yend=ProgramLocationID)) +
      geom_segment(linewidth=4, aes(color=filtered_df$Region)) +
      labs(title=paste0("Years of data for each Station in ", ent),
           x="Years",
           y="ProgramLocationID",
           color="Region") +
      scale_y_discrete(limits = unique(filtered_df$ProgramLocationID)) +
      scale_color_manual(values=region_pal) +
      scale_x_continuous(limits=c(min_year, max_year),
                         breaks=seq(max_year, min_year, -2)) +
      plot_theme
  }
  return(plot)
}
# Define all_entities plot beforehand to save processing
all_entities_gantt_plot <- plot_gantt(type="All")

plot_gantt(type="Entity", ent="Aquatic Preserve Continuous Water Quality Program")
plot_gantt(type="Entity", ent="National Estuarine Research Reserve SWMP")
