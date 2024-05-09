source("seacar_data_location.R") # import data location variable

library(xlsx)

# SEACAR Plot theme
plot_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="gray97"),
        text=element_text(family="Arial"),
        plot.title=element_text(hjust=0.5, size=12, color="#314963"),
        plot.subtitle=element_text(hjust=0.5, size=10, color="#314963"),
        legend.title=element_text(size=14),
        legend.text = element_text(size=12),
        legend.text.align = 0,
        axis.title.x = element_text(size=10, margin = margin(t = 5, r = 0,
                                                             b = 10, l = 0)),
        axis.title.y = element_text(size=10, margin = margin(t = 0, r = 10,
                                                             b = 0, l = 0)),
        axis.text=element_text(size=11),
        axis.text.x=element_text(angle = -45, hjust = 0))

# Custom SEACAR palette
seacar_palette <- c(
  "#964059",
  "#E05E7B",
  "#E98C86",
  "#F1B8AB",
  "#F8CAAA",
  "#F8E6B9",
  "#FEEEE1",
  "#DAE9DA",
  "#8BE4C2",
  "#7EE7E8",
  "#8FD0EC",
  "#6FA1DD",
  "#889BD1",
  "#8F83D3",
  "#6B59AB"
)

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
  # Group data by PLID & Year, compute Data_N, necessary info
  grouped_df <- data %>%
    group_by(ProgramLocationID, Year) %>%
    reframe(Data_N = n(),
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
                      sheetName = "Sheet1", header=TRUE, startRow = 2) %>%
  select(Group, ID, Program.Name)
# Rename columns, place ProgramName into Entity column if not already designated
entities <- entities %>% 
  rename(Entity = Group,ProgramID = ID,ProgramName = Program.Name)
setDT(entities)
entities[is.na(Entity), `:=` (Entity = ProgramName)]
entities[ , `:=` (link = paste0("https://data.florida-seacar.org/programs/details/",ProgramID))]
entities[!Entity %in% highlights, `:=` (link2 = paste0("https://data.florida-seacar.org/programs/details/",ProgramID))]

# Merge "Entity" into dataframe
df <- merge(data, entities[ , c("Entity", "ProgramID", "link", "link2")], 
            by="ProgramID", all=TRUE)
df[is.na(Entity), `:=` (Entity = ProgramName)]
df <- df[!is.na(ProgramLocationID)]
df[Entity=="USGS Coral Reef Ecosystem Studies (CREST) Project", `:=` 
   (link2 = paste0("https://data.florida-seacar.org/programs/details/",ProgramID))]

# Group all others as "Other"
# df <- df[!Entity %in% highlights, `:=` (Entity = "Other")]

# Create summarised dataframe for use in map
map_df <- df %>% group_by(ProgramLocationID, ProgramID, ProgramName, Entity) %>%
  summarise(years = list(sort(unique(Year))),
            params = list(unique(Parameter)), 
            YearMin = min(Year),
            YearMax = max(Year),
            Data_N = sum(Data_N),
            Lat = unique(Lat),
            Lon = unique(Lon), 
            Region = unique(Region), .groups = "keep")
setDT(map_df)
map_df[is.na(Entity), `:=` (Entity = ProgramName)]

# Set palette and radius (bubble size) - by Entity
pal <- colorFactor(seacar_palette, map_df$Entity)

# Determine radius levels
map_df <- map_df %>% mutate(rad = sqrt(Data_N)/100)

# Add commas to large numbers for easier viewing
map_df$Data_N <- formatC(map_df$Data_N, format="d", big.mark = ",")

# Create popup labels to display metadata info
map_df <- map_df %>%
  mutate(popup = paste("<br> <b>ProgLocID</b>: ", ProgramLocationID,
                       "<br> <b>ProgramName</b> (ID): ", ProgramName," (",ProgramID,")",
                       "<br> <b>Data_N</b>: ", Data_N,
                       "<br> <b>Years</b>: ", years,
                       "<br> <b>Params</b>: ", params),
         label = paste(ProgramLocationID))

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
  group_by(Entity, ProgramLocationID, ProgramName) %>%
  reframe(Years = unique(Year)) %>%
  arrange(ProgramLocationID, Years)
setDT(site_years)

# Apply function to individual stations within each entity (individual gantt plot)
df_gaps_by_entity <- site_years %>%
  group_by(Entity, ProgramLocationID, ProgramName) %>%
  summarize(gap_years = list(find_gaps(Years))) %>%
  unnest(cols = c(gap_years)) %>%
  arrange(ProgramName, ProgramLocationID)

# plot_gantt(type="Entity", ent="Aquatic Preserve Continuous Water Quality Program")
# plot_gantt(type="Entity", ent="National Estuarine Research Reserve SWMP")

# Entity-level Table display
table_display <- df %>% 
  group_by(Entity, link2) %>%
  summarize(NumStations = length(unique(ProgramLocationID)),
            Data_N = sum(Data_N),
            IncludedParams = paste(sort(unique(Parameter), 
                                        decreasing=FALSE), 
                                   collapse=", ")) %>%
  arrange(desc(Data_N))

# Formatting to include links
table_display <- table_display %>%
  mutate(EntityLink = 
           ifelse(!is.na(link2), paste0("<a href='",link2,"' target='_blank'>",Entity,"</a>"), Entity))

# Formatting to display commas between data counts
table_display$Data_N <- formatC(table_display$Data_N, format="d", big.mark = ",")


# Tables display for each entity
table_display_by_entity <- df %>%
  group_by(Entity, ProgramLocationID, ProgramName, link) %>% 
  summarize(Data_N = sum(Data_N),
            IncludedParams = paste(sort(unique(Parameter), 
                                        decreasing=FALSE), 
                                   collapse=", ")) %>%
  arrange(desc(ProgramName),desc(Data_N))

# Formatting to include links
table_display_by_entity <- table_display_by_entity %>%
  mutate(ProgramNameLink = paste0("<a href='",link,"' target='_blank'>",ProgramName,"</a>"))

# Formatting to display commas between data counts
table_display_by_entity$Data_N <- formatC(table_display_by_entity$Data_N, format="d", big.mark = ",")

# SAVE RDS OBJECTS
files_to_save <- c("df_gaps", "df_gaps_by_entity", "map_df",
                   "table_display", "table_display_by_entity", "pal")
for(file in files_to_save){
  saveRDS(get(file), file=paste0("rds/",file,".rds"))
}