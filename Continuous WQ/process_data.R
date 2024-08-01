library(sf)
library(xlsx)
library(stringr)
library(rstudioapi)

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

source("seacar_data_location.R") # import data location variable

# Create .rds folder if it doesn't already exist
for(path in c("rds")){if(!dir.exists(path)){dir.create(path)}}

# Determine Active sites dynamically based on current date minus 1 Year
active_date <- ymd(Sys.Date()) - years(1)

# add publish date beneath funding acknowledgement to show date of latest update
publish_date <- Sys.Date()

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
hab_files <- str_subset(files, "All_") # Locate species habitat files

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

### TEMPORARY FIX TO INCLUDE EB01 STATION DATA FROM EARLIER EXPORT ###
eb_files <- list.files(paste0(seacar_data_location,"/archive/2024-Apr-15"), 
                       pattern = "_cont_", full.names = TRUE)
eb_files <- str_subset(eb_files, "_SW")
eb_data <- data.table()
for(file in eb_files){
  # Import data
  data_temp <- fread(file, sep='|', na.strings = "NULL")
  # Find parameter name, region
  param <- unique(data_temp$ParameterName)
  region <- unique(data_temp$Region)
  # Group data by PLID & Year, compute Data_N, necessary info
  grouped_df <- data_temp %>%
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
  eb_data <- bind_rows(eb_data, grouped_df)
  print(paste0("Processing ", param, " - ", region, " completed"))
}
rm(data_temp, grouped_df)
eb_data <- eb_data[ProgramLocationID=="EB01", ]
data <- bind_rows(data, eb_data)
#####################################################################

### GATHER SPECIES SAMPLING SITES
species_sites <- data.table()
# Read in species-based Habitat files to plot their locations
for(h_file in hab_files){
  hab_df <- fread(h_file, na.strings = "NULL", sep='|')
  hab_df <- hab_df %>% group_by(ProgramID, ProgramLocationID) %>%
    summarize(N_Data = n())
  hab_df$habitat <- str_split(tail(str_split(h_file,"/")[[1]],1),"_")[[1]][2]
  
  species_sites <- bind_rows(species_sites, hab_df)
}

# Reading in sample locations files (pt)
sample_loc_date <- "6june2024"
loc_files <- list.files(paste0(seacar_shape_location, "/SampleLocations",sample_loc_date), pattern = ".shp", full.names = TRUE)
pt_file <- str_subset(str_subset(loc_files, "_Point"), ".xml", negate = TRUE)
sample_locs_pt <- st_read(pt_file)

sample_locs_pt <- sample_locs_pt %>% filter(ProgramLoc %in% unique(species_sites$ProgramLocationID))

species_sample_locations_pt <- merge(x=sample_locs_pt, y=species_sites,
                                     by.x = c("ProgramLoc", "ProgramID"),
                                     by.y = c("ProgramLocationID", "ProgramID"))

# Create popups for display on Leaflet map
species_sample_locations_pt <- species_sample_locations_pt %>%
  mutate(popup = paste0("<br> <b>", habitat, "</b>", 
                        "<br> ProgramLocationID: ", ProgramLoc,
                        "<br> ProgramID: ", ProgramID))
# Select only necessary columns to save on space
species_sample_locations_pt <- as.data.frame(species_sample_locations_pt) %>%
  select(habitat, ProgramLoc, ProgramID, Latitude_D, Longitude_)

# Entities to highlight
highlights <- c("Aquatic Preserve Continuous Water Quality Program", 
                "National Estuarine Research Reserve SWMP")

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
   (link = paste0("https://data.florida-seacar.org/programs/details/",ProgramID))]

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
# map_df <- map_df %>% mutate(rad = sqrt(Data_N)*20)
# map_df <- map_df %>% mutate(rad = log10(Data_N)*2)
# map_df <- map_df %>% mutate(rad = sqrt(log10(Data_N))*2)

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
df_gaps <- df_gaps %>% 
  mutate(Status = ifelse(endYear >= year(active_date), "Active", "Historical"))

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
  arrange(ProgramName, ProgramLocationID) %>%
  mutate(Status = ifelse(endYear >= year(active_date), "Active", "Historical"))
setDT(df_gaps_by_entity)

# Entity-level Table display
table_display <- df %>% 
  group_by(Entity, link2) %>%
  summarize(
    Status = ifelse(max(Year) >= year(active_date), "Active", "Historical"),
    NumStations = length(unique(ProgramLocationID)),
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
  group_by(Entity, ProgramLocationID, ProgramName, link,
           Parameter) %>%
  summarize(Data_N = sum(Data_N),
            Status = ifelse(max(Year) >= year(active_date), "Active", "Historical")) %>%
  arrange(desc(ProgramName),desc(Data_N))

# Load in SKT stats files to grab "SufficientData" column
skt_combined <- readRDS("data/skt_combined.rds")

### EB01 FIX ###
skt_combined_old <- readRDS("data/skt_combined_old.rds")
skt_combined <- bind_rows(skt_combined, skt_combined_old[ProgramLocationID=="EB01"])
YM_combined <- readRDS("data/YM_combined.rds")
YM_combined_old <- readRDS("data/YM_combined_old.rds")
YM_combined <- bind_rows(YM_combined, YM_combined_old[ProgramLocationID=="EB01"])
################

table_display_by_entity <- merge(x=table_display_by_entity,
                                 y=skt_combined[,c("ProgramLocationID",
                                                   "ParameterName",
                                                   "SufficientData",
                                                   "N_Years")],
                                 by.x=c("ProgramLocationID", "Parameter"),
                                 by.y=c("ProgramLocationID", "ParameterName"))

# Formatting to include links
table_display_by_entity <- table_display_by_entity %>%
  mutate(ProgramNameLink = paste0("<a href='",link,"' target='_blank'>",ProgramName,"</a>"))

# Adding Button column to serve links into shiny
# Parameter and ProgramLocationID to be used as inputs in plotting function
table_display_by_entity <- table_display_by_entity %>%
  rowwise() %>%
  mutate(
    Button = ifelse(SufficientData==TRUE, as.character(
      actionLink(
        paste0("button_", Parameter, "_", ProgramLocationID),
        label = paste0(Parameter, " (",N_Years,")"),
        onclick = 'Shiny.onInputChange(\"select_button\",  this.id);'
    )),paste0(Parameter, " (",N_Years,")")))

# Formatting to display commas between data counts
table_display_by_entity$Data_N <- formatC(table_display_by_entity$Data_N, format="d", big.mark = ",")

# Add Active/Historical capability to map_df
map_df <- map_df %>% 
  mutate(Status = ifelse(YearMax >= year(active_date), "Active", "Historical"))

#### TEMPORARY EB01 FIXES ####
df_gaps_by_entity[ProgramLocationID=="EB01", `:=` (endYear = 2023,
                                                   Status = "Active")]
setDT(table_display_by_entity)
table_display_by_entity[ProgramLocationID=="EB01", `:=` (Status = "Active")]
##############################

# SAVE RDS OBJECTS
files_to_save <- c("df_gaps", "df_gaps_by_entity", "map_df",
                   "table_display", "table_display_by_entity", "pal",
                   "species_sample_locations_pt", "publish_date", "YM_combined", 
                   "skt_combined")
for(file in files_to_save){
  saveRDS(get(file), file=paste0("rds/",file,".rds"))
}

