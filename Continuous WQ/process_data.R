library(sf)
library(xlsx)
library(stringr)
library(rstudioapi)
library(lubridate)
library(data.table)
library(tidyverse)
library(leaflet)
library(shiny)

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
  # Find parameter name
  param <- unique(data$ParameterName)
  # Group data by PLID & Year, compute Data_N, necessary info
  grouped_df <- data %>%
    group_by(ProgramLocationID, Year) %>%
    reframe(Data_N = n(),
            ProgramID = unique(ProgramID), 
            ProgramName = unique(ProgramName),
            ManagedAreaName = unique(ManagedAreaName),
            Parameter = unique(ParameterName),
            Units = unique(ParameterUnits),
            Lat = unique(OriginalLatitude),
            Lon = unique(OriginalLongitude))
  # Append to data directory
  data_directory[[param]] <- grouped_df
  print(paste0("Processing ", param, " - completed"))
}
# Store final results table
data <- data.table()
# Loop through each parameter, combine into single file
for(p in names(data_directory)){
  data <- bind_rows(data, bind_rows(data_directory[[p]]))
}

### GATHER SPECIES SAMPLING SITES
species_sites <- data.table()
# Read in species-based Habitat files to plot their locations
for(h_file in hab_files){
  hab <- str_split(tail(str_split(h_file,"/")[[1]],1),"_")[[1]][2]
  hab_df <- fread(h_file, na.strings = "NULL", sep='|')
  # Ensure only true corals are being plotted
  if(hab=="CORAL"){
    hab_df <- hab_df[SpeciesGroup1 %in% c("Octocoral","Milleporans","Scleractinian"),]
  }
  hab_df <- hab_df %>% group_by(LocationID, ProgramID, ProgramLocationID) %>%
    summarize(N_Data = n())
  hab_df$habitat <- hab
  
  species_sites <- bind_rows(species_sites, hab_df)
}

# Reading in sample locations files (pt)
# Read in point shapefile
sample_locs_pt <- SEACAR::GeoData$pointLocations
# Filter for sample locations available in SEACAR combined tables
sample_locs_pt <- sample_locs_pt %>% 
  filter(LocationID %in% unique(species_sites$LocationID))
# Merge shapefile and add habitat designation, number of data at each site
species_sample_locations_pt <- merge(x=sample_locs_pt, y=species_sites,
                                     by.x = c("ProgramLoc", "ProgramID", "LocationID"),
                                     by.y = c("ProgramLocationID", "ProgramID", "LocationID"))

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

# Grab list of programs and names, classify into "entities" by name
# Collapses and categorizes APCWQ and NERR SWMP ProgramIDs together
entities <- data %>% group_by(ProgramID, ProgramName) %>% summarise() %>%
  mutate(
    Entity = ifelse(
      str_detect(ProgramName, "Aquatic Preserves Continuous Water Quality Monitoring|Aquatic Preserve Continuous Water Quality Monitoring"),
      "Aquatic Preserve Continuous Water Quality Program",
      ifelse(
        str_detect(ProgramName, "National Estuarine Research Reserve System-Wide Monitoring Program"),
        "National Estuarine Research Reserve SWMP",
        ProgramName
      )
    )
  ) %>% as.data.table()
# Rename and shorten other program names for display
rename_map <- c(
  "Atlantic Oceanographic and Meteorological Laboratory (AOML) South Florida Program Moored Instrument Array" = "AOML South Florida Program Moored Instrument Array",
  "Florida Keys National Marine Sanctuary Seagrass Monitoring Project" = "FKNMS Seagrass Monitoring Project",
  "FDEP Bureau of Survey and Mapping Continuous Water Quality Program" = "FDEP Bureau of Survey and Mapping Continuous WQ Program",
  "St. Johns River Water Management District Continuous Water Quality Programs" = "St. Johns River Water Management District Continuous WQ Programs",
  "Pensacola Bay Water Quality Monitoring Program" = "Pensacola Bay WQ Monitoring Program"
)
entities[, Entity := fcoalesce(rename_map[Entity], Entity)]
# Add DDI Links by ProgramID
entities[ , `:=` (link = paste0("https://data.florida-seacar.org/programs/details/",ProgramID))]
entities[!Entity %in% highlights, `:=` (link2 = paste0("https://data.florida-seacar.org/programs/details/",ProgramID))]

# Merge "Entity" into dataframe
df <- merge(data, entities[ , c("Entity", "ProgramID", "link", "link2")], 
            by="ProgramID", all=TRUE)
df[is.na(Entity), `:=` (Entity = ProgramName)]
df <- df[!is.na(ProgramLocationID)]

# Group all others as "Other"
# df <- df[!Entity %in% highlights, `:=` (Entity = "Other")]

# Function to display years within popup boxes
collapse_years <- function(years) {
  years <- sort(unique(years))
  gaps <- c(TRUE, diff(years) > 1)
  starts <- years[gaps]
  ends <- years[c(gaps[-1], TRUE)]
  # Format the output with ranges or single years
  formatted <- ifelse(starts == ends, as.character(starts), paste(starts, ends, sep = "-"))
  paste(formatted, collapse = ", ")
}

# Create summarised dataframe for use in map
map_df <- df %>% group_by(ProgramLocationID, ProgramID, ProgramName, Entity) %>%
  summarise(years = collapse_years(Year),
            params = paste(unique(Parameter), collapse = ", "), 
            YearMin = min(Year),
            YearMax = max(Year),
            Data_N = sum(Data_N),
            Lat = unique(Lat),
            Lon = unique(Lon), .groups = "keep")
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
  mutate(popup = paste0("<br> <b>ProgLocID</b>: ", ProgramLocationID,
                       "<br> <b>ProgramName</b> (ID): ", ProgramName," (",ProgramID,")",
                       "<br> <b>Amount of Data</b>: ", Data_N,
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

# Function to find gaps in years for each entity (for gantt plot)
df_gaps <- program_years %>%
  arrange(Entity, Years) %>%
  group_by(Entity) %>%
  mutate(gap_id = cumsum(c(1, diff(Years) != 1))) %>%
  group_by(Entity, gap_id) %>%
  summarise(
    startYear = min(Years),
    endYear = max(Years),
    .groups = "drop"
  ) %>%
  select(-gap_id)

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

# Determine gaps for each individual station within each entity
df_gaps_by_entity <- site_years %>%
  group_by(Entity, ProgramLocationID, ProgramName) %>%
  mutate(gap_id = cumsum(c(1, diff(Years) != 1))) %>%
  group_by(Entity, ProgramLocationID, ProgramName, gap_id) %>%
  summarise(
    startYear = min(Years),
    endYear = max(Years),
    .groups = "drop"
  ) %>%
  mutate(Status = ifelse(endYear >= year(active_date), "Active", "Historical")) %>% 
  select(-gap_id) %>% as.data.table()

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
  mutate(EntityLink = ifelse(!is.na(link2), 
                             paste0("<a href='", link2,"' target='_blank'>",Entity,"</a>"), 
                             Entity))

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
YM_combined <- readRDS("data/YM_combined.rds")

# table_display_by_entity <- table_display_by_entity %>% 
#   left_join(skt_combined[,c("ProgramLocationID","ParameterName","SufficientData","N_Years")],
#             join_by("ProgramLocationID" == "ProgramLocationID", "Parameter" == "ParameterName"))

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
    )), paste0(Parameter, " (",N_Years,")"))) %>%
  arrange(ProgramName)

# Formatting to display commas between data counts
table_display_by_entity$Data_N <- formatC(table_display_by_entity$Data_N, format="d", big.mark = ",")

# Add Active/Historical capability to map_df
map_df <- map_df %>% 
  mutate(Status = ifelse(YearMax >= year(active_date), "Active", "Historical"))

# Add parametervisid to skt_combined and create key column to locate relevant plot
# skt_combined <- skt_combined %>% left_join(
#   SEACAR::WebsiteParameters %>% 
#     filter(SamplingFrequency=="Continuous" & Website==1) %>% 
#     select(ParameterVisId, ParameterName)
#   ) %>%
#   mutate(plot_id = paste0("ma-", AreaID, "-pv-", ParameterVisId))

# Modify skt_combined to include SKT trendline start and end points
skt_combined <- skt_combined %>%
  mutate(start_x = decimal_date(EarliestSampleDate),
         start_y = (start_x-EarliestYear)*SenSlope+SenIntercept,
         end_x = decimal_date(LastSampleDate),
         end_y = (end_x-EarliestYear)*SenSlope+SenIntercept) %>%
  as.data.table()

# Figure caption
FigCaps <- SEACAR::FigureCaptions[SamplingFrequency=="Continuous"]

# Creating units datatable for display in plots
cont_param_df <- SEACAR::WebsiteParameters[SamplingFrequency=="Continuous", c("ParameterName", "ParameterUnits")]

# SAVE RDS OBJECTS
files_to_save <- c("df_gaps", "df_gaps_by_entity", "map_df",
                   "table_display", "table_display_by_entity", "pal",
                   "species_sample_locations_pt", "publish_date", "YM_combined", 
                   "skt_combined", "FigCaps", "cont_param_df")
for(file in files_to_save){
  saveRDS(get(file), file=paste0("rds/",file,".rds"))
}
