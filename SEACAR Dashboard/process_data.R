library(shiny)
library(dplyr)
library(leaflet)
library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)
library(rstudioapi)
library(lubridate)
library(sf)
library(xlsx)

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

active_date <- ymd(Sys.Date()) - years(1)

# add publish date beneath funding acknowledgement to show date of latest update
publish_date <- Sys.Date()

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

source("seacar_data_location.R")
source("load_shape_samples.R")

# File Import ----
files <- list.files(seacar_data_location, full.names = T)

disc_files <- str_subset(str_subset(files, "_WQ_WC_NUT"), "_cont_", negate=T)

sav <- fread(str_subset(files, "SAV"), sep='|', na.strings = "NULL")
sav <- sav[Include == 1 & MADup==1, ]

oyster <- fread(str_subset(files, "OYSTER"), sep='|', na.strings = "NULL")
oyster <- oyster[Include == 1 & MADup==1, ]

coral <- fread(str_subset(files, "CORAL"), sep='|', na.strings = "NULL")
coral <- coral[Include == 1 & MADup==1, ]

cw <- fread(str_subset(files, "CW"), sep='|', na.strings = "NULL")
cw <- cw[Include == 1 & MADup==1, ]

nekton <- fread(str_subset(files, "NEKTON"), sep='|', na.strings = "NULL")
nekton <- nekton[Include == 1 & MADup==1, ]

# Create named habitats list to populate selections
habitats <- c("sav" = "Submerged Aquatic Vegetation", "oyster" = "Oyster/Oyster Reef", 
              "coral" = "Coral/Coral Reef", "cw" = "Coastal Wetlands", "nekton" = "Nekton")

# Function to find gaps in years by program (Gantt timeline)
find_gaps <- function(years) {
  if(length(years) == 1) {
    return(data.frame(startYear = years, endYear = years))
  }
  start_years <- c(years[1], years[which(diff(years) != 1)] + 1)
  end_years <- c(years[which(diff(years) != 1)] - 1, years[length(years)])
  return(data.frame(startYear = start_years, endYear = end_years))
}

# Import processing
# Create directory to store all necessary data subsets by habitat
data_directory <- list()
# allMapData object to store overall results for leaflet map
# includes information for all habitats in each layer
allMapData <- data.table()
MA_Include <- c()

# List to exclude parameters from display for certain habitats
params_to_exclude <- c("Number of Oysters Counted - Dead",
                       "Number of Oysters Counted - Live",
                       "Number of Oysters Counted - Total",
                       "Reef Height", "Stem Density", "Colony Width", 
                       "Colony Length", "Colony Height", "Colony Diameter")

# Until ParameterUnits are added into CombinedTable Exports
# Import Database_Thresholds.xlsx output from IndicatorQuantiles GitHub
db_thresholds <- read.xlsx("data/Database_Thresholds.xlsx", 
                           sheetName="Thresholds in Database", header=T, startRow=7) %>%
  select(Habitat, ParameterName, Units)
setDT(db_thresholds)

# Loop through each habitat and generate items, place into data_directory
for(h in names(habitats)){
  data <- get(h)
  
  # Grab full habitat name
  habitat <- habitats[[h]]
  
  # DB_Thresholds file lists habitat as Water Column, make exception for Nekton
  hab_subset <- ifelse(habitat=="Nekton", "Water Column", habitat)
  
  # Merge in Units from db_thresholds
  data <- merge(data, db_thresholds[Habitat==hab_subset], 
                by=c("ParameterName","Habitat"), allow.cartesian=T)
  
  # Exclude parameters
  data <- data[!ParameterName %in% params_to_exclude, ]
  
  # Grab list of managed areas for each habitat
  MA_Include <- c(MA_Include, unique(data$ManagedAreaName))
  
  # save palette (by ProgramID) for each habitat
  data_directory[[habitat]][["pal"]] <- colorFactor("plasma", data$ProgramID)
  
  ### ProgramParamPlots
  data_directory[[habitat]][["programParams"]] <- data %>% 
    group_by(ParameterName) %>% summarise(n = length(unique(ProgramID)))
  
  ### ProgramYearsPlots
  programYears <- data %>%
    group_by(ProgramID) %>% reframe(Years = unique(Year)) %>%
    arrange(ProgramID, Years)
  
  programYears <- programYears %>% group_by(ProgramID) %>%
    summarize(gap_years = list(find_gaps(Years))) %>%
    unnest(cols = c(gap_years)) %>% 
    group_by(ProgramID) %>%
    mutate(most_recent_endYear = max(endYear)) %>%
    ungroup() %>%
    mutate(Status = ifelse(most_recent_endYear >= year(active_date), "Active", "Historical")) %>%
    select(-most_recent_endYear)
  
  programYears$ProgramID <- as.factor(programYears$ProgramID)
  
  data_directory[[habitat]][["programYears"]] <- programYears
  
  ### MapData
  # Get unique ProramIDs
  programs <- unique(data$ProgramID)
  program_locs <- unique(data$ProgramLocationID)
  # Create data groupings and record n_data, years, params
  ma_data <- data %>% 
    group_by(ProgramLocationID, ProgramID, ProgramName, LocationID) %>%
    summarise(n_data = n(), 
              years = list(sort(unique(Year))), 
              params = list(unique(ParameterName)), .groups="keep")
  
  # Grab shapefile points, filter for relevant programs and programLocs
  df <- point %>% filter(ProgramID %in% programs)
  
  mapData <- merge(x=ma_data, y=df, 
                       by.x=c("ProgramLocationID", "ProgramID", "LocationID"), 
                       by.y=c("ProgramLoc","ProgramID","LocationID")) %>%
    mutate(popup = paste("ProgramID: ", ProgramID, 
                         "<br> ProgramName: ", ProgramName, 
                         "<br> LocID: ", LocationID, 
                         "<br> ProgLocID: ", ProgramLocationID, 
                         "<br> N_Data: ", n_data, 
                         "<br> Years: ", years, 
                         "<br> params: ", params),
           label = paste0(ProgramID, ": ", ProgramName, 
                          " - LocID: ", LocationID)) %>%
    select(-c(ObjectID,DateAdded,LastModifi,LastModi_1,geometry)) %>%
    rename(lat = Latitude_D, lon = Longitude_) %>%
    mutate(habitat = habitat,
           rad = sqrt(n_data)/8)
  
  allMapData <- rbind(allMapData, mapData)
  
  ### Summary tables
  data_directory[[habitat]][["summTableAll"]] <- data %>% 
    group_by(ParameterName, Units) %>%
    summarise(N_samples = n(), mean = round(mean(ResultValue),2),
              min = min(ResultValue), max = max(ResultValue),
              .groups = "keep")
  
  data_directory[[habitat]][["summTableByProgram"]] <- data %>% 
    group_by(ParameterName, Units, ProgramID, ProgramName) %>%
    summarise(N_samples = n(), mean = round(mean(ResultValue),2),
              min = min(ResultValue), max = max(ResultValue),
              .groups = "keep") %>%
    arrange(ProgramID)
  
  data_directory[[habitat]][["maSummTable"]] <- data %>% 
    group_by(ParameterName, Units, ManagedAreaName) %>%
    summarise(N_samples = n(), mean = round(mean(ResultValue),2),
              min = min(ResultValue), max = max(ResultValue),
              .groups = "keep")
  
  ### Overview tables
  data_directory[[habitat]][["overviewTable"]] <- data %>%
    group_by(ProgramName, ProgramID, ParameterName, Units) %>%
    reframe("Number of Stations" = length(unique(ProgramLocationID)),
            "Number of Observations" = n(),
            "Years of Data" = length(unique(Year)),
            "Period of Record" = paste0(min(Year), " - ", max(Year))) %>%
    arrange(ProgramID)
  
  data_directory[[habitat]][["maOverviewTable"]] <- data %>%
    group_by(ManagedAreaName, ParameterName, Units) %>%
    reframe("Number of Stations" = length(unique(ProgramLocationID)),
            "Number of Observations" = n(),
            "Years of Data" = length(unique(Year)),
            "Period of Record" = paste0(min(Year), " - ", max(Year))) %>%
    arrange(ManagedAreaName)
  
  rm(programYears, programs, ma_data, df)
}

# # Import RCP shapefile to pre-process and increase efficiency
# shape_file <- list.files(paste0(seacar_shape_location, "/orcp_all_sites"), pattern = ".shp$", full.names=T)
# rcp <- st_read(shape_file) %>% filter(LONG_NAME %in% unique(MA_Include)) %>% 
#   select(-c(Mgmt_Unit, Acres, OBJECTID_1)) %>% st_transform('+proj=longlat +datum=WGS84')

# Saving RDS objects ----
rds_to_save <- c("data_directory", "allMapData") #"rcp"
for(file in rds_to_save){
  saveRDS(get(file), file=paste0("rds/",file,".rds"))
}

# Post-processing
params <- c()
for(h in habitats){
  params <- c(params, unique(data_directory[[h]][["overviewTable"]] %>% pull(ParameterName)))
}

icon_df <- data.frame(param = params)
icon_df <- icon_df %>% mutate(icon = ifelse(str_detect(param, "Number|Count"), "hashtag", 
                                            ifelse(str_detect(param, "Percent"), "percent",
                                                   ifelse(str_detect(param, "Height|Length|Width|Diameter"), "ruler", 
                                                          ifelse(str_detect(param, "Presence"), "check", 
                                                                 ifelse(str_detect(param, "Density"), "fish",
                                                                        ifelse(str_detect(param, "Score"),"star","No")))))))
setDT(icon_df)

# SAV Figures
sav_figures <- list.files("figures/sav", 
                          full.names = TRUE, pattern = ".rds")

MA_All <- fread("data/ManagedArea.csv")
MA_All <- MA_All %>% rowwise() %>% 
  mutate(multiplot = ifelse(length(str_subset(str_subset(str_subset(sav_figures, "_multiplot"), "_BBpct_"), Abbreviation))>0, 
                            str_subset(str_subset(str_subset(sav_figures, "_multiplot"), "_BBpct_"), Abbreviation), "FALSE"),
         trendplot = ifelse(length(str_subset(str_subset(str_subset(sav_figures, "_trendplot"), "_BBpct_"), Abbreviation))>0, 
                            str_subset(str_subset(str_subset(sav_figures, "_trendplot"), "_BBpct_"), Abbreviation), "FALSE"),
         barplot = ifelse(length(str_subset(str_subset(sav_figures, "_barplot"), Abbreviation))>0, 
                          str_subset(str_subset(sav_figures, "_barplot"), Abbreviation), "FALSE"))
setDT(MA_All)








### EXPORT SHAPEFILE ###
# allPoints <- list()
# for(h in c("sav","oyster","coral","cw","nekton")){
#   data <- get(h)
#   allPoints[[h]] <- data %>% group_by(Habitat, ProgramID, ProgramName, ProgramLocationID, Region, 
#                     OriginalLatitude, OriginalLongitude) %>%
#     summarise(n = n(), MAs = unique(ManagedAreaName))
# }
# 
# allPoints_df <- bind_rows(allPoints)
# allPoints_df <- allPoints_df %>% rename("lat" = OriginalLatitude,
#                                         "lon" = OriginalLongitude,
#                                         "ProgramLoc" = ProgramLocationID,
#                                         "ProgID" = ProgramID,
#                                         "ProgName" = ProgramName) %>%
#   filter(!is.na(lat), !is.na(lon)) %>% ungroup()
# 
# allPoints_shp <- st_as_sf(allPoints_df,
#                           coords = c("lon","lat"),
#                           remove = F,
#                           crs = 4326)
# 
# st_write(allPoints_shp, paste0(seacar_shape_location, "/allPoints_habitats.shp"))
