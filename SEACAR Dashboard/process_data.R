# This script pre-processes combined tables and creates the necessary .RDS modules
# for more efficient use in app.R.

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
library(tidyr)

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
habitats <- c("sav" = "Submerged Aquatic Vegetation", "oyster" = "Oyster Reef", 
              "coral" = "Coral Reef", "cw" = "Coastal Wetlands", "nekton" = "Nekton")

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
  # Convert "Oyster" back to "Oyster/Oyster Reef" format to match db_thresholds Habitat format, same for Coral
  hab_subset <- ifelse(habitat=="Nekton", "Water Column", 
                       ifelse(habitat %in% c("Oyster Reef","Coral Reef"), 
                              paste0(str_split_1(habitat, " ")[1],"/",habitat), 
                              habitat))
  
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
  
  ### ProgramIDs for each ManagedArea
  data_directory[[habitat]][["MAPrograms"]] <- data %>% 
    group_by(ProgramID, ProgramName, ManagedAreaName) %>% summarise() %>%
    mutate(pNameID = paste0(ProgramID, " - ", ProgramName)) %>%
    mutate(link = paste0("tags$a(href='https://data.florida-seacar.org/programs/details/",ProgramID,"'",",",pNameID,")"))
  
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
    filter(!is.na(ManagedAreaName)) %>%
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
    mutate(pNameID = paste0(ProgramID, " - ", ProgramName)) %>%
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

### Discrete Water Quality data wrangling ----
# Makes use of discrete data object outputs from MA Report Generation
# Change folder date to select which objects to load (to match Atlas)
disc_folder_date <- "2024-Mar-27"
cont_folder_date <- "2024-Mar-27"

# Point to location where Disc objects are located
data_obj_loc <- "C:/Users/Hill_T/Desktop/SEACAR GitHub/SEACAR_Trend_Analyses/MA Report Generation/output/tables/"

# Lists of disc and cont .rds objects to read
disc_files <- list.files(paste0(data_obj_loc,"disc/",disc_folder_date,"/"),pattern = "\\.rds$", full.names = T)
cont_files <- list.files(paste0(data_obj_loc,"cont/",cont_folder_date,"/"),pattern = "\\.rds$", full.names = T)

# function of parameter, activity type, depth, with specified filetype
# retrieves RDS filepath to be loaded
get_files <- function(p, a, d, filetype) {
  
  # "data" contains overall data for each param, regardless of depth/activity
  if (filetype == "data") {
    pattern <- paste0(p,"_",filetype)
  } else {
    pattern <- paste0(p,"_",a,"_",d,"_",filetype)
  }
  # subset directory files for given pattern
  file_return <- str_subset(disc_files, pattern)
  return(file_return)
}

# Discrete & Continuous Combine necessary tables ----
# Combine continuous/disc trend result tables for faster plot generation
# Creates MA_MMYY_Stats_disc, Mon_YM_Stats_cont, skt_stats_disc, skt_stats_cont
for(type in c("disc", "cont")){
  # Specify table names and file lists for disc and cont
  if(type=="disc"){
    tables <- c("MA_MMYY_Stats", "skt_stats")
    files <- disc_files
  } else {
    tables <- c("Mon_YM_Stats", "skt_stats")
    files <- cont_files
  }
  
  for(table in tables){
    # Subset for desired RDS files
    table_file <- str_subset(files, table)
    # importing RDS files
    df <- lapply(table_file, readRDS)
    # Combine all regions into 1 single output dataframe
    output <- do.call(rbind, df)
    # Create variable of same name
    eval(call("<-", as.name(paste0(table,"_",type)),output))
  }
}

skt_stats_disc <- skt_stats_disc %>% 
  mutate("Period of Record" = paste0(EarliestYear, " - ", LatestYear),
         "Statistical Trend" = ifelse(p <= 0.05 & SennSlope > 0, "Significantly increasing trend",
                                      ifelse(p <= 0.05 & SennSlope < 0, "Significantly decreasing trend", 
                                             ifelse(SufficientData==FALSE, "Insufficient data to calculate trend",
                                                    ifelse(SufficientData==TRUE & is.na(SennSlope), "Model did not fit the available data", 
                                                           ifelse(is.na(Trend), "Insufficient data to calculate trend","No significant trend"))))))
skt_stats_disc[is.na(Trend), `:=` ("Statistical Trend" = "Insufficient data to calculate trend")]

skt_stats_cont <- skt_stats_cont %>% 
  mutate("Period of Record" = paste0(EarliestYear, " - ", LatestYear),
         "Statistical Trend" = ifelse(p <= 0.05 & SennSlope > 0, "Significantly increasing trend",
                                      ifelse(p <= 0.05 & SennSlope < 0, "Significantly decreasing trend", 
                                             ifelse(SufficientData==FALSE, "Insufficient data to calculate trend",
                                                    ifelse(SufficientData==TRUE & is.na(SennSlope), "Model did not fit the available data", 
                                                           ifelse(is.na(Trend), "Insufficient data to calculate trend","No significant trend"))))))
skt_stats_cont[is.na(Trend), `:=` ("Statistical Trend" = "Insufficient data to calculate trend")]

# Combine all discrete data into a single output file
data_output_disc <- setDT(do.call(rbind, lapply(str_subset(disc_files, "data"), readRDS)))

# Post-processing ----
# The following sets up Icon designations for valueBoxes
# params <- c()
# for(h in habitats){
#   params <- c(params, unique(data_directory[[h]][["overviewTable"]] %>% pull(ParameterName)))
# }
# # Create icon associations for valueBoxes
# icon_df <- data.frame(param = params)
# icon_df <- icon_df %>% mutate(icon = ifelse(str_detect(param, "Number|Count"), "hashtag", 
#                                             ifelse(str_detect(param, "Percent"), "percent",
#                                                    ifelse(str_detect(param, "Height|Length|Width|Diameter"), "ruler", 
#                                                           ifelse(str_detect(param, "Presence"), "check", 
#                                                                  ifelse(str_detect(param, "Density"), "fish",
#                                                                         ifelse(str_detect(param, "Score"),"star","No")))))))
# setDT(icon_df)

# Function to check for available figures by MA, provides filepath if T, otherwise F 
fig_detect <- function(figures, ma_short, plot_type) {
  # Create regex patterns for both possible orders
  pattern1 <- paste0("(?i)", ma_short, ".*", plot_type)
  pattern2 <- paste0("(?i)", plot_type, ".*", ma_short)
  
  # Detect files matching either pattern
  matched_files1 <- str_subset(figures, pattern1)
  matched_files2 <- str_subset(figures, pattern2)
  
  # Combine matched files from both patterns
  matched_files <- c(matched_files1, matched_files2)
  
  # Return the first matched file path if any, else return FALSE
  if (length(matched_files) > 0) {
    return(matched_files[1])
  } else {
    return("FALSE")
  }
}

# SAV Figures
figures <- list.files("www/figures", recursive = T,
                      full.names = TRUE, pattern = ".png")

# Create associations for filepaths in MA_All
# Function within Server.R will plot on dashboard using these filepaths
MA_All <- fread("data/ManagedArea.csv")
MA_All <- MA_All %>% rowwise() %>% mutate(
  multiplot = fig_detect(figures, Abbreviation, "multiplot"),
  trendplot = fig_detect(figures, Abbreviation, "trendplot"),
  barplot_sp = fig_detect(figures, Abbreviation, "barplot_sp"),
  Oyster_Dens = fig_detect(figures, Abbreviation, "Oyster_Dens"),
  Oyster_PrcLive = fig_detect(figures, Abbreviation, "Oyster_PrcLive"),
  Oyster_SH = fig_detect(figures, Abbreviation, "Oyster_SH"),
  Nekton_SpeciesRichness = fig_detect(figures, gsub(" ", "", ManagedAreaName), "Nekton_SpeciesRichness"),
  Coral_pc = fig_detect(figures, gsub(" ", "", ManagedAreaName), "Coral_pc"),
  Coral_SpeciesRichness = fig_detect(figures, gsub(" ", "", ManagedAreaName), "Coral_SpeciesRichness"),
  CoastalWetlands_SpeciesRichness = fig_detect(figures, gsub(" ", "", ManagedAreaName), "CoastalWetlands_SpeciesRichness"),
) %>% ungroup()
setDT(MA_All)

# plot_type_list <- c("multiplot","trendplot","barplot_sp",
#                     "Oyster_Dens","Oyster_SH","Oyster_PrcLive",
#                     "Nekton_SpeciesRichness","Coral_pc","Coral_SpeciesRichness",
#                     "CoastalWetlands_SpeciesRichness")

# Dataframe to store titles and alt text for figure display
plot_info <- list(
  multiplot = list(
    title = "Median percent cover for ",
    alt = "Median Percent LME Trends",
    label = "Median percent cover",
    habitat = "Submerged Aquatic Vegetation"
  ),
  barplot_sp = list(
    title = "Frequency of occurrence for ",
    alt = "Occurrence frequency by species",
    label = "Frequency of occurrence",
    habitat = "Submerged Aquatic Vegetation"
  ),
  trendplot = list(
    title = "Median percent cover for ",
    alt = "Median Percent LME Trends",
    label = "Median percent cover trends",
    habitat = "Submerged Aquatic Vegetation"
  ),
  Oyster_Dens = list(
    title = "Oyster Density trends for ",
    alt = "Oyster Density trends",
    label = "Oyster Density",
    habitat = "Oyster Reef"
  ),
  Oyster_SH = list(
    title = "Oyster Size Class trends for ",
    alt = "Oyster Size Class",
    label = "Oyster Size Class",
    habitat = "Oyster Reef"
  ),
  Oyster_PrcLive = list(
    title = "Oyster Percent Live Cover trends for ",
    alt = "Oyster Percent Live Cover trends",
    label = "Oyster Percent Live Cover",
    habitat = "Oyster Reef"
  ),
  Nekton_SpeciesRichness = list(
    title = "Nekton Species Richness for ",
    alt = "Nekton Species Richness",
    label = "Nekton Species Richness",
    habitat = "Nekton"
  ),
  Coral_pc = list(
    title = "Coral Percent Cover trend for ",
    alt = "Coral Percent Cover",
    label = "Coral Percent Cover",
    habitat = "Coral Reef"
  ),
  Coral_SpeciesRichness = list(
    title = "Grazers and Reef-Dependent Species Richness for ",
    alt = "Grazers and Reef-Dependent Species Richness",
    label = "Grazers and Reef-Dependent Species Richness",
    habitat = "Coral Reef"
  ),
  CoastalWetlands_SpeciesRichness = list(
    title = "Coastal Wetlands Species Richness for ",
    alt = "Coastal Wetlands Species Richness",
    label = "Coastal Wetlands Species Richness",
    habitat = "Coastal Wetlands"
  )
)

plot_df <- map(plot_info, ~as.data.frame(.x), .id = "plot_type")
setDT(plot_df)

# Incorporate trend tables for each habitat
## SAV
sav_trends <- fread("data/SAV_BBpct_LMEresults_All.txt", sep='|')
sav_trends[, `:=` ("Period of Record" = paste0(EarliestYear, " - ", LatestYear))]
sav_trends <- sav_trends[, c("ManagedAreaName","Species","StatisticalTrend",
                             "Period of Record","LME_Intercept","LME_Slope","p")]
## Oyster
# Function pulled from CheckTrendText.R from QAQC-Tools repo
checkOysterTrends <- function(modelEstimate, lowConfidence, upConfidence, suffData){
  if(is.na(modelEstimate)){
    if(suffData){
      trendStatus <- "Model did not fit the available data"
    } else {
      trendStatus <- "Insufficient data to calculate trend"
    }
  } else {
    increasing <- modelEstimate > 0
    trendPresent <- ifelse(lowConfidence < 0 & upConfidence < 0, TRUE, 
                           ifelse(lowConfidence > 0 & upConfidence > 0, TRUE, FALSE))
    trendStatus <- "No significant trend"
    if(trendPresent){
      trendStatus <- ifelse(increasing, "Significantly increasing trend", "Significantly decreasing trend")
    }    
  }
  return(trendStatus)
}

oy_trends <- fread("data/Oyster_All_GLMM_Stats.txt", sep='|')
oy_trends <- oy_trends %>% rowwise() %>% 
  mutate(TrendStatus = checkOysterTrends(ModelEstimate, LowerConfidence, UpperConfidence, SufficientData)) %>% ungroup() %>%
  mutate(CredibleInterval = paste0(round(LowerConfidence,2), " to ", round(UpperConfidence,2))) %>%
  select(ManagedAreaName, ParameterName, ShellType, HabitatType, SizeClass, TrendStatus, ModelEstimate, 
         StandardError, CredibleInterval)
setDT(oy_trends)

# Overall TrendTable object
allTrendTables <- list(
  "Submerged Aquatic Vegetation" = sav_trends,
  "Oyster Reef" = oy_trends
)

#########################
# Saving RDS objects ----
#########################
rds_to_save <- c("data_directory", "allMapData", "data_output_disc", "MA_All", 
                 "plot_df", "publish_date", "sav_trends")
for(file in rds_to_save){
  saveRDS(get(file), file=paste0("rds/",file,".rds"))
}

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
