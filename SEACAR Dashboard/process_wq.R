library(stringr)
library(data.table)
library(rstudioapi)

# Gets directory of this script and sets it as the working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

source("seacar_data_location.R")

tables_loc <- "C:/Users/Hill_T/Desktop/SEACAR GitHub/SEACAR_Trend_Analyses/MA Report Generation/output/tables"

all_depths <- c("Surface","Bottom","All")
all_activities <- c("Field","Lab","All")
all_params_short <- c("ChlaC","Chla","CDOM","DO","DOS","pH","Sal","Secchi",
                      "TN","TP","TSS","Turb","TempW")

cont_params_short <- c("DO","DOS","pH","Sal","Turb","TempW")

disc_files <- list.files(paste0(tables_loc,"/disc/"), pattern = "\\.rds$", full.names=TRUE)
cont_files <- list.files(paste0(tables_loc,"/cont/"), pattern = "\\.rds$", full.names=TRUE)

# Continuous ----
# Combine continuous trend result table for faster plot generation
for(table in c("Mon_YM_Stats", "skt_stats")){
  # Subset for desired RDS files
  table_files <- str_subset(cont_files, table)
  # importing RDS files
  df <- lapply(table_files, readRDS)
  # Combine all regions into 1 single output dataframe
  output <- do.call(rbind, df)
  # Create variable of same name
  outputFileName <- paste0(table,"_cont")
  eval(call("<-", as.name(paste0(outputFileName)),output))
  rm(output, df)
}

# Discrete ----
disc_file_list <- c()
for(param in all_params_short){
  if(param == "Secchi"){depth <- "Surface"} else {depth <- "All"}
  
  if(param %in% c("ChlaC", "Chla", "CDOM", "TN", "TP")){activity = "Lab"} 
  else if(param %in% c("DO","DOS","pH","Secchi","TempW")){activity = "Field"} 
  else if (param %in% c("Sal","TSS","Turb")){activity = "All"}
  
  file_pattern <- paste0("_",param,"_",activity,"_",depth)
  file <- str_subset(disc_files, file_pattern)
  disc_file_list <- c(disc_file_list, file)
}

for(table in c("MA_MMYY_Stats", "skt_stats")){
  # Subset for desired RDS files
  table_files <- str_subset(disc_file_list, table)
  # importing RDS files
  df <- lapply(table_files, readRDS)
  # Combine all regions into 1 single output dataframe
  output <- do.call(rbind, df)
  # Create variable of same name
  outputFileName <- paste0(table,"_disc")
  eval(call("<-", as.name(paste0(outputFileName)),output))
  rm(output, df)
}
