## This script was used within MA Report Generation environment / R Project
## Grabs the continuous outputs (SKT results, YM data [condensed])
## Combines them into individual files to allow for quicker loading with .RDS

library(stringr)

params <- c("DO","DOS","pH","Sal","TempW","Turb")
regions <- c("NE","NW","SE","SW")

YM_combined <- data.table()
skt_combined <- data.table()

for(p in params){
  for(r in regions){
    Mon_YM_Stats <- as.data.frame(load_cont_data_table(p, r, "Mon_YM_Stats"))
    skt_stats <- as.data.frame(load_cont_data_table(p, r, "skt_stats"))
    
    YM_combined <- bind_rows(YM_combined, Mon_YM_Stats)
    skt_combined <- bind_rows(skt_combined, skt_stats)
  }
}

saveRDS(YM_combined, file = "output/YM_combined.rds")
saveRDS(skt_combined, file = "output/skt_combined.rds")