# Continuous Water Quality Dashboard

The live version of the [Continuous Water Quality Dashboard](https://floridaseacar.shinyapps.io/continuous_wq/)
## Script Overview

This script utilizes modularized .RDS objects to help save on server-side memory. The objects are created using the following scripts, and imported into app.R to launch the app:
1. process_data.R
   - All available SEACAR continuous files are processed within a loop that provides summary information by ProgramLocationID and Year.
   - All available SEACAR habitat files are processed and relevant sample location points are collected for display on the map.
   - The following .RDS objects are created:
     - df_gaps
     - df_gaps_by_entity
     - map_df
     - table_display
     - table_display_by_entity
     - pal
     - species_sample_locations_pt
2. combine_cont_tables.R
   - This script uses .RDS objects created within the MA Report Generation environment, specifically WQ_Continuous_Data_Creation.R and is intended to be run within the MA Report Generation environment for best results.
   - The .RDS objects to be processed are located in the *MA Report Generation/output/tables/cont* folder.
   - The script then creates the *skt_combined.RDS* and *YM_combined.RDS* objects which then must be placed within the *SEACAR-Dashboards/Continuous WQ/data/* folder
3. app.R
   - Launches and runs the Shiny app.
   - The app is deployed by copying the *deployApp()* command (commented out in the bottom lines) and pasting into the console in RStudio.
