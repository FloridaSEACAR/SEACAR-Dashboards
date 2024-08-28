library(sf)

# Sample Locations
point <- st_read(paste0(seacar_shape_location,"/SampleLocations6june2024/vw_SampleLocation_Point62024.shp"))

# ORCP boundaries
orcp_shp <- st_read(paste0(seacar_shape_location, "/orcp_all_sites/ORCP_Managed_Areas.shp")) %>%
  st_transform('+proj=longlat +datum=WGS84')

###############
## FUNCTIONS ##
###############

# Allows location of shapefile for each MA
find_shape <- function(ma){
  orcp_shp[orcp_shp$LONG_NAME==ma, ]
}

get_shape_coordinates <- function(ma_shape){
  st_bbox(st_geometry(ma_shape))
}
