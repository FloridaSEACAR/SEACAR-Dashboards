library(sf)
library(rmapshaper)

sample_date <- "5Mar2025"

# Sample Locations
point <- st_read(paste0(seacar_shape_location,"/SampleLocations", sample_date, "/seacar_dbo_vw_SampleLocation_Point.shp"))

# ORCP boundaries
orcp_shp <- st_read(paste0(seacar_shape_location, "/orcp_all_sites/ORCP_Managed_Areas.shp")) %>%
  st_transform('+proj=longlat +datum=WGS84')

# OIMMP boundaries
oimmp <- st_read(paste0(seacar_shape_location, "/OIMMP_Boundaries/OIMMPRegions.shp")) %>%
  st_transform('+proj=longlat +datum=WGS84')
# Simplify boundaries (reduced filesize)
oimmp <- rmapshaper::ms_simplify(oimmp)
# CHIMMP boundaries
chimmp <- st_read(paste0(seacar_shape_location, "/CHIMMP_Boundaries/chimmp_regions.shp")) %>%
  st_transform('+proj=longlat +datum=WGS84')
chimmp <- rmapshaper::ms_simplify(chimmp)

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
