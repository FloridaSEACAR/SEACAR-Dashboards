library(sf)
library(rmapshaper)

# Sample Locations
point <- SEACAR::GeoData$pointLocations

# ORCP boundaries
orcp_shp <- SEACAR::GeoData$`RCP Boundaries`

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
