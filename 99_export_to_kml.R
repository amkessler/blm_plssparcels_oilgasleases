library(tidyverse)
library(sf)

#load saved file of UT lease sections created in step 00
joined_sections_geo_hasleasedata <- readRDS("processed_data/joined_sections_geo_hasleasedata.rds")

class(joined_sections_geo_hasleasedata)

## In order for the points to be displayed in the correct place they need to be re-projected to WGS84 
#geographical coordinates.
lease_parcels_wgs84roj <- joined_sections_geo_hasleasedata %>% 
  st_transform(4326) # %>% # transform to wgs84...
  # select(Description = NAME) # see https://gdal.org/drivers/vector/kml.html#creation-options

#export to kml
st_write(lease_parcels_wgs84roj, "geo_data/lease_parcels_wgs84roj.kml", driver = "kml")

