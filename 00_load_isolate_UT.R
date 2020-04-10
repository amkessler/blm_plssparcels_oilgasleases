# NOTE: The source geodatabase files are not included in this repo due to size.
# They can be downloaded at:
# https://catalog.data.gov/dataset/blm-national-public-land-survey-system-polygons-national-geospatial-data-asset-ngda
# (zipped file ~8GB)

library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)

#use sf to read from parcel boundaries geodatabase

#townships ####
townships <- sf::st_read(dsn = "BLM_National_PLSS/blm_natl_plss.gdb", layer = "PLSSTownship")

glimpse(townships)

#isolate just utah
ut_townships <- townships %>% 
  filter(STATEABBR == "UT")

#save UT as rds file
saveRDS(ut_townships, "geo_data/ut_townships_geo.rds")




# first divisions (sections) ####
firstdivisions <- sf::st_read(dsn = "BLM_National_PLSS/blm_natl_plss.gdb", layer = "PLSSFirstDivision")

glimpse(firstdivisions)

#create column for state base on first part of PLSSID
firstdivisions <- firstdivisions %>% 
  mutate(
    state = str_sub(PLSSID, 1L, 2L)
  )

#isolate just utah
ut_firstdivisions <- firstdivisions %>% 
  filter(state == "UT")

#save as rds file
saveRDS(ut_firstdivisions, "geo_data/ut_firstdivisions_geo.rds")




### STATE boundary files from DOI/BLM to have as base layer ####

stateboundaries <- sf::st_read(dsn = "BLM_National_PLSS/boc_cb_2017_us_state_500k.gdb", layer = "cb_2017_us_state_500k")

glimpse(stateboundaries)

#isolate just utah
ut_stateboundary <- stateboundaries %>% 
  filter(STUSPS == "UT")

#save as rds file
saveRDS(ut_stateboundary, "geo_data/ut_stateboundary_geo.rds")

