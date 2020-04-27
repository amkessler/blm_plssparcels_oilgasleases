library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)

#load saved files of UT-only geospatial data created in step 00

#townships 
ut_townships_geo <- readRDS("geo_data/ut_townships_geo.rds")

#first divisions (sections)
ut_firstdivisions_geo <- readRDS("geo_data/ut_firstdivisions_geo.rds")

#state boundary
ut_stateboundary <- readRDS("geo_data/ut_stateboundary_geo.rds")



#using tmap

#map out Utah townships
tm_shape(ut_townships_geo) +
  tm_polygons()

#map out just a portion of sections 
ut_section_13 <- ut_firstdivisions_geo %>% 
  filter(FRSTDIVNO == 13)

tm_shape(ut_section_13) +
  tm_polygons()
