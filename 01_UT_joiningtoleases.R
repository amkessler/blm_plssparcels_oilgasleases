library(tidyverse)
library(lubridate)
library(janitor)
library(sf)
library(tmap)
library(tmaptools)

#load saved files of UT-only geospatial data created in step 00

#first divisions (sections)
ut_firstdivisions_geo <- readRDS("geo_data/ut_firstdivisions_geo.rds")

#examine the columns
glimpse(ut_firstdivisions_geo)

#for the purposes here - trying to figure out how to join to leases - we'll take out the geospatial stuff
firstdivisions <- ut_firstdivisions_geo %>% 
  select(-SHAPE_Length, -SHAPE_Area)

st_geometry(firstdivisions) <- NULL

#confirm they're gone
glimpse(firstdivisions)


### Now import the oil/gas lease data of proposed parcels ####
lands_nominated_raw <- read_csv("lease_data/Lands_Nominated_Export_04_09_2020.csv")

lands_nominated_raw %>% 
  clean_names()



#using tmap

#map out just a portion of sections 
ut_section_13 <- ut_firstdivisions_geo %>% 
  filter(FRSTDIVNO == 13)

tm_shape(ut_section_13) +
  tm_polygons()
