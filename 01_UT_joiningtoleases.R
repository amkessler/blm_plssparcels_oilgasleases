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

lands_nominated <- lands_nominated_raw %>% 
  clean_names() %>% 
  mutate(
    submitted_date = mdy(submitted_date)
  )

glimpse(lands_nominated)

#the ld_summary column is the one with the township/range numbers
#the challenge here is trying to match this up to the geo table columns
lands_nominated %>% 
  select(ld_summary)


#let's parse the ld summary column into parts ####

#what's the length of each entry, are they all the same?
str_length(lands_nominated$ld_summary)

#ah - all but the last one! There's an extraneous period in there
lands_nominated %>% 
  select(ld_summary) %>% 
  tail(1)

#let's take that out
lands_nominated <- lands_nominated %>% 
  mutate(
    ld_summary = str_remove_all(ld_summary, "\\.")
  )

#great, now let's start parsing based on the location within the string
lands_nominated %>% 
  select(ld_summary)

lands_nominated <- lands_nominated %>% 
  mutate(
    ld_township = str_sub(ld_summary, 5L, 9L),
    ld_range = str_sub(ld_summary, 12L, 16L),
    ld_section = str_sub(ld_summary, 41L, 43L)
  ) 






# 
# 
# #### MAPPING ####
# 
# #map out just a portion of sections 
# ut_section_13 <- ut_firstdivisions_geo %>% 
#   filter(FRSTDIVNO == 13)
# 
# tm_shape(ut_section_13) +
#   tm_polygons()
