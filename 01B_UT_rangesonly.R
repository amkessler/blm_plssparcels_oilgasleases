library(tidyverse)
library(lubridate)
library(janitor)
library(sf)
library(tigris)
library(tmap)
library(tmaptools)

#load saved files of UT-only geospatial data created in step 00

#townships/ranges 
ut_townshipranges_geo <- readRDS("geo_data/ut_townships_geo.rds")

#state boundary
ut_stateboundary <- readRDS("geo_data/ut_stateboundary_geo.rds")

#examine the columns
glimpse(ut_townshipranges_geo)

#new object for use here in the joining
ranges_geo <- ut_townshipranges_geo


### Now import the oil/gas lease data of proposed parcels ####
lands_nominated_raw <- read_csv("https://nflss.blm.gov/api/v1/selectedeoi/csv/UT/2020/ALL/0/ALL?$skip=0") %>% 
  mutate(data_pulled_at = Sys.time()) #add timestamp for when data is pulled

#archive a copy in case needed 
# filestring <- paste0("lease_data/lands_nominated_raw_archived_", Sys.Date(), ".csv")
# write_csv(lands_nominated_raw, filestring)

#clean up names and format date column
lands_nominated <- lands_nominated_raw %>% 
  clean_names() %>% 
  mutate(
    submitted_date = mdy(submitted_date)
  )

glimpse(lands_nominated)


#ld_summary column is the one with the township/range numbers
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

#parse TOWNSHIP AND RANGE based on the location within the string ####
lands_nominated %>% 
  select(ld_summary)

lands_nominated <- lands_nominated %>% 
  mutate(
    ld_township = str_sub(ld_summary, 5L, 9L), 
    ld_range = str_sub(ld_summary, 12L, 16L)
  ) 



#do the same for geodata ####

glimpse(ranges_geo)

#let's look at the string lengths
str_length(ranges_geo$PLSSID)

#good, they're all identical in their length
#let's parse in a similar way 
ranges_geo <- ranges_geo %>% 
  mutate(
    ID_township = str_sub(PLSSID, 5L, 9L), 
    ID_range = str_sub(PLSSID, 10L, 14L)
  ) 


#let's visually inspect the two tables' new columns
ranges_geo %>% 
  select(
    PLSSID, ID_township, ID_range
  )

lands_nominated %>% 
  select(
    ld_summary, ld_township, ld_range
  ) 


#### JOINING ####

#create a unified field for township and range combo to use as unique id for joining
lands_nominated <- lands_nominated %>% 
  mutate(
    matchfield_range = paste0(ld_township, ld_range),
    matchfield_range = str_squish(matchfield_range)
  ) %>% 
  select(
    matchfield_range, everything()
  )

lands_nominated


ranges_geo <- ranges_geo %>% 
  mutate(
    matchfield_range = paste0(ID_township, ID_range),
    matchfield_range = str_squish(matchfield_range)
  ) %>% 
  select(
    matchfield_range, everything()
  )

ranges_geo


# Let's try to ferret out multiple matchfield records so we've got one distinct record per range itself. 
# For mapping things, this should work.
lands_nominated_distinct <- lands_nominated %>% 
  filter(status != "Duplicate") %>% 
  distinct(matchfield_range, .keep_all = TRUE)

ranges_geo_distinct <- ranges_geo %>% 
  distinct(matchfield_range, .keep_all = TRUE)

## now let's join
joined_geo_all <- geo_join(ranges_geo_distinct, lands_nominated_distinct, "matchfield_range", "matchfield_range")

# filter just ones with leases
joined_geo_leases <- joined_geo_all %>% 
  filter(!is.na(ld_summary))


#create map using tmap
#start with static map
# tmap_mode("plot")

tm_shape(joined_geo_leases) +
  tm_polygons(col = "darkred")


#switch to interactive mode to produce leaflet map result
tmap_mode("view")

lease_ranges_map_interactive <- tm_basemap(leaflet::providers$CartoDB.Voyager) +
  tm_shape(joined_geo_leases) +
  tm_polygons(col = "darkred", alpha = .5) +
  tm_tiles("Stamen.TonerLines") 


lease_ranges_map_interactive


