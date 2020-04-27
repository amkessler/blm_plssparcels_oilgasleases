# The database of the oil and gas leases can be found here:  
#   https://nflss.blm.gov/eoi/list  
# Direct csv download here:
#   https://nflss.blm.gov/api/v1/selectedeoi/csv/UT/2020/ALL/0/ALL?$skip=0

library(tidyverse)
library(lubridate)
library(janitor)
library(sf)
library(tigris)
library(tmap)
library(tmaptools)

#load saved files of UT-only geospatial data created in step 00

#first divisions (sections)
firstdivisions_geo <- readRDS("geo_data/ut_firstdivisions_geo.rds")

#examine the columns
glimpse(firstdivisions_geo)



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

#the ld_summary column is the one with the township/range numbers
#the challenge here is trying to match this up to the geo table columns
lands_nominated %>% 
  select(ld_summary)

#let's parse the ld summary column into parts ####

#what's the length of each entry, are they all the same?
str_length(lands_nominated$ld_summary)

#ah - all but the last one. There's an extraneous period in there
lands_nominated %>% 
  select(ld_summary) %>% 
  tail(1)

#let's take that out
lands_nominated <- lands_nominated %>% 
  mutate(
    ld_summary = str_remove_all(ld_summary, "\\.")
  )

#parsing to create the PSSLID equivalent for **Utah** parcels from within the string
#PLSSID format taken from BLM docs referenced in the readme  
lands_nominated %>% 
  select(ld_summary)

lands_nominated <- lands_nominated %>% 
  mutate(
    ld_township = str_sub(ld_summary, 5L, 9L), 
    ld_range = str_sub(ld_summary, 12L, 16L),
    PLSSID = paste0("UT26", ld_township, ld_range, "0")
  ) 

#extract the section number to use as the second joining criteria to natch geo data fields
lands_nominated <- lands_nominated %>% 
  mutate(
    ld_section = str_sub(ld_summary, 41L, 43L),
    FRSTDIVNO = as.numeric(ld_section),
    FRSTDIVNO = as.character(FRSTDIVNO)
  )

#create a single matchstring to use as a unique ID for the join
lands_nominated <- lands_nominated %>% 
  mutate(
    matchstring = str_trim(paste0(PLSSID, FRSTDIVNO))
  )

#reorder columns
lands_nominated <- lands_nominated %>% 
  select(matchstring, PLSSID, FRSTDIVNO, everything())


#are there any matchstring values that appear more than once?
lands_nominated %>% 
  count(matchstring, sort = TRUE) %>% 
  filter(n > 1)

lands_nominated %>% 
  filter(matchstring == "UT260270S0200E04")

#appears to be due to subdivisions being different, which we're not looking at this time...so will roll up to a distinct section.
#note: this means two subdivisions within a section may have different companies/statuses associated with them
#will also remove any records where the status is listed clearly as "Duplicate"
lands_nominated <- lands_nominated %>% 
  filter(status != "Duplicate") %>% 
  distinct(matchstring, .keep_all = TRUE)

#confirm no more duplicate matchstrings
lands_nominated %>% 
  count(matchstring, sort = TRUE) %>% 
  filter(n > 1)

# ok now we should be ready to join
lands_nominated



# create a matchstring field in the geo data based on the same variables
firstdivisions_geo <- firstdivisions_geo %>% 
  mutate(
    matchstring = str_trim(paste0(PLSSID, FRSTDIVNO))
  )

#there are also some non-section records in there for non-surveyed block groups, let's take those out to avoid potential duplicate matches
firstdivisions_geo <- firstdivisions_geo %>% 
  filter(FRSTDIVTXT == "Section")



#let's visually inspect the new table against the geo table's common fields
firstdivisions_geo %>% 
  select(
    matchstring, PLSSID, FRSTDIVNO
  )

lands_nominated %>% 
  select(
    matchstring, PLSSID, FRSTDIVNO
  )




#### JOINING ####

#the moment of truth, let's see if things can join successfully or not
joined_sections_geo <- geo_join(firstdivisions_geo, lands_nominated, "matchstring", "matchstring")

#yay! it worked.
head(joined_sections_geo)

#finally, let's create a field to flag whether there is lease data joined to a geographic boundary or not
joined_sections_geo <- joined_sections_geo %>% 
  mutate(
    has_lease_data = if_else(is.na(status), "N", "Y")
  ) 

joined_sections_geo %>% 
  filter(!is.na(status)) %>% 
  nrow()

joined_sections_geo %>% 
  filter(has_lease_data == "Y") %>% 
  nrow()

#for initial mapping purposes let's create a version with just the parcels with the lease records
joined_sections_geo_hasleasedata <- joined_sections_geo %>% 
  filter(has_lease_data == "Y")

#save object
saveRDS(joined_sections_geo_hasleasedata, "processed_data/joined_sections_geo_hasleasedata.rds")


### MAPPING #####

#create map using tmap
#start with static map
# tmap_mode("plot")
tm_shape(joined_sections_geo_hasleasedata) +
  tm_polygons(col = "darkred")

#switch to interactive mode to produce leaflet map result
tmap_mode("view")

lease_ranges_map_interactive <- tm_basemap(leaflet::providers$CartoDB.Voyager) +
  tm_shape(joined_sections_geo_hasleasedata) +
  tm_polygons(col = "darkred", alpha = .5) +
  tm_tiles("Stamen.TonerLines") 

lease_ranges_map_interactive

#an attempt to see what happens with coloring by status -- WARNING: because of potential multiple subdivisions this shouldn't be used for publication work, but rather
#just to see if certain patterns emerge that we could then analyze with the full data behind them and determine which parcels had multiple subs included and their statuses
tm_basemap(leaflet::providers$CartoDB.Voyager) +
  tm_shape(joined_sections_geo_hasleasedata) +
  tm_polygons("status", alpha = .5) +
  tm_tiles("Stamen.TonerLines") 

