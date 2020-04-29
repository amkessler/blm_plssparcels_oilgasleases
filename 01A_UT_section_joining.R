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
filestring <- paste0("lease_data/lands_nominated_raw_archived_", Sys.Date(), ".csv")
write_csv(lands_nominated_raw, filestring)

#clean up names and format date column
lands_nominated <- lands_nominated_raw %>% 
  clean_names() %>% 
  mutate(
    submitted_date = mdy(submitted_date),
    submitted_month = month(submitted_date),
    submitted_year = year(submitted_date) #they're all currently 2020 but capturing this anyway
  )

glimpse(lands_nominated)

#counting how many records of each status category
lands_nominated %>% 
  count(status, sort = TRUE)

#by month
lands_nominated %>% 
  count(submitted_month, status) %>% 
  arrange(submitted_month, desc(n))


# Any ld_summary listings more than once (b/c of subdivisions)
lands_nominated %>% 
  count(ld_summary, sort = TRUE)

lands_nominated %>% 
  filter(ld_summary == "UT T0260S-R0230E SALT LAKE MER Section: 021")

#duplicate subdivisions?
lands_nominated %>% 
  count(ld_summary, subdivision, sort = TRUE)

#less than a handful. they're either listed wtih status duplicate or withdrawn.
lands_nominated %>% 
  filter(ld_summary == "UT T0270S-R0200E SALT LAKE MER Section: 004")




#### PARSING ####

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

#a very unusual rangle that include a period, Range 17.5
#let's take that out for now, and then put it back in separately at the end of the process.
lands_outliertoaddlater <- lands_nominated %>% 
  filter(ld_summary == "UT T0250S-R17.50E SALT LAKE MER Section: 001")

lands_nominated <- lands_nominated %>% 
  filter(ld_summary != "UT T0250S-R17.50E SALT LAKE MER Section: 001")

#seeing if there's an id in the geo file that might be the one supposed to match this...
temp_geocheck <- firstdivisions_geo %>% 
  filter(str_sub(PLSSID, 1L, 12L) == "UT260250S017") 


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
    ld_section = str_sub(ld_summary, 42L, 43L),
    FRSTDIVNO = ld_section
  )

#create a single matchstring to use as a unique ID for the join
lands_nominated <- lands_nominated %>% 
  mutate(
    matchstring = str_squish(paste0(PLSSID, FRSTDIVNO))
  )


#now we'll put that one **outlier** record with the 17.5 back in manually here
lands_outliertoaddlater

# *** STILL WORK IN PROGRESS HERE ####

# 
# 
# #add it back to main table
# temp <- bind_rows(lands_nominated, lands_outliertoaddlater)
# lands_nominated <- temp

#reorder columns
lands_nominated <- lands_nominated %>% 
  select(matchstring, PLSSID, FRSTDIVNO, everything())


#are there any matchstring values that appear more than once?
lands_nominated %>% 
  count(matchstring, sort = TRUE) %>% 
  filter(n > 1)

lands_nominated %>% 
  filter(matchstring == "UT260270S0200E004")

#appears to be due to subdivisions being different, which we're not looking at this time...so will roll up to a distinct section.
#note: this means two subdivisions within a section may have different companies/statuses associated with them
#will also remove any records where the status is listed clearly as "Duplicate"
lands_nominated_distinct <- lands_nominated %>% 
  filter(status != "Duplicate") %>% 
  distinct(matchstring, .keep_all = TRUE)

#confirm no more duplicate matchstrings
lands_nominated_distinct %>% 
  count(matchstring, sort = TRUE) %>% 
  filter(n > 1)

# ok now we should be ready to join
lands_nominated_distinct



# create a matchstring field in the geo data based on the same variables
firstdivisions_geo <- firstdivisions_geo %>% 
  mutate(
    matchstring = str_squish(paste0(PLSSID, FRSTDIVNO))
  )

#there are also some non-section records in there for non-surveyed block groups, let's take those out to avoid potential duplicate matches
firstdivisions_geo <- firstdivisions_geo %>% 
  filter(FRSTDIVTXT == "Section")



#let's visually inspect the new table against the geo table's common fields
firstdivisions_geo %>% 
  select(
    matchstring, PLSSID, FRSTDIVNO
  )

lands_nominated_distinct %>% 
  select(
    matchstring, PLSSID, FRSTDIVNO
  )




#### JOINING ####

#before doing the geo_join, we'll do a regular join just to see if anything falls out and doesn't match
inner_join(lands_nominated_distinct, firstdivisions_geo, by = "matchstring")

anti_join(lands_nominated_distinct, firstdivisions_geo, by = "matchstring") %>% 
  View()

anti_join(lands_nominated_distinct, firstdivisions_geo, by = "matchstring") %>% 
  write_csv("output/notjoined_landsnominated.csv")


#looks like four records falling out -- **will investigate to find out why
lands_nominated_distinct %>% 
  filter(PLSSID == "UT260250S0170E0")

firstdivisions_geo %>% 
  filter(PLSSID == "UT260250S0170E0") 

firstdivisions_geo %>% 
  filter(PLSSID == "UT260250S0170E0")  %>% 
  count(PLSSID, FRSTDIVNO) %>% 
  arrange(FRSTDIVNO)
#ah ha-- there's no section 01, 02 or 04 in the geo file itself able to match up against the lands nominated
#wonder why that is?
#explains three of the four so far that didn't join

#let's look at the last one
lands_nominated_distinct %>% 
  filter(PLSSID == "UT260250S1750E0")

firstdivisions_geo %>% 
  filter(PLSSID == "UT260250S1750E0")

#ha, that plssid doesn't exist at all in the geo file. Could it have been a typo, possibly meant to be the same as the other three?
#if so, bad data entry overall might explain why all four of these are not there, if they were entered incorrectly in the oil lease data

#going to reload the enormous national geodatabe file for sections from step 00. See whether there
#is any possibility those records may have gotten filtering out in the Utah isolation?

# temp_missingcheck <- firstdivisions %>% 
#   filter(PLSSID %in% c("UT260250S0170E0", "UT260250S1750E0"))

#save to avoid having to repeat this again
# saveRDS(temp_missingcheck, "geo_data/temp_missingcheck.rds")

#to start here instead load the temp file saved above
temp_missingcheck <- readRDS("geo_data/temp_missingcheck.rds")

#are there two?
temp_missingcheck %>% 
  count(PLSSID)

#there aren't. So now we know the solo missing one with no plssid in our ut data doesn't exist at all in the national file.

#and what about the low numbered sections in our one remaining plssid?
temp_missingcheck %>% 
  arrange(FRSTDIVNO)

#nope, they don't exist either. 

#one additional step, let's see what companies are active in the plssid we do have in the nominations data
lands_nominated %>% 
  filter(PLSSID == "UT260250S0170E0") %>% 
  count(nominator)

#they're all from the same company, Prairie Hills Oil and Gas. 
#What else does that company have associated with it - any other sections with same numbers in other ranges?
lands_nominated %>% 
  filter(str_detect(nominator, "Prairie Hills")) %>% 
  count(nominator, PLSSID, FRSTDIVNO) %>% 
  filter(FRSTDIVNO %in% c("01", "02", "04"))

#could this get us closer to sorting out?


#another question -- does each range have a section 01? Do some just not have one in the first place...
#will use the UT data to see
#slice a temporary table out without the shape data to avoid it taking a LONG time to run a count.
temp_forcount <- firstdivisions_geo 
st_geometry(temp_forcount) <- NULL

temp_count <- temp_forcount %>% 
  count(PLSSID, FRSTDIVNO) %>% 
  arrange(PLSSID, FRSTDIVNO)

num_plss <- temp_count %>% 
  count(PLSSID) %>% 
  nrow()

num_01sections <- temp_count %>% 
  filter(FRSTDIVNO == "01") %>% 
  count(PLSSID) %>% 
  nrow()

num_plss - num_01sections

#ok then...so there are indeed a few hundred ranges that simply don't have section 01.
#that would appear to be even more evidence that the problem here is with the nominations data itself, not the geodata.

#we'll table this for now, and proceed with the geo_joining without those four, while the investigation into that continues.





#geojoin #### -----------------------

#the moment of truth, let's see if things can join successfully or not geospatially
joined_sections_geo <- geo_join(firstdivisions_geo, lands_nominated_distinct, "matchstring", "matchstring")

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

tm_basemap(leaflet::providers$CartoDB.Voyager) +
  tm_shape(joined_sections_geo_hasleasedata) +
  tm_polygons(col = "darkred", alpha = .5) +
  tm_tiles("Stamen.TonerLines") 

#an attempt to see what happens with coloring by status -- WARNING: because of potential multiple subdivisions this shouldn't be used for publication work, but rather
#just to see if certain patterns emerge that we could then analyze with the full data behind them and determine which parcels had multiple subs included and their statuses
tm_basemap(leaflet::providers$CartoDB.Voyager) +
  tm_shape(joined_sections_geo_hasleasedata) +
  tm_polygons("status", alpha = .5) +
  tm_tiles("Stamen.TonerLines") 



#what if we map the section with the missing 01, 02, etc... see if visually there's something missing or not?
tm_basemap(leaflet::providers$CartoDB.Voyager) +
  tm_shape(temp_missingcheck) +
  tm_polygons(col = "darkred", alpha = .7) +
  tm_tiles("Stamen.TonerLines") 


#map out the just the recent one from Apr 24
# UT260200S0230E009
newonly <- joined_sections_geo_hasleasedata %>% 
  filter(matchstring == "UT260200S0230E009")





#### MORE SEARCHING FOR MYSTERY 17.5 RANGE ####

#potential areas for the mystery 17.5 range?
tm_basemap(leaflet::providers$CartoDB.Voyager) +
  tm_shape(temp_geocheck) +
  tm_polygons(col = "darkred", alpha = .7) +
  tm_tiles("Stamen.TonerLines") 



#looking for Prairie Hills parcels only
phills_distinct <- lands_nominated %>% 
  filter(str_detect(nominator, "Prairie Hills"),
         status != "Duplicate") %>% 
  distinct(matchstring, .keep_all = TRUE)

#geo join
phills_geo <- geo_join(firstdivisions_geo, phills_distinct, "matchstring", "matchstring")
phills_geo <- phills_geo %>% 
  filter(!is.na(nominator))

# map prairie hills parcels
tm_basemap(leaflet::providers$CartoDB.Voyager) +
  tm_shape(phills_geo) +
  tm_polygons(col = "green", alpha = .7) +
  tm_tiles("Stamen.TonerLines") 


#both together
tm_basemap(leaflet::providers$CartoDB.Voyager) +
  tm_shape(temp_geocheck) +
  tm_polygons(col = "darkred", alpha = .7) +
  tm_shape(phills_geo) +
  tm_polygons(col = "green", alpha = .7) 
  
#This may be the plssid of the mystery range?
# UT260250S0172E0

