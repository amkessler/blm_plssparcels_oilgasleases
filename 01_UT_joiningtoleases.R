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

#for the initial purposes here - trying to figure out how to join to leases - we'll take out the geospatial stuff
firstdivisions <- ut_firstdivisions_geo %>% 
  select(-SHAPE_Length, -SHAPE_Area)

st_geometry(firstdivisions) <- NULL

firstdivisions <- as_tibble(firstdivisions)

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



#do something similar now for the geodata file ####

#there are two id columns to potentially use
firstdivisions %>% 
  select(PLSSID, FRSTDIVID)

#the longer one has the section, the shorter does not. we'll use the longer
#let's look at the string lengths
str_length(firstdivisions$FRSTDIVID)

#good, they're all identical in their length
#let's parse in a similar way to see if they may align with other table
firstdivisions <- firstdivisions %>% 
  mutate(
    ID_township = str_sub(FRSTDIVID, 5L, 9L), #let's hope this is correct, we're going to ignore the 26's altogether
    ID_range = str_sub(FRSTDIVID, 10L, 14L),
    ID_section = str_sub(FRSTDIVID, 18L, 20L)
  ) 


#let's visually inspect the two tables' new columns
firstdivisions %>% 
  select(
    FRSTDIVID, ID_township, ID_range, ID_section
  )

lands_nominated %>% 
  select(
    ld_summary, ld_township, ld_range, ld_section
  ) 


#### JOINING ####

#the moment of truth, let's see if things can join successfully or not
z <- inner_join(lands_nominated, firstdivisions, by = c("ld_township" = "ID_township",
                                                        "ld_range" = "ID_range",
                                                        "ld_section" = "ID_section"
                                                        ))

z

#so this gives us about 50. Is that close to being right, with 600+ original land nomination records? Hmm.

#are there duplicates in the land nominations, and what if we took them out?
lands_nominated %>% 
  count(status)

lands_nominated %>% 
  distinct(ld_township, ld_range, ld_section)

#hmm that doesn't seem to be it.

#let's try creating a different way of joining -- perhaps removing all the zeros in case they aren't
#consistent across the two tables
lands_nominated <- lands_nominated %>% 
  mutate(
    matchfield = paste0(ld_township, ld_range, ld_section),
    matchfield = str_remove_all(matchfield, "0"),
    matchfield = str_squish(matchfield)
  ) %>% 
  select(
    matchfield, everything()
  )

lands_nominated


firstdivisions <- firstdivisions %>% 
  mutate(
    matchfield = paste0(ID_township, ID_range, ID_section),
    matchfield = str_remove_all(matchfield, "0"),
    matchfield = str_squish(matchfield)
  ) %>% 
  select(
    matchfield, everything()
  )

firstdivisions


## ok, now let's see what happens when we try joining again
zz <- inner_join(lands_nominated, firstdivisions, by = "matchfield")

#whew! now we're talking, hopefully. Let's see what we've got here
zz

## which ones left out, if any?
zz_not_joined <- anti_join(lands_nominated, firstdivisions, by = "matchfield")


# With more than 900 results, appears we have multiple matches happening for a section,
# likely because there are subdivisions in the records we've been ignoring.
# Let's try to ferret them out so we've got one record per distinct section itself.
zz %>% 
  count(matchfield, sort = TRUE) %>% 
  filter(n > 1)

#we'll go back to create distinct versions of each table prior to joining
lands_nominated_distinct <- lands_nominated %>% 
  filter(status != "Duplicate") %>% 
  distinct(matchfield, .keep_all = TRUE)

firstdivisions %>% 
  distinct(matchfield, .keep_all = TRUE)

firstdivisions %>% 
  count(matchfield, sort = TRUE)

firstdivisions %>% 
  filter(matchfield == "1S1W1") %>% 
  View()


## ok, now let's see what happens when we try joining again
zz <- inner_join(lands_nominated, firstdivisions, by = "matchfield")




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
