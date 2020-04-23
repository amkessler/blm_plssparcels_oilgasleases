library(tidyverse)
library(lubridate)
library(janitor)
library(sf)

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

#let's try creating a different way of joining -- creating one matchfield and clearing possible hidden whitespace
lands_nominated <- lands_nominated %>% 
  mutate(
    matchfield = paste0(ld_township, ld_range, ld_section),
    matchfield = str_squish(matchfield)
  ) %>% 
  select(
    matchfield, everything()
  )

lands_nominated


firstdivisions <- firstdivisions %>% 
  mutate(
    matchfield = paste0(ID_township, ID_range, ID_section),
    matchfield = str_squish(matchfield)
  ) %>% 
  select(
    matchfield, everything()
  )

firstdivisions


## ok, now let's see what happens when we try joining again
zz <- inner_join(lands_nominated, firstdivisions, by = "matchfield")

#well that didn't help, we got the same results
zz

## which ones left out
zz_not_joined <- anti_join(lands_nominated, firstdivisions, by = "matchfield")

#let's look at the lengths of the match strings
str_length(lands_nominated$matchfield)

test <- as_tibble(str_length(firstdivisions$matchfield))
test %>% 
  filter(value != 13)

#Hmm, something else is amiss here.

# Let's try to ferret out multiple matchfield records so we've got one record per distinct 
# section itself. (Since there were multiple in there due to subdivisions we've ignored for this.)

#we'll go back to create distinct versions of each table prior to joining
lands_nominated_distinct <- lands_nominated %>% 
  filter(status != "Duplicate") %>% 
  distinct(matchfield, .keep_all = TRUE)

firstdivisions_distinct <- firstdivisions %>% 
  distinct(matchfield, .keep_all = TRUE)

## ok, now let's see what happens when we try joining again
zzz <- inner_join(lands_nominated_distinct, firstdivisions_distinct, by = "matchfield")

#even less, which is perhaps to be expected. But shows us how many uniques are actually joining.

# So something must be up here with the parsing not resulting in the same measures in the first place
# for each table... townships, ranges, sections


# before going deeper, what would happen if we looked at just to the range level? Left out sections.
lands_nominated_rangedistinct <- lands_nominated %>% 
  filter(status != "Duplicate") %>% 
  distinct(ld_township, ld_range, .keep_all = TRUE)

firstdivisions_rangedistinct <- firstdivisions %>% 
  distinct(ID_township, ID_range, .keep_all = TRUE)

#join
joined_rangeonly <- inner_join(lands_nominated_rangedistinct, firstdivisions_rangedistinct,
           by = c("ld_township" = "ID_township",
                  "ld_range" = "ID_range"))

#well ok now, maybe we're getting somewhere. All but one of the ranges nominated matched up!

joined_rangeonly <- joined_rangeonly %>% 
  select(-matchfield.x, -matchfield.y)

joined_rangeonly

# so something clearly going on with the **sections** that's causing the trouble...and perhaps it's worth
# asking before going further if we should stick with just ranges overall for now. 
# i.e. deal with sections later, but use ranges alone as the way to locate UT parcels at a higher level.

