library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)

#load saved file of UT lease sections created in step 00

joined_sections_geo_hasleasedata <- readRDS("processed_data/joined_sections_geo_hasleasedata.rds")


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

