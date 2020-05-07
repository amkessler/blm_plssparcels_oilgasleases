library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(ggmap)


#save google api key for ggmap - just needed to run once per machine
# register_google(key = "[your key]", write = TRUE)


#load saved file of UT lease sections created in step 00
joined_sections_geo_hasleasedata <- readRDS("processed_data/joined_sections_geo_hasleasedata.rds")


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

#let's try a different basemap 
tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
  tm_shape(joined_sections_geo_hasleasedata) +
  tm_polygons(col = "darkred", alpha = .4) +
  tm_tiles("Stamen.TonerLines") 

#yeah, that's a nice one for our purposes - let's stick with that basemap for now
#save as an named object
utah_lease_map <- tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
  tm_shape(joined_sections_geo_hasleasedata) +
  tm_polygons(col = "darkred", alpha = .4) +
  tm_tiles("Stamen.TonerLines") 

utah_lease_map



##bubbles instead of polygons
tm_basemap(leaflet::providers$Esri.WorldTopoMap) +
  tm_shape(joined_sections_geo_hasleasedata) +
  tm_bubbles(col = "green", alpha = .4, size = .2) 



# 
# #an attempt to see what happens with coloring by status -- WARNING: because of potential multiple subdivisions this shouldn't be used for publication work, but rather
# #just to see if certain patterns emerge that we could then analyze with the full data behind them and determine which parcels had multiple subs included and their statuses
# tm_basemap(leaflet::providers$CartoDB.Voyager) +
#   tm_shape(joined_sections_geo_hasleasedata) +
#   tm_polygons("status", alpha = .5) +
#   tm_tiles("Stamen.TonerLines") 




#static maps with ggplot ####

ggplot(joined_sections_geo_hasleasedata) + 
  geom_sf(aes(fill=status))

test <- ggplot(joined_sections_geo_hasleasedata) + 
  geom_sf()


#maps with ggmap ####

ut_basemap <- get_map(location=c(lon = -111.0937, lat = 39.3210), zoom=7, maptype = 'terrain-background', source = 'stamen')
ggmap(ut_basemap)


ut_basemap2 <- get_map(location=c(lon = -111.0937, lat = 39.3210), zoom=7, maptype = 'toner-lite', source = 'stamen')
ggmap(ut_basemap2)


map <- get_googlemap("Moab, Utah", zoom = 9, maptype = "terrain")
ggmap(map) 



### combine both ggmap and ggplot







