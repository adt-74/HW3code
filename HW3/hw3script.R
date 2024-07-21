library(dplyr) # data wrangling
library(readr) # reading data
library(ggplot2) # data vizualization
library(tidygraph)
library(sf)
library(ggspatial)
library(viridis)


startdate = "2019-08-28"
enddate = "2019-09-07"


aea <- "+proj=aea +lat_0=24 +lon_0=-84 +lat_1=24 +lat_2=31.5 +x_0=400000 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs" 
#all Florida Projection
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

counties <- read_sf("counties.geojson")
countysub <- read_sf("county_subdivisions.geojson")
states <- read_sf("county_subdivisions.geojson")



read_rds("dorian.rds") %>%
  # Focus on just the nodes of the network
  activate("nodes") %>%
  # Turn it into a tibble/data.frame
  as_tibble() %>%
  # Create a unique node id,
  # which we'll use to join in geoid to the from and to columns of the edges
  mutate(node = 1:n()) %>%
  # Reorder the variables; dropping geometry
  select(node, geoid, social_capital:evacuation_less, geometry) %>%
  # Save the nodes
  saveRDS("nodes.rds")

# Load the nodes in
nodes = read_rds("nodes.rds")


# Save the edges to file.
read_rds("dorian.rds") %>%
  # Focus on just the edges of the network
  activate("edges") %>%
  # Turn it into a tibble/data.frame
  as_tibble() %>%
  # Drop the geometry 
 # select(-geometry) %>%
  # Let's join the source (from) geoid in using the shared node id
  left_join(by = c("from" = "node"), y = nodes %>% select(node, from_geoid = geoid)) %>%
  # Let's join the destination (to) geoid in using the shared node id 
  left_join(by = c("to" = "node"), y = nodes %>% select(node, to_geoid = geoid)) %>%
  # Save this to file
  saveRDS("edges.rds")
edges = read_rds("edges.rds")


##################################################################################################################
countynames = read.csv("County.csv")


#creates the data to be used for the plots, formatted and filtered by the date.
data_p = edges %>%
  mutate(date = as.Date(date_time), time = format(as.POSIXct(date_time),format = "%H:%M:%S")) %>%
  mutate( c_from = as.integer(substr(from_geoid,1,5)), c_to = as.integer(substr(to_geoid,1,5))) %>%
  left_join(countynames, by =c("c_from" = "County_Code")) %>%
  rename(From_County = County_Name) %>%
  left_join(countynames, by =c("c_to" = "County_Code")) %>% 
  #Joins the county names table tother to match with the and from county ids
  rename(To_County = County_Name) %>%
  select(from:From_County, To_County,State = State.x) %>%
  mutate(EVAC_SHEL = if_else(evacuation > 0, "Evac", "Shel"))



#sort the datta to what to analyize

#sirts it between the dates of hurricane dorian and find which counties have the top counties
data_sf <- data_p %>% 
  filter(EVAC_SHEL == "Evac", between(date, as.Date(startdate), as.Date(enddate)), State == "Florida") %>% 
  select(From_County,To_County,c_from, c_to, from_geoid,to_geoid, geometry,km, evacuation, date) %>% 
  group_by(From_County,To_County, geometry) %>% 
  summarise(sum_evac= sum(evacuation), distance = mean(km)) %>% 
  arrange(desc(sum_evac)) %>%
  filter(sum_evac >100) %>% 
  st_as_sf()
  
#filter the counties to only show florida

counties_f <- counties %>% 
  filter(state == "FL")

#creates the top counties to be joined to crate county nodes
topdata_sf <- data_p %>% 
  filter(EVAC_SHEL == "Evac", between(date, as.Date(startdate), as.Date(enddate)), State == "Florida") %>% 
  select(From_County,To_County,c_from, c_to, from_geoid,to_geoid, geometry,km, evacuation, date) %>% 
  group_by(From_County,c_from) %>% 
  summarise(sum_evac= sum(evacuation), distance = mean(km)) %>% 
  arrange(desc(sum_evac)) %>%
  filter(sum_evac >100) %>% 
  mutate(c_from = as.character(c_from))

#left joins the counties
top_count <- counties_f %>% 
  left_join(topdata_sf, by = c("geoid"="c_from")) %>% 
  arrange(desc(sum_evac)) %>%
  head(3) %>% 
  st_as_sf()




ggplot() +
  geom_sf(data = counties_f, fill = "transparent", color = "black") +
  geom_sf(data= top_count, aes(fill = From_County), color = "red") + # Map the fill to the county names
  geom_sf(data = data_sf, aes(geometry = geometry, color = sum_evac), size = 4) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Evacuations") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral"), name = "Top 3 Counties") + # Manual color scale for top counties
  scale_size_continuous(range = c(0.5, 2), guide = 'none') +
  labs(title = "Evacuation Routes in Florida on 2019-08-28 to 2019-09-07", subtitle = "Each line represents where individuals traveled from each country", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))




#######################################

#Filtering data to get the top sub countys that people reside in.  

nodes2 <- nodes %>% 
  select(geoid,geometry)


topnodes <- data_p %>% 
  filter(EVAC_SHEL == "Evac", between(date, as.Date(startdate), as.Date(enddate)), State == "Florida") %>% 
  select(From_County,To_County,c_from, c_to, from_geoid,to_geoid, geometry,km, evacuation, date) %>% 
  group_by(From_County, To_County,from_geoid) %>% 
  summarise(sum_evac= sum(evacuation), distance = mean(km)) %>% 
  arrange(desc(sum_evac)) %>%
  filter(sum_evac >100)

topnodes <- topnodes %>%
  left_join(countysub, by = c("from_geoid" = "geoid")) %>% 
  select(From_County, from_geoid, To_County, sum_evac,name,geometry, distance) %>%
  filter( From_County %in% c("Miami-Dade County", "Broward County")) %>%
  head(3) %>% 
  st_as_sf()




#zooms into the bottom right. 


ggplot() +
  geom_sf(data = counties_f, fill = "transparent", color = "black") +
  geom_sf(data = top_count %>% filter(From_County %in% c("Miami-Dade County", "Broward County")), aes(fill = From_County), color = "red") + 
  geom_sf(data = topnodes, aes(fill = name), color = "black", shape = 21, size = 5) +
  geom_sf(data = data_sf, aes(geometry = geometry, color = sum_evac), size = 5) +
  scale_color_viridis_c(option = "plasma", direction = -1, name = "Evacuations") +
  scale_fill_manual(values = c("Miami-Dade County" = "lightblue", "Broward County" = "lightgreen", "Miami" = "pink"   , "Kendall-Palmetto Bay" = "orange", "Deerfield Beach" = "wheat3"   ), 
                    name = "County & Sub County",
                    labels = c("Miami-Dade County", "Broward County", unique(topnodes$name))) +
  scale_size_continuous(range = c(0.5, 2), guide = 'none') +
  labs(title = "Top Evacuations from SE Florida between 2019-08-28 to 2019-09-07", subtitle = "Evacuations Between Counties and Sub Counties", x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-81, -80), ylim = c(25, 27)) +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))




