#load packages
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(ggplot2)
library(dplyr)
library(usmap)
library(sf)
library(tigris)

#read data
hurricaneHWM <- readxl::read_excel(here("hurricane-helene-data", "HurricaneData", "Hurricane_Helene_HWM_Database_Table.xlsx")) %>%
  clean_names()

hurricaneLS <- read_csv(here("hurricane-helene-data", "HurricaneData", "Landslide_Inventory_WithCounties.csv"))

HWMbyHeight <- hurricaneHWM %>% arrange(-hwm_elevation_ft) %>%
  select(hwm_type, hwm_elevation_ft, northing, easting, county, state)

HWMbyCounty <- hurricaneHWM %>% arrange(-hwm_elevation_ft) %>%
  select(hwm_type, hwm_elevation_ft, northing, easting, county, state)

landslidesByCounty <- hurricaneLS %>% arrange(COUNTYNAME)

HWMbyHeight %>% count(state, sort = TRUE)
HWMbyCounty %>% count(county, sort = TRUE)

county_counts <- landslidesByCounty %>%
  group_by(COUNTYNAME, STATE) %>%
  count(COUNTYNAME) %>% clean_names()

county_counts_impact <- landslidesByCounty %>%
  group_by(COUNTYNAME, STATE) %>%
  count(COUNTYNAME, Impact)

#disaster types
#disaster <- read_csv(here("HurricaneData", "us_disaster_declarations.csv")) %>%
  #count(incident_type)

#plotting data 
ggplot(HWMbyCounty, aes(x = county, y = hwm_elevation_ft)) +
  geom_point() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(HWMbyCounty, aes(x = paste(county, state, sep = ", "), y = hwm_elevation_ft)) +
  stat_summary(fun = mean, geom = "bar")

ggplot(hurricaneLS, aes(x = COUNTYNAME)) +
  geom_bar()

##
landslides <- data.frame(
  County = c("Alleghany","Ashe","Avery","Blount","Buncombe","Burke","Caldwell",
             "Carter","City of Bristol","Clay","Cleveland","Cocke","Grayson",
             "Greene","Greenville","Haywood","Henderson","Jackson","Johnson",
             "Macon","Madison","McDowell","Mitchell","Monroe","Pickens","Polk",
             "Rutherford","Scott","Sevier","Smyth","Stephens","Sullivan","Surry",
             "Swain","Towns","Transylvania","Unicoi","Union","Washington",
             "Washington","Watauga","Wilkes","Wythe","Yancey"),
  State = c("NC","NC","NC","TN","NC","NC","NC","TN","VA","NC","NC","TN","VA","TN",
            "SC","NC","NC","NC","TN","NC","NC","NC","NC","TN","SC","NC","NC","VA",
            "TN","VA","GA","TN","NC","NC","GA","NC","TN","GA","TN","VA","NC","NC",
            "VA","NC"),
  Landslide_Count = c(7,99,139,1,329,13,11,8,1,1,2,8,11,10,39,53,253,5,24,8,9,
                      320,43,1,6,69,93,1,21,1,1,3,1,2,1,10,9,1,8,10,270,10,3,302)
)

options(tigris_use_cache = TRUE)

counties <- counties(state = c("NC","TN","VA","SC","GA"), cb = TRUE, class = "sf")

# Clean names
landslides <- landslides %>%
  mutate(County = tolower(County))

counties <- counties %>%
  mutate(County = tolower(NAME),
         State = STUSPS)

# Join
map_data <- counties %>%
  left_join(landslides, by = c("County", "State"))

centroids <- st_centroid(map_data)

data_only <- map_data %>% filter(!is.na(Landslide_Count))

ggplot() +
  geom_sf(data = data_only, fill = "gray95", color = "white") +
  geom_sf(data = st_centroid(data_only),
          aes(size = Landslide_Count),
          color = "red", alpha = 0.7) +
  scale_size_continuous(range = c(1, 10)) +
  theme_minimal()