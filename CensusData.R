#install.packages("leaflet")
#install.packages("tidycensus")

library(leaflet)
library(sf)
library(tidycensus)
library(units)
library(crsuggest) #Uses data from the 'EPSG' Registry to look up suitable coordinate reference system transformations for spatial datasets.
library(tmap)
library(RColorBrewer) 
library(here)
library(tidyverse)
library(tidyr)



# Getting census data ------------

# Tutorial to work with census data at: https://www.youtube.com/watch?v=1WKAeM8yR-0&t=3s
# Request API key at: https://api.census.gov/data/key_signup.html

api_key <- '' ## Add your API key here
census_api_key(api_key, install = TRUE, overwrite=TRUE)

year <- 2023

# Defining sociodemographic variable to explore: total population ------
variable <- "B01001_001"  # Total Population (POP)


# Getting total population data for the selected states ----
acs.raw <- get_acs(
  geography = "county", 
  variables = variable, 
  year = year, 
  state = c("NC","TN","VA","SC","GA"),
  geometry = TRUE,
  key = api_key,
  survey = "acs5", 
  output = "wide"
)

acs.normal <- acs.raw %>% 
  mutate(
    geoid = GEOID,
    name = NAME,
    pop_total = B01001_001E,
    geometry = geometry,
    .keep = "none"
  )

counties <- acs.normal %>% 
  separate(name, #separating information about county and state in two different columns
           sep = ",",
           into = c("county", "state"), 
           remove = FALSE) %>% 
  mutate(county = str_remove(county," County")) #removing the word "County" after their names


# Creating state boundary polygons
states <- counties %>% 
  group_by(state) %>% 
  summarise(geometry = st_union(geometry))


# Adding affected counties 
affected_counties <- st_read(here("Shapefiles","AffectedCounties.shp"))

# Adding target counties 
target_counties <- st_read(here("Shapefiles","TargetCounties.shp"))


# Creating static map of total population ------

ggplot() +
  geom_sf(data = acs.normal, fill = "gray95", color = "white") +
  geom_sf(data = st_centroid(acs.normal),
          aes(size = pop_total),
          color = "#1c9099", alpha = 0.7) +
  scale_size_continuous(range = c(0.01, 15)) + 
  theme_void()

# Calculate area in square meters, convert to sq miles, and estimate population density
acs.normal_density <- counties %>%
  mutate(
    # st_area returns area in square meters
    area_sq_meters = st_area(geometry),
    # Convert to square miles
    area_sq_miles = set_units(area_sq_meters, "mi2"),
    # Calculate population density (people / sq. mile)
    pop_density = pop_total/as.numeric(area_sq_miles)
  )

# Load landslide data, convert to spatial object to include in map
landslides <- read_csv(here("HurricaneData", "Landslide_Inventory_WithCounties.csv"))
landslides_sf <- st_as_sf(landslides, coords = c("X", "Y"), crs = 4326)

# Load HWM Data
hwm <- readxl::read_excel(here("HurricaneData", "Hurricane_Helene_HWM_Database_Table.xlsx"))
hwm_sf <- st_as_sf(hwm,
                   coords = c("Longitude (Decimal Degrees)", "Latitude (Decimal Degrees)"),
                   crs = 4326)
# Load NOAA Data
storm <- read_csv(here("HurricaneData", "storm_data_search_results_Combined.csv"))
storm_sf <- st_as_sf(storm,
                     coords = c("BEGIN_LON", "BEGIN_LAT"),
                     crs = 4326)


# Creating static map population density --------------

wc_crs <- suggest_top_crs(acs.normal_density) #get top crs (coordinate reference system

# Plotting the data
acs.normal_density %>%
  ggplot(aes(fill = pop_density)) +
  geom_sf(linewidth = 0.1, color = 'gray') + # 'color = NA' removes the outline of the polygons
  scale_fill_distiller(palette = "Blues", direction = 1) + # Example color scale
  labs(title = "Population Density by County",
       subtitle = "2023 ACS Estimates") +
  coord_sf(crs = wc_crs) +
  theme_void() # Removes axes and grid lines for a cleaner map



# Creating interactive map with tmap ------------

# an intro to this package:https://r-tmap.github.io/tmap/

pal <- brewer.pal(9, "YlGnBu")
tmap_mode("view")

interactive_map <- tm_shape(acs.normal_density) +
  tm_polygons(
    fill = "pop_density",
    fill.scale = tm_scale_intervals(
      n = 9,
      style = "jenks",
      values = pal
    ),
    fill_alpha = 0.6,
    lwd = 0.5,
    col = "black"
  ) +
  # tm_text(
  # text = "county",        # show name of the county
  # size = 1,
  # col = "black"
  # ) +
  tm_shape(target_counties, name = "Target Counties") +
  tm_polygons(
    fill = "#969696",
    fill_alpha = 0.4) +
  tm_shape(hwm_sf, name = "High Water Marks") +
  tm_dots(
    fill = "#02818a",
    size = 0.25,
    popup.vars = c("HWM Name", "HWM Type", "HWM Quality",
                   "HWM Elevation (ft)", "Stream", "County", "State")
  ) +
  tm_shape(states) +
  tm_borders(col = "black", lwd = 2.5, lty = "solid") +
  tm_shape(affected_counties) +
  tm_borders(col = "#a50f15", lwd = 1.5, lty = "solid") +
  tm_shape(landslides_sf, name = "Landslides") +
  tm_symbols(
    fill = "Impact",
    fill.scale = tm_scale_categorical(values = "brewer.set1"),
    fill.legend = tm_legend(title = "Landslide Impact"),
    shape = 21,           # circle
    size = 0.5,
    col = NA,
  ) +
  tm_shape(storm_sf, name = "Storm Events") +
  tm_dots(
    fill = "#FF69B4",    # pink to differentiate from other layers
    size = 0.5,
    popup.vars = c("EVENT_TYPE", "STATE_ABBR", "CZ_NAME_STR",
                   "BEGIN_LOCATION", "DEATHS_DIRECT", "INJURIES_DIRECT",
                   "DAMAGE_PROPERTY_NUM", "DAMAGE_CROPS_NUM")
  ) + 
  tm_basemap("CartoDB.Positron")

tmap_save(interactive_map, here("Outputs", "map.html"))



# Creating interactive map with leaflet ------------

# A leaflet tutorial to get started: https://mikejohnson51.github.io/leaflet-intro/index.html

leaflet() %>% 
  setView(lng=-105.0848, lat=40.5729, zoom = 12) %>% 
  addProviderTiles(providers$CartoDB)
