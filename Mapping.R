#install.packages("leaflet")

library(leaflet)
library(sf)
library(crsuggest) #Uses data from the 'EPSG' Registry to look up suitable coordinate reference system transformations for spatial datasets.
library(tmap)
library(RColorBrewer) 


# LOADING DATA-----------------------

states <- readRDS(here("Outputs", "states.rds"))

# Load population data
pop.total <- readRDS(here("Outputs", "pop_total.rds"))
pop.density <- readRDS(here("Outputs", "pop_density.rds"))


# Adding affected counties 
affected_counties <- st_read(here("Shapefiles","AffectedCounties.shp"))

# Adding target counties 
target_counties <- st_read(here("Shapefiles","TargetCounties.shp"))

# Load NOAA Data
storm <- read_csv(here("HurricaneData", "storm_data_search_results_Combined.csv"))
storm_sf <- st_as_sf(storm,
                     coords = c("BEGIN_LON", "BEGIN_LAT"),
                     crs = 4326)

# CREATING MAP-----------------------

# Creating static map of total population ------

ggplot() +
  geom_sf(data = pop.total, fill = "gray95", color = "white") +
  geom_sf(data = st_centroid(pop.total),
          aes(size = pop_total),
          color = "#1c9099", alpha = 0.7) +
  scale_size_continuous(range = c(0.01, 15)) + 
  theme_void()


# Load landslide data, convert to spatial object to include in map
landslides <- read_csv(here("HurricaneData", "Landslide_Inventory_WithCounties.csv"))
landslides_sf <- st_as_sf(landslides, coords = c("X", "Y"), crs = 4326)

# Load HWM Data
hwm <- readxl::read_excel(here("HurricaneData", "Hurricane_Helene_HWM_Database_Table.xlsx"))
hwm_sf <- st_as_sf(hwm,
                   coords = c("Longitude (Decimal Degrees)", "Latitude (Decimal Degrees)"),
                   crs = 4326)


# Creating static map population density --------------

wc_crs <- suggest_top_crs(pop.density) #get top crs (coordinate reference system

# Plotting the data
pop.density %>%
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

interactive_map <- tm_shape(pop.density) +
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
