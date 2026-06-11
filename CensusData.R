#install.packages("leaflet")
#install.packages("tidycensus")

library(leaflet)
library(sf)
library(tidycensus)
library(units)
library(crsuggest) #Uses data from the 'EPSG' Registry to look up suitable coordinate reference system transformations for spatial datasets.
library(tmap)
library(RColorBrewer) 



# Getting census data ------------

# Tutorial to work with census data at: https://www.youtube.com/watch?v=1WKAeM8yR-0&t=3s
# Request API key at: https://api.census.gov/data/key_signup.html

#api_key <- 'XXX' ## Add your API key here
api_key <- 'b3b4a0116be055b9ff20c0aa253e8aaaeffa2ebf'
census_api_key(api_key, install = TRUE)

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
    household_data = B11001_001E,
    income_past12M = B17001_002E,
    pop_poverty_def = B17001_001E,
    housing_units_total = B25001_001E,
    geometry = geometry,
    .keep = "none"
  )




# Creating static map of total population ------

ggplot() +
  geom_sf(data = acs.normal, fill = "gray95", color = "white") +
  geom_sf(data = st_centroid(acs.normal),
          aes(size = pop_total),
          color = "#1c9099", alpha = 0.7) +
  scale_size_continuous(range = c(0.01, 15)) + 
  theme_void()

# Calculate area in square meters, convert to sq miles, and estimate population density
acs.normal_density <- acs.normal %>%
  mutate(
    # st_area returns area in square meters
    area_sq_meters = st_area(geometry),
    # Convert to square miles
    area_sq_miles = set_units(area_sq_meters, "mi2"),
    # Calculate population density (people / sq. mile)
    pop_density = pop_total/as.numeric(area_sq_miles)
  )



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

pal <- brewer.pal(9, "YlGnBu") # Setting color palette

tmap_mode("view") # setting mapviewer to interactive

tm_shape(acs.normal_density) +
  tm_polygons(fill = "pop_density",
              alpha = 0.6, #adding some transparency to the map to allow labels visibility
              n = 9,
              style = "jenks",
              midpoint = NA,
              lwd = 0.5,
              palette = pal,
              border.col = "black")  +
  tmap_options(basemaps = "CartoDB.Positron") 



# Creating interactive map with leaflet ------------

# A leaflet tutorial to get started: https://mikejohnson51.github.io/leaflet-intro/index.html

leaflet() %>% 
  setView(lng=-105.0848, lat=40.5729, zoom = 12) %>% 
  addProviderTiles(providers$CartoDB)
