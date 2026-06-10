#install.packages("leaflet")
#install.packages("tidycensus")

library(leaflet)
library(sf)
library(tidycensus)


### Getting census data

# Tutorial to work with census data at: https://www.youtube.com/watch?v=1WKAeM8yR-0&t=3s
# Request API key at: https://api.census.gov/data/key_signup.html

api_key <- 'XXX' ## Add your API key here
census_api_key(api_key, install = TRUE)

year <- 2023

# Defining sociodemographic variables to explore
variables <- c(
  "B01001_001",  # Total Population (POP)
  "B11001_001",  # Total Households (HH)
  "B17001_002",  # Income in the past 12 months below poverty level
  "B17001_001",  # Total population for who poverty is determined
  "B25001_001")  # Total Housing Units (for People per unit)

# Getting these variables' data for the entire U.S
acs.raw <- get_acs(
  geography = "county", 
  variables = variables, 
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

### Creating static map

ggplot() +
  geom_sf(data = acs.normal, fill = "gray95", color = "white") +
  geom_sf(data = st_centroid(acs.normal),
          aes(size = pop_total),
          color = "#1c9099", alpha = 0.7) +
  scale_size_continuous(range = c(0.01, 15)) + 
  theme_void()

### Mapping data - Interactive map

# A leaflet tutorial to get started: https://mikejohnson51.github.io/leaflet-intro/index.html

leaflet() %>% 
  setView(lng=-105.0848, lat=40.5729, zoom = 12) %>% 
  addProviderTiles(providers$CartoDB)
