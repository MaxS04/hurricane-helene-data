library(tidycensus)
library(here)
library(tidyverse)
library(sf)
library(units)
#library(tidyr)



# Getting census data ------------

# Tutorial to work with census data at: https://www.youtube.com/watch?v=1WKAeM8yR-0&t=3s
# Request API key at: https://api.census.gov/data/key_signup.html

api_key <- 'b3b4a0116be055b9ff20c0aa253e8aaaeffa2ebf' ## Add your API key here
census_api_key(api_key, install = TRUE, overwrite=TRUE)

year <- 2023

# Defining sociodemographic variable to explore: total population ------
variable <- "B01001_001"  # Total Population (POP)

# Getting total population data for the selected states ----
pop.raw <- get_acs(
  geography = "county", 
  variables = variable, 
  year = year, 
  state = c("NC","TN","VA","SC","GA"),
  geometry = TRUE,
  key = api_key,
  survey = "acs5", 
  output = "wide"
)

pop.total <- pop.raw %>% 
  mutate(
    geoid = GEOID,
    name = NAME,
    pop_total = B01001_001E,
    geometry = geometry,
    .keep = "none"
  )

pop.total <- pop.total %>% 
  separate(name, #separating information about county and state in two different columns
           sep = ",",
           into = c("county", "state"), 
           remove = FALSE) %>% 
  mutate(county = str_remove(county," County")) #removing the word "County" after their names

saveRDS(pop.total, here("Outputs", "pop_total.rds"))


# Calculate area in square meters, convert to sq miles, and estimate population density
pop.density <- pop.total %>%
  mutate(
    # st_area returns area in square meters
    area_sq_meters = st_area(geometry),
    # Convert to square miles
    area_sq_miles = set_units(area_sq_meters, "mi2"),
    # Calculate population density (people / sq. mile)
    pop_density = pop_total/as.numeric(area_sq_miles)
  )

saveRDS(pop.density, here("Outputs", "pop_density.rds"))


# Creating counties dataset
counties <- pop.total %>% 
  select(-pop_total)

saveRDS(counties, here("Outputs", "counties.rds"))


# Creating state boundary polygons
states <- pop.total %>% 
  group_by(state) %>% 
  summarise(geometry = st_union(geometry))

saveRDS(states, here("Outputs", "states.rds"))



