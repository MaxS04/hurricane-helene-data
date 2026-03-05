#load packages
library(tidyverse)
library(here)
library(skimr)
library(janitor)

#read data
hurricaneHWM <- readxl::read_excel(here("HurricaneData", "Hurricane_Helene_HWM_Database_Table.xlsx")) %>%
  clean_names()

hurricaneLS <- read_csv(here("HurricaneData", "HurricaneHelene_LS_Inventory.csv"))

HWMbyHeight <- hurricaneHWM %>% arrange(-hwm_elevation_ft) %>%
  select(hwm_type, northing, easting, county, state)

HWMbyHeight %>% count(state, sort = TRUE)
