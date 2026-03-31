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
  select(hwm_type, hwm_elevation_ft, northing, easting, county, state)

HWMbyCounty <- hurricaneHWM %>% arrange(-hwm_elevation_ft) %>%
  select(hwm_type, hwm_elevation_ft, northing, easting, county, state)

HWMbyHeight %>% count(state, sort = TRUE)
HWMbyCounty %>% count(county, sort = TRUE)


#plotting data 
library(ggplot2)

ggplot(HWMbyCounty, aes(x = county, y = hwm_elevation_ft)) +
  geom_point() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
