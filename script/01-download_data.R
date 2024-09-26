---
#### Preamble ####
# Purpose: Download and save the shelter data from Toronto Opendata
# Author: Yiyi Feng
# Date: 25th September 2024
# Contact: yiyi.feng@mail.utoronto.ca
# License: MIT
# Pre-requisites: none. 
#Other information: Need to install packages "opendatatoronto", "readr", and "dplyr"
---

#### Workspace setup ####
library(opendatatoronto)
library(dplyr)
library(readr)

#### Download data ####

# get package for shelter system from Toronto Opendata
package <- show_package("21c83b32-d5a8-4106-a54f-010dbe49f6f2")
head(package)

# get all resources for this package
resources <- resources <- list_package_resources("21c83b32-d5a8-4106-a54f-010dbe49f6f2")
resources#show the first six lines of package for checking

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# Load both datasets (2021, 2022, 2023, and 2024)
data_2021 <- filter(datastore_resources, row_number() == 7) %>% get_resource()
data_2022 <- filter(datastore_resources, row_number() == 6) %>% get_resource()
data_2023 <- filter(datastore_resources, row_number() == 8) %>% get_resource()
data_2024 <- filter(datastore_resources, row_number() == 5) %>% get_resource()

# Remove the "T00:00:00" part from the OCCUPANCY_DATE column in data_2023 using base R
data_2023$OCCUPANCY_DATE <- sub("T.*", "", data_2023$OCCUPANCY_DATE)

# Combine the datasets
combined_data <- bind_rows(data_2021, data_2022, data_2023, data_2024)
combined_data




#### Save data ####
write_csv(
  x = combined_data,
  file = "data/raw_data/raw_data_homeless.csv"
)

