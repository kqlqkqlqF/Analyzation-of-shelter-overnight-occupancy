---
#### Preamble ####
# Purpose: Download and save the shelter flow data from Toronto Opendata
# Author: Yiyi Feng
# Date: 16th April 2024
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
package <- show_package("ac77f532-f18b-427c-905c-4ae87ce69c93")
head(package)

# get all resources for this package
resources <- list_package_resources("ac77f532-f18b-427c-905c-4ae87ce69c93")
head(resources)#show the first six lines of package for checking

# load the first datastore resource as a sample
raw_data_homeless <- filter(resources, row_number()==1) %>% get_resource()
raw_data_homeless#show the whole raw data for checking


#### Save data ####
write_csv(
  x = raw_data_homeless,
  file = "data/raw_data/raw_data_homeless.csv"
)

