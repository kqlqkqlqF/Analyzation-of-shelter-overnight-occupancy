---
#### Preamble ####
# Purpose: Clean the data download from OpendataToronto to make the 
#          datas into a uniform and cleaned style
# Author: Yiyi Feng
# Date: 25th September 2024
# Contact: yiyi.feng@mail.utoronto.ca
# License: MIT
# Pre-requisites: "data/raw_data.csv" file, which is the raw data. 
#Other information: Need to install packages "knitr", "dplyr", "janitor", "tidyverse", and "lubridate".
---

#### Workspace setup ####
library(knitr)
library(janitor)
library(lubridate)
library(tidyverse)
library(dplyr)
library(ggmap)
library(tidygeocoder)
library(purrr)
#### Basic cleaning ####
#import raw data
raw_data_homeless <-
  read_csv(
    file = "data/raw_data/raw_data_homeless.csv",
    show_col_types = FALSE
  )

#### Clean needed homeless data ####

#remove useless cols from the dataset
raw_data_homeless <- raw_data_homeless %>%
  select(-`_id`, -ORGANIZATION_ID, -SHELTER_ID, -LOCATION_ID, -PROGRAM_ID)

# Convert OCCUPANCY_DATE based on the year with adjusted format
raw_data_homeless <- raw_data_homeless %>%
  mutate(OCCUPANCY_DATE = case_when(
    grepl("^21|^22", OCCUPANCY_DATE) ~ as.Date(OCCUPANCY_DATE, format = "%y-%m-%d"),  # Assuming this is correct for your dataset
    grepl("^2023|^2024", OCCUPANCY_DATE) ~ as.Date(OCCUPANCY_DATE),  # Assuming these dates are already in proper format
    TRUE ~ as.Date(NA)  # Handle any other cases
  ))

#### Clean needed data for figure 1 ####

#Select cols needed
cleaned_data_figone <- raw_data_homeless %>%
  select(OCCUPANCY_DATE, LOCATION_ADDRESS, LOCATION_PROVINCE, LOCATION_POSTAL_CODE, LOCATION_CITY, SERVICE_USER_COUNT)

# Remove rows with any NA values
cleaned_data_figone <- cleaned_data_figone %>%
  na.omit()

# Add the year col and remove the old one
cleaned_data_figone <- cleaned_data_figone %>%
  mutate(YEAR = format(OCCUPANCY_DATE, "%Y"))
cleaned_data_figone <- cleaned_data_figone %>%
  select(-OCCUPANCY_DATE)

# Re-format the address information
summarized_data_figone <- cleaned_data_figone %>%
  mutate(LOCATION_ADDRESS = paste(LOCATION_ADDRESS,
                                  LOCATION_CITY,
                                  LOCATION_PROVINCE,
                                  LOCATION_POSTAL_CODE,
                                  sep = ", ")) %>%
  select(LOCATION_ADDRESS, SERVICE_USER_COUNT, YEAR)  # Select the columns you want to keep

# sum up the user counts grouped by address and year
summarized_data_figone <- summarized_data_figone %>%
  group_by(LOCATION_ADDRESS, YEAR) %>%
  summarise(Total_Service_User_Count = sum(SERVICE_USER_COUNT, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(YEAR, LOCATION_ADDRESS)  # Arrange by YEAR first, then LOCATION_ADDRESS




# Register Google API key 
register_google(key = "AIzaSyCTiSeL3m1Wr7rjElHoKYoFlxQdexwKxQg")


geocoded_data <- data.frame()  # Initialize an empty data frame

for (i in 1:nrow(summarized_data_figone)) {
  address <- summarized_data_figone$LOCATION_ADDRESS[i]
  year <- summarized_data_figone$YEAR[i]  # Get the year for this entry
  
  geocode_result <- tryCatch({
    ggmap::geocode(address, output = "latlon", source = "google")
  }, error = function(e) {
    cat("Error geocoding:", address, "\n", e$message, "\n")
    return(data.frame(lat = NA, lon = NA))
  })
  
  # Combine address, year, latitude, and longitude
  geocoded_data <- rbind(geocoded_data, 
                         data.frame(LOCATION_ADDRESS = address,
                                    Latitude = geocode_result$lat,
                                    Longitude = geocode_result$lon,
                                    YEAR = year,  # Include the year column
                                    stringsAsFactors = FALSE))
}

#### Save data ####
write_csv(
  x = geocoded_data,
  file = "data/cleaned_data/cleaned_data_figone.csv"
)



