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
  select(-`X_id`, -ORGANIZATION_ID, -ORGANIZATION_NAME, -PROGRAM_NAME, -LOCATION_NAME, -SHELTER_GROUP, -SHELTER_ID, -LOCATION_ID, -PROGRAM_ID)

raw_data_homeless$OCCUPANCY_DATE <- as.Date(raw_data_homeless$OCCUPANCY_DATE, format = "%Y/%m/%d")

# Remove rows with negative values in specified columns
raw_data_homeless <- raw_data_homeless %>%
  filter(
    (is.na(UNOCCUPIED_ROOMS) | UNOCCUPIED_ROOMS >= 0) &
      (is.na(UNAVAILABLE_ROOMS) | UNAVAILABLE_ROOMS >= 0) &
      (is.na(UNOCCUPIED_BEDS) | UNOCCUPIED_BEDS >= 0) &
      (is.na(UNAVAILABLE_BEDS) | UNAVAILABLE_BEDS >= 0)
  )

# Update occupancy rates in raw_data_homeless
raw_data_homeless <- raw_data_homeless %>%
  mutate(
    OCCUPANCY_RATE_BEDS = ifelse(CAPACITY_TYPE == "Bed Based Capacity", 
                                 round(OCCUPIED_BEDS / CAPACITY_ACTUAL_BED, 2), 
                                 OCCUPANCY_RATE_BEDS),
    OCCUPANCY_RATE_ROOMS = ifelse(CAPACITY_TYPE == "Room Based Capacity", 
                                  round(OCCUPIED_ROOMS / CAPACITY_ACTUAL_ROOM, 2), 
                                  OCCUPANCY_RATE_ROOMS)
  )

#### Save data ####
write_csv(
  x = raw_data_homeless,
  file = "data/cleaned_data/cleaned_data.csv"
)

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


#### Clean needed data for figure 2####

#Select cols needed
cleaned_data_figtwo <- raw_data_homeless %>%
  select(OCCUPANCY_DATE, PROGRAM_MODEL, OVERNIGHT_SERVICE_TYPE, PROGRAM_AREA)

# Remove rows with any NA values
cleaned_data_figtwo <- cleaned_data_figtwo %>%
  na.omit()

# Add the year col and remove the old one
cleaned_data_figtwo <- cleaned_data_figtwo %>%
  mutate(YEAR = format(OCCUPANCY_DATE, "%Y"))
cleaned_data_figtwo <- cleaned_data_figtwo %>%
  select(-OCCUPANCY_DATE)

# Add new columns with value 1
cleaned_data_figtwo <- cleaned_data_figtwo %>%
  mutate(
    PROGRAM_MODEL_NUM = 1,  # Column with value 1
    OVERNIGHT_TYPE_NUM = 1,  # Column with value 1
    PROGRAM_AREA_NUM = 1  # Column with value 1
  )

# Reorder the columns
cleaned_data_figtwo <- cleaned_data_figtwo %>%
  select(YEAR, PROGRAM_MODEL, PROGRAM_MODEL_NUM, OVERNIGHT_SERVICE_TYPE, 
         OVERNIGHT_TYPE_NUM, PROGRAM_AREA, PROGRAM_AREA_NUM)

# Summarize the count of PROGRAM_MODEL_NUM by YEAR and PROGRAM_MODEL
summary_program_model <- cleaned_data_figtwo %>%
  group_by(YEAR, PROGRAM_MODEL) %>%
  summarise(Total_PROGRAM_MODEL_NUM = sum(PROGRAM_MODEL_NUM))

# Summarize the count of OVERNIGHT_TYPE_NUM by YEAR and OVERNIGHT_SERVICE_TYPE
summary_overnight_type <- cleaned_data_figtwo %>%
  group_by(YEAR, OVERNIGHT_SERVICE_TYPE) %>%
  summarise(Total_OVERNIGHT_TYPE_NUM = sum(OVERNIGHT_TYPE_NUM))

# Summarize the count of PROGRAM_MODEL_NUM by YEAR and PROGRAM_MODEL
summary_program_area <- cleaned_data_figtwo %>%
  group_by(YEAR, PROGRAM_AREA) %>%
  summarise(Total_PROGRAM_AREA_NUM = sum(PROGRAM_AREA_NUM))

#### Save data ####
write_csv(
  x = summary_program_model,
  file = "data/cleaned_data/cleaned_data_figtwo_program_model.csv"
)

write_csv(
  x = summary_overnight_type,
  file = "data/cleaned_data/cleaned_data_figtwo_overnight_type.csv"
)

write_csv(
  x = summary_program_area,
  file = "data/cleaned_data/cleaned_data_figtwo_program_area.csv"
)

#### Clean needed data for figure 3 ####

#Select cols needed
cleaned_data_figthr <- raw_data_homeless %>%
  select(OCCUPANCY_DATE, SECTOR, SERVICE_USER_COUNT)

# Remove rows with any NA values
cleaned_data_figthr <- cleaned_data_figthr %>%
  na.omit()

# Summarize the count of PROGRAM_MODEL_NUM by YEAR and PROGRAM_MODEL
cleaned_data_figthr <- cleaned_data_figthr %>%
  group_by(OCCUPANCY_DATE, SECTOR) %>%
  summarise(Total_SECTOR_NUM = sum(SERVICE_USER_COUNT))

# Create a new column for 4-month intervals
summary_data_figthr <- cleaned_data_figthr %>%
  mutate(
    interval = case_when(
      OCCUPANCY_DATE >= as.Date("2021-01-01") & OCCUPANCY_DATE <= as.Date("2021-04-30") ~ "2021-01 to 2021-04",
      OCCUPANCY_DATE >= as.Date("2021-05-01") & OCCUPANCY_DATE <= as.Date("2021-08-31") ~ "2021-05 to 2021-08",
      OCCUPANCY_DATE >= as.Date("2021-09-01") & OCCUPANCY_DATE <= as.Date("2021-12-31") ~ "2021-09 to 2021-12",
      OCCUPANCY_DATE >= as.Date("2022-01-01") & OCCUPANCY_DATE <= as.Date("2022-04-30") ~ "2022-01 to 2022-04",
      OCCUPANCY_DATE >= as.Date("2022-05-01") & OCCUPANCY_DATE <= as.Date("2022-08-31") ~ "2022-05 to 2022-08",
      OCCUPANCY_DATE >= as.Date("2022-09-01") & OCCUPANCY_DATE <= as.Date("2022-12-31") ~ "2022-09 to 2022-12",
      OCCUPANCY_DATE >= as.Date("2023-01-01") & OCCUPANCY_DATE <= as.Date("2023-04-30") ~ "2023-01 to 2023-04",
      OCCUPANCY_DATE >= as.Date("2023-05-01") & OCCUPANCY_DATE <= as.Date("2023-08-31") ~ "2023-05 to 2023-08",
      OCCUPANCY_DATE >= as.Date("2023-09-01") & OCCUPANCY_DATE <= as.Date("2023-12-31") ~ "2023-09 to 2023-12",
      OCCUPANCY_DATE >= as.Date("2024-01-01") & OCCUPANCY_DATE <= as.Date("2024-04-30") ~ "2024-01 to 2024-04",
      OCCUPANCY_DATE >= as.Date("2024-05-01") & OCCUPANCY_DATE <= as.Date("2024-08-28") ~ "2024-05 to 2024-08"
    )
  )

# Summarize the total of Total_SECTOR_NUM by SECTOR and 4-month interval
summary_data_figthr_new <- summary_data_figthr %>%
  group_by(SECTOR, interval) %>%
  summarise(Total_SECTOR_SUM = sum(Total_SECTOR_NUM, na.rm = TRUE))

# Remove rows with NA values
summary_data_figthr_new <- summary_data_figthr_new %>%
  drop_na()

#### Save data ####
write_csv(
  x = summary_data_figthr_new,
  file = "data/cleaned_data/cleaned_data_figthr.csv"
)

#### Clean needed data for figure 4 ####

#Select cols needed
cleaned_data_figfou <- raw_data_homeless %>%
  select(OCCUPANCY_DATE, CAPACITY_TYPE, CAPACITY_FUNDING_BED, CAPACITY_ACTUAL_BED, OCCUPIED_BEDS, CAPACITY_FUNDING_ROOM, CAPACITY_ACTUAL_ROOM, OCCUPIED_ROOMS)

# Split into room and bed datasets
room_data <- cleaned_data_figfou %>%
  filter(CAPACITY_TYPE == "Room Based Capacity") %>%
  select(-CAPACITY_FUNDING_BED, -OCCUPIED_BEDS, -CAPACITY_FUNDING_BED)  # Remove bed-related columns

bed_data <- cleaned_data_figfou %>%
  filter(CAPACITY_TYPE == "Bed Based Capacity") %>%
  select(-CAPACITY_FUNDING_ROOM, -OCCUPIED_ROOMS, -CAPACITY_FUNDING_ROOM)  # Remove room-related columns

# Create a new column for 4-month intervals
summary_room_data <- room_data %>%
  mutate(
    interval = case_when(
      OCCUPANCY_DATE >= as.Date("2021-01-01") & OCCUPANCY_DATE <= as.Date("2021-04-30") ~ "2021-01 to 2021-04",
      OCCUPANCY_DATE >= as.Date("2021-05-01") & OCCUPANCY_DATE <= as.Date("2021-08-31") ~ "2021-05 to 2021-08",
      OCCUPANCY_DATE >= as.Date("2021-09-01") & OCCUPANCY_DATE <= as.Date("2021-12-31") ~ "2021-09 to 2021-12",
      OCCUPANCY_DATE >= as.Date("2022-01-01") & OCCUPANCY_DATE <= as.Date("2022-04-30") ~ "2022-01 to 2022-04",
      OCCUPANCY_DATE >= as.Date("2022-05-01") & OCCUPANCY_DATE <= as.Date("2022-08-31") ~ "2022-05 to 2022-08",
      OCCUPANCY_DATE >= as.Date("2022-09-01") & OCCUPANCY_DATE <= as.Date("2022-12-31") ~ "2022-09 to 2022-12",
      OCCUPANCY_DATE >= as.Date("2023-01-01") & OCCUPANCY_DATE <= as.Date("2023-04-30") ~ "2023-01 to 2023-04",
      OCCUPANCY_DATE >= as.Date("2023-05-01") & OCCUPANCY_DATE <= as.Date("2023-08-31") ~ "2023-05 to 2023-08",
      OCCUPANCY_DATE >= as.Date("2023-09-01") & OCCUPANCY_DATE <= as.Date("2023-12-31") ~ "2023-09 to 2023-12",
      OCCUPANCY_DATE >= as.Date("2024-01-01") & OCCUPANCY_DATE <= as.Date("2024-04-30") ~ "2024-01 to 2024-04",
      OCCUPANCY_DATE >= as.Date("2024-05-01") & OCCUPANCY_DATE <= as.Date("2024-08-28") ~ "2024-05 to 2024-08"
    )
  )

# Summarize the total of Total_SECTOR_NUM by SECTOR and 4-month interval
summary_room_data_new <- summary_room_data %>%
  group_by(interval) %>%
  summarise(Total_FUNDING_ROOM = sum(CAPACITY_FUNDING_ROOM, na.rm = TRUE),
            Total_OCCUPIED_ROOM = sum(OCCUPIED_ROOMS, na.rm = TRUE),
            Total_ACTUAL_ROOM = sum(CAPACITY_ACTUAL_ROOM, na.rm = TRUE))

# Remove rows with NA values
summary_room_data_new <- summary_room_data_new %>%
  drop_na()


# Create a new column for 4-month intervals
summary_bed_data <- bed_data %>%
  mutate(
    interval = case_when(
      OCCUPANCY_DATE >= as.Date("2021-01-01") & OCCUPANCY_DATE <= as.Date("2021-04-30") ~ "2021-01 to 2021-04",
      OCCUPANCY_DATE >= as.Date("2021-05-01") & OCCUPANCY_DATE <= as.Date("2021-08-31") ~ "2021-05 to 2021-08",
      OCCUPANCY_DATE >= as.Date("2021-09-01") & OCCUPANCY_DATE <= as.Date("2021-12-31") ~ "2021-09 to 2021-12",
      OCCUPANCY_DATE >= as.Date("2022-01-01") & OCCUPANCY_DATE <= as.Date("2022-04-30") ~ "2022-01 to 2022-04",
      OCCUPANCY_DATE >= as.Date("2022-05-01") & OCCUPANCY_DATE <= as.Date("2022-08-31") ~ "2022-05 to 2022-08",
      OCCUPANCY_DATE >= as.Date("2022-09-01") & OCCUPANCY_DATE <= as.Date("2022-12-31") ~ "2022-09 to 2022-12",
      OCCUPANCY_DATE >= as.Date("2023-01-01") & OCCUPANCY_DATE <= as.Date("2023-04-30") ~ "2023-01 to 2023-04",
      OCCUPANCY_DATE >= as.Date("2023-05-01") & OCCUPANCY_DATE <= as.Date("2023-08-31") ~ "2023-05 to 2023-08",
      OCCUPANCY_DATE >= as.Date("2023-09-01") & OCCUPANCY_DATE <= as.Date("2023-12-31") ~ "2023-09 to 2023-12",
      OCCUPANCY_DATE >= as.Date("2024-01-01") & OCCUPANCY_DATE <= as.Date("2024-04-30") ~ "2024-01 to 2024-04",
      OCCUPANCY_DATE >= as.Date("2024-05-01") & OCCUPANCY_DATE <= as.Date("2024-08-28") ~ "2024-05 to 2024-08"
    )
  )

# Summarize the total of Total_SECTOR_NUM by SECTOR and 4-month interval
summary_bed_data_new <- summary_bed_data %>%
  group_by(interval) %>%
  summarise(Total_FUNDING_BED = sum(CAPACITY_FUNDING_BED, na.rm = TRUE),
            Total_OCCUPIED_BED = sum(OCCUPIED_BEDS, na.rm = TRUE),
            Total_ACTUAL_BED = sum(CAPACITY_ACTUAL_BED, na.rm = TRUE))

# Remove rows with NA values
summary_bed_data_new <- summary_bed_data_new %>%
  drop_na()

# Merging using dplyr
merged_data <- full_join(summary_bed_data_new, summary_room_data_new, by = "interval")


#### Save data ####
write_csv(
  x = merged_data,
  file = "data/cleaned_data/cleaned_data_figfou.csv"
)




