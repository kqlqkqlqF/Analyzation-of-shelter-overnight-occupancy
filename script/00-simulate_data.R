---
#### Preamble ####
# Purpose: Simulate and generate a dataset for mimicing the actual dataset
# Author: Yiyi Feng
# Date: 25th September 2024
# Contact: yiyi.feng@mail.utoronto.ca
# Pre-requisites: no pre-requisites 
# License: MIT
#Other information: Need to install packages"tidyverse", and "lubridate".
---

#### Workspace setup ####
# Load necessary libraries for data manipulation and date handling
library(tidyverse)
library(lubridate)

# Set a seed to ensure reproducibility when simulating data
set.seed(7)

# Simulate dates for the year 2018, with monthly intervals
dates <- seq(ymd("2021-01-01"), ymd("2024-09-24"), by = "day")

# Create simulated data
# Create a data frame with fixed values
data_group <- data.frame(
  LOCATION_ADDRESS = c("445 Rexdale Blvd", "14 Roncesvalles Ave", "850 Bloor St W", "1651 Sheppard Ave W", "3600 Steeles Ave W"),  # Fixed addresses
  LOCATION_POSTAL_CODE = c("M9W 6P8", "M6R 2K3", "M6G 1M2", "M3M 2X4", "L4L 8P5"),  # Fixed postal codes
  LOCATION_CITY = c("Etobicoke", "Toronto", "Toronto", "Toronto", "Vaughan"),  # Fixed cities
  LOCATION_PROVINCE = c("ON", "ON", "ON", "ON", "ON"),  # Fixed provinces
  PROGRAM_MODEL = c("Emergency", "Emergency", "Emergency", "Transitional", "Emergency"),  # Fixed program models
  OVERNIGHT_SERVICE_TYPE = c("Motel/Hotel Shelter", "Motel/Hotel Shelter", "Shelter", "Shelter", "Motel/Hotel Shelter"),  # Fixed service types
  PROGRAM_AREA = c("COVID-19 Response", "COVID-19 Response", "Base Shelter and Overnight Services System", "Base Shelter and Overnight Services System", "COVID-19 Response"),  # Fixed program areas
  CAPACITY_TYPE = c("Room Based Capacity", "Room Based Capacity", "Bed Based Capacity", "Bed Based Capacity", "Room Based Capacity")  # Fixed capacity types
)

# Repeat data_group to cover 1500 rows
data_group_repeated <- data_group[rep(1:nrow(data_group), length.out = 1500), ]

# Randomize the order of the repeated rows
data_group_repeated <- data_group_repeated[sample(1:nrow(data_group_repeated)), ]

# Repeat the dates enough to cover at least 1500 rows
repeat_times <- ceiling(1500 / length(dates))  # Calculate how many times to repeat
occupancy_dates <- rep(dates, times = repeat_times)  # Repeat dates

# Trim to exactly 1500 rows and sort the dates in chronological order
occupancy_dates <- occupancy_dates[1:1500]

# Create the dataset
new_dataset <- data.frame(OCCUPANCY_DATE = occupancy_dates)

# Shuffle the repeated rows randomly
data_group_repeated <- data_group_repeated[sample(1:nrow(data_group_repeated)), ]

# Combine with the new_dataset
sum_dataset <- cbind(new_dataset, data_group_repeated)

# Define the possible sectors
sectors <- c("Families", "Mixed Adult", "Men", "Women", "Youth")

# Add SECTOR column with random selection from sectors
sum_dataset$SECTOR <- sample(sectors, nrow(sum_dataset), replace = TRUE)

# Generate SERVICE_USER_COUNT with a skew towards numbers under 100
# Using a custom distribution where values under 100 are more frequent
service_user_count <- c(sample(0:99, size = 1200, replace = TRUE), 
                        sample(100:300, size = 300, replace = TRUE))  # Skewed distribution

# Shuffle the SERVICE_USER_COUNT to randomize the order
service_user_count <- sample(service_user_count)

# Add SERVICE_USER_COUNT column to sum_dataset
sum_dataset$SERVICE_USER_COUNT <- service_user_count[1:nrow(sum_dataset)]


# Initialize the new columns with NA
sum_dataset$CAPACITY_ACTUAL_BED <- NA
sum_dataset$CAPACITY_FUNDING_BED <- NA
sum_dataset$OCCUPIED_BEDS <- NA
sum_dataset$CAPACITY_ACTUAL_ROOM <- NA
sum_dataset$CAPACITY_FUNDING_ROOM <- NA
sum_dataset$OCCUPIED_ROOMS <- NA

# Loop through each row and assign values based on CAPACITY_TYPE
for (i in 1:nrow(sum_dataset)) {
  if (sum_dataset$CAPACITY_TYPE[i] == "Room Based Capacity") {
    # Generate values for room-based columns
    occupied_rooms <- sample(0:48, 1)
    capacity_actual_room <- sample(occupied_rooms:49, 1)  # Actual must be >= occupied
    capacity_funding_room <- sample(capacity_actual_room:50, 1)  # Funding must be >= actual
    
    sum_dataset$CAPACITY_ACTUAL_ROOM[i] <- capacity_actual_room
    sum_dataset$CAPACITY_FUNDING_ROOM[i] <- capacity_funding_room
    sum_dataset$OCCUPIED_ROOMS[i] <- occupied_rooms
    
  } else if (sum_dataset$CAPACITY_TYPE[i] == "Bed Based Capacity") {
    # Generate values for bed-based columns
    occupied_beds <- sample(0:48, 1)
    capacity_actual_bed <- sample(occupied_beds:49, 1)  # Actual must be >= occupied
    capacity_funding_bed <- sample(capacity_actual_bed:50, 1)  # Funding must be >= actual
    
    sum_dataset$CAPACITY_ACTUAL_BED[i] <- capacity_actual_bed
    sum_dataset$CAPACITY_FUNDING_BED[i] <- capacity_funding_bed
    sum_dataset$OCCUPIED_BEDS[i] <- occupied_beds
  }
}

# Calculate UNOCCUPIED_BEDS and UNAVAILABLE_BEDS for Bed Based Capacity
sum_dataset$UNOCCUPIED_BEDS <- sum_dataset$CAPACITY_ACTUAL_BED - sum_dataset$OCCUPIED_BEDS
sum_dataset$UNAVAILABLE_BEDS <- sum_dataset$CAPACITY_FUNDING_BED - sum_dataset$CAPACITY_ACTUAL_BED

# Calculate UNOCCUPIED_ROOMS and UNAVAILABLE_ROOMS for Room Based Capacity
sum_dataset$UNOCCUPIED_ROOMS <- sum_dataset$CAPACITY_ACTUAL_ROOM - sum_dataset$OCCUPIED_ROOMS
sum_dataset$UNAVAILABLE_ROOMS <- sum_dataset$CAPACITY_FUNDING_ROOM - sum_dataset$CAPACITY_ACTUAL_ROOM

# Calculate OCCUPANCY_RATE_BEDS and handle division by zero
sum_dataset$OCCUPANCY_RATE_BEDS <- ifelse(
  sum_dataset$CAPACITY_ACTUAL_BED > 0, 
  sum_dataset$OCCUPIED_BEDS / sum_dataset$CAPACITY_ACTUAL_BED, 
  NA
)

# Calculate OCCUPANCY_RATE_ROOMS and handle division by zero
sum_dataset$OCCUPANCY_RATE_ROOMS <- ifelse(
  sum_dataset$CAPACITY_ACTUAL_ROOM > 0, 
  sum_dataset$OCCUPIED_ROOMS / sum_dataset$CAPACITY_ACTUAL_ROOM, 
  NA
)

# Write simulated data to a CSV file for further analysis
write.csv(sum_dataset, "data/simulated_data/simulated_data.csv", row.names = TRUE)

