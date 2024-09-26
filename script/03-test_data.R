#### Preamble ####
# Purpose: Test the cleaned dataset and the simulated dataset to ensure the data were cleaned and there's no unexpected errors
# Author: Yiyi Feng
# Date: 26th September 2024
# Contact: yiyi.feng@mail.utoronto.ca
# License: MIT
#Other information: Need to install packages "testthat", "lubridate", "tidyverse", "car", and "stringr".

#### Workspace setup ####
library(tidyverse)
library(testthat)
library(lubridate)
library(stringr)
library(car)

#load files
cleaned_data <-
  read_csv(
    file = "data/cleaned_data/cleaned_data.csv",
    show_col_types = FALSE)

simulated_data <-
  read_csv(
    file = "data/simulated_data/simulated_data.csv",
    show_col_types = FALSE)

#### Generate the first test ####

# Generate the complete sequence of dates
expected_dates <- seq(ymd("2021-01-01"), ymd("2024-09-24"), by = "day")

# Check for cleaned_data
if (all(expected_dates %in% cleaned_data$OCCUPANCY_DATE)) {
  print("All dates are present in the OCCUPANCY_DATE column of cleaned_data.")
} else {
  missing_dates_cleaned <- setdiff(expected_dates, cleaned_data$OCCUPANCY_DATE)
  print("The following dates are missing in cleaned_data:")
  print(missing_dates_cleaned)
}

# Check for simulated_data
if (all(expected_dates %in% simulated_data$OCCUPANCY_DATE)) {
  print("All dates are present in the OCCUPANCY_DATE column of simulated_data.")
} else {
  missing_dates_simulated <- setdiff(expected_dates, simulated_data$OCCUPANCY_DATE)
  print("The following dates are missing in simulated_data:")
  print(missing_dates_simulated)
}

#### Generate the 2nd test ####

# Check for negative values and NA in cleaned_data
if (any(is.na(cleaned_data$SERVICE_USER_COUNT)) || any(cleaned_data$SERVICE_USER_COUNT < 0)) {
  print("cleaned_data has negative values or NA in SERVICE_USER_COUNT.")
} else {
  print("cleaned_data has no negative values or NA in SERVICE_USER_COUNT.")
}

# Check for negative values and NA in simulated_data
if (any(is.na(simulated_data$SERVICE_USER_COUNT)) || any(simulated_data$SERVICE_USER_COUNT < 0)) {
  print("simulated_data has negative values or NA in SERVICE_USER_COUNT.")
} else {
  print("simulated_data has no negative values or NA in SERVICE_USER_COUNT.")
}

#### Generate the 3rd test ####

# Function to test conditions for a given dataset
check_capacity_conditions <- function(data) {
  # Check for bed-based capacity
  bed_based_issues <- data[data$CAPACITY_TYPE == "bed-based capacity", ]
  bed_based_check <- any(is.na(bed_based_issues$CAPACITY_ACTUAL_ROOM)) ||
    any(is.na(bed_based_issues$CAPACITY_FUNDING_ROOM)) ||
    any(is.na(bed_based_issues$OCCUPIED_ROOMS)) ||
    any(is.na(bed_based_issues$UNOCCUPIED_ROOMS)) ||
    any(is.na(bed_based_issues$UNAVAILABLE_ROOMS)) ||
    any(is.na(bed_based_issues$OCCUPANCY_RATE_ROOMS)) ||
    any(bed_based_issues$CAPACITY_ACTUAL_BED <= 0) ||
    any(bed_based_issues$CAPACITY_FUNDING_BED <= 0) ||
    any(bed_based_issues$OCCUPIED_BEDS <= 0) ||
    any(bed_based_issues$UNOCCUPIED_BEDS <= 0) ||
    any(bed_based_issues$UNAVAILABLE_BEDS <= 0) ||
    any(bed_based_issues$OCCUPANCY_RATE_BEDS <= 0)
  
  if (bed_based_check) {
    print("Issues detected for bed-based capacity in the cleaned_data.")
  } else {
    print("No issues for bed-based capacity in the cleaned_data.")
  }
  
  # Check for room-based capacity
  room_based_issues <- data[data$CAPACITY_TYPE == "room-based capacity", ]
  room_based_check <- any(!is.na(room_based_issues$CAPACITY_ACTUAL_ROOM)) ||
    any(!is.na(room_based_issues$CAPACITY_FUNDING_ROOM)) ||
    any(!is.na(room_based_issues$OCCUPIED_ROOMS)) ||
    any(!is.na(room_based_issues$UNOCCUPIED_ROOMS)) ||
    any(!is.na(room_based_issues$UNAVAILABLE_ROOMS)) ||
    any(!is.na(room_based_issues$OCCUPANCY_RATE_ROOMS)) ||
    any(room_based_issues$CAPACITY_ACTUAL_ROOM > 0) ||
    any(room_based_issues$CAPACITY_FUNDING_ROOM > 0) ||
    any(room_based_issues$OCCUPIED_ROOMS > 0) ||
    any(room_based_issues$UNOCCUPIED_ROOMS > 0) ||
    any(room_based_issues$UNAVAILABLE_ROOMS > 0) ||
    any(room_based_issues$OCCUPANCY_RATE_ROOMS > 0)
  
  if (room_based_check) {
    print("Issues detected for room-based capacity in the cleaned_data.")
  } else {
    print("No issues for room-based capacity in the cleaned_data.")
  }
}

# Run the checks 
check_capacity_conditions(cleaned_data)
check_capacity_conditions(simulated_data)

#### Generate the 4th test ####

# Function to check capacity funding conditions
check_capacity_funding_conditions <- function(data) {
  # Check for Bed Based Capacity
  bed_based <- data[data$CAPACITY_TYPE == "Bed Based Capacity", ]
  
  if (nrow(bed_based) > 0) {
    if (all(bed_based$CAPACITY_FUNDING_BED >= bed_based$CAPACITY_ACTUAL_BED, na.rm = TRUE) &&
        all(bed_based$CAPACITY_ACTUAL_BED >= bed_based$OCCUPIED_BEDS, na.rm = TRUE)) {
      print("All conditions are met for Bed Based Capacity.")
    } else {
      print("Conditions are not satisfied for Bed Based Capacity.")
    }
  } else {
    print("No Bed Based Capacity records found.")
  }
  
  # Check for Room Based Capacity
  room_based <- data[data$CAPACITY_TYPE == "Room Based Capacity", ]
  
  if (nrow(room_based) > 0) {
    if (all(room_based$CAPACITY_FUNDING_ROOM >= room_based$CAPACITY_ACTUAL_ROOM, na.rm = TRUE) &&
        all(room_based$CAPACITY_ACTUAL_ROOM >= room_based$OCCUPIED_ROOMS, na.rm = TRUE)) {
      print("All conditions are met for Room Based Capacity.")
    } else {
      print("Conditions are not satisfied for Room Based Capacity.")
    }
  } else {
    print("No Room Based Capacity records found.")
  }
}

# Run the checks 
check_capacity_funding_conditions(cleaned_data)
check_capacity_funding_conditions(simulated_data)


#### Generate the 5th test ####

# Function to check non-negativity conditions for capacity
check_non_negativity_conditions <- function(data) {
  # Check for Bed Based Capacity
  bed_based <- data[data$CAPACITY_TYPE == "Bed Based Capacity", ]
  
  if (nrow(bed_based) > 0) {
    if (all(bed_based$UNOCCUPIED_BEDS >= 0, na.rm = TRUE) &&
        all(bed_based$UNAVAILABLE_BEDS >= 0, na.rm = TRUE)) {
      print("All conditions are met for Bed Based Capacity: UNOCCUPIED_BEDS and UNAVAILABLE_BEDS are non-negative.")
    } else {
      print("Conditions are not satisfied for Bed Based Capacity: Non-negativity requirement violated.")
    }
  } else {
    print("No Bed Based Capacity records found.")
  }
  
  # Check for Room Based Capacity
  room_based <- data[data$CAPACITY_TYPE == "Room Based Capacity", ]
  
  if (nrow(room_based) > 0) {
    if (all(room_based$UNOCCUPIED_ROOMS >= 0, na.rm = TRUE) &&
        all(room_based$UNAVAILABLE_ROOMS >= 0, na.rm = TRUE)) {
      print("All conditions are met for Room Based Capacity: UNOCCUPIED_ROOMS and UNAVAILABLE_ROOMS are non-negative.")
    } else {
      print("Conditions are not satisfied for Room Based Capacity: Non-negativity requirement violated.")
    }
  } else {
    print("No Room Based Capacity records found.")
  }
}

# Run the checks
check_non_negativity_conditions(cleaned_data)
check_non_negativity_conditions(simulated_data)


#### Generate the 6th test ####

# Function to check occupancy rates
check_occupancy_rates <- function(data) {
  # Check for Bed Based Capacity
  bed_based <- data[data$CAPACITY_TYPE == "Bed Based Capacity", ]
  
  if (nrow(bed_based) > 0) {
    bed_based$calculated_occupancy_rate <- round(bed_based$OCCUPIED_BEDS / bed_based$CAPACITY_ACTUAL_BED, 2)
    bed_based$OCCUPANCY_RATE_BEDS <- round(bed_based$OCCUPANCY_RATE_BEDS, 2)  # Round the existing rate
    
    bed_based_check <- all(bed_based$OCCUPANCY_RATE_BEDS == bed_based$calculated_occupancy_rate, na.rm = TRUE)
    
    if (bed_based_check) {
      print("All occupancy rates are correct for Bed Based Capacity.")
    } else {
      print("Occupancy rates are incorrect for Bed Based Capacity.")
    }
  } else {
    print("No Bed Based Capacity records found.")
  }
  
  # Check for Room Based Capacity
  room_based <- data[data$CAPACITY_TYPE == "Room Based Capacity", ]
  
  if (nrow(room_based) > 0) {
    room_based$calculated_occupancy_rate <- round(room_based$OCCUPIED_ROOMS / room_based$CAPACITY_ACTUAL_ROOM, 2)
    room_based$OCCUPANCY_RATE_ROOMS <- round(room_based$OCCUPANCY_RATE_ROOMS, 2)  # Round the existing rate
    
    room_based_check <- all(room_based$OCCUPANCY_RATE_ROOMS == room_based$calculated_occupancy_rate, na.rm = TRUE)
    
    if (room_based_check) {
      print("All occupancy rates are correct for Room Based Capacity.")
    } else {
      print("Occupancy rates are incorrect for Room Based Capacity.")
    }
  } else {
    print("No Room Based Capacity records found.")
  }
}


# Run the checks 
check_occupancy_rates(cleaned_data)
check_occupancy_rates(simulated_data)


