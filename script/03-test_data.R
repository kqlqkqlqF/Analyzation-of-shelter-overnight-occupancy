#### Preamble ####
# Purpose: Test the cleaned dataset and the simulated dataset to ensure the data were cleaned and there's no unexpected errors
# Author: Yiyi Feng
# Date: 16th April 2024
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
cleaned_covid_data <-
  read_csv(
    file = "data/cleaned_data/cleaned_data_covid.csv",
    show_col_types = FALSE)

simulated_covid_data <-
  read_csv(
    file = "data/simulated_data/simulated_covid_data.csv",
    show_col_types = FALSE)


#### Test Homeless DATA for both Generated data and Cleaned data ####

# Define the file paths
file_paths <- c("data/cleaned_data/cleaned_data_homeless.csv", 
                "data/simulated_data/simulated_homeless_data.csv")

# Function to perform tests on a single file
perform_tests <- function(file_path) {
  # Read the CSV file
  data <- read_csv(file_path, col_types = cols())
  
  # Check if the N/A data has been completely cleared from the table
  na_free <- all(!is.na(data))
  
  # Check if the first line is all strings
  first_row_is_string <- all(sapply(names(data), is.character))
  
  # Check if the second column is all dates and the dates are all on the 1st
  dates_valid_and_first_of_month <- all(!is.na(ymd(data[[2]][-1]))
                                        & day(ymd(data[[2]][-1])) == 1)
  
  # Check whether the third column belongs to one of the specified strings
  population_group_valid <- all(data[[3]][-1] %in% c("All Population", "Chronic", "Refugees", "Families", "Youth", "Single Adult", "Non-refugees", "Indigenous"))
  
  # Check whether the fourth to seventeenth columns are all integers greater 
  # than or equal to 0
  cols_4_to_17_are_integers <-
    all(sapply(data[4:17], 
               function(col) all(col[-1] >= 0 
                                 & col[-1] == floor(col[-1]))))
  
  # Check whether the eighteenth column is all percentages between 0 and 100
  percentages_valid <- all(sapply(data[[18]][-1], function(x) {
    percentage <- as.numeric(sub("%", "", x))
    return(!is.na(percentage) && percentage >= 0 && percentage <= 100)
  }))
  
  # Check for All Population rows to have 100% population_group_percentage
  data$population_group_percentage_numeric <-
    as.numeric(str_remove(data$population_group_percentage, "%"))
  all_population_percentage_check <- 
    all(data[data$population_group == "All Population", ]
        $population_group_percentage_numeric == 100)
  
  # Return result
  list(
    file_path = file_path,
    na_free = na_free,
    first_row_is_string = first_row_is_string,
    ddates_valid_and_first_of_month = dates_valid_and_first_of_month,
    population_group_valid = population_group_valid,
    cols_4_to_17_are_integers = cols_4_to_17_are_integers,
    percentages_valid = percentages_valid,
    all_population_percentage_check = all_population_percentage_check
  )
}

# Perform tests for each file one by one
results <- lapply(file_paths, perform_tests)

# Print results
for (i in seq_along(results)) {
  cat("File:", results[[i]]$file_path, "\n")
  cat("NA Free:", results[[i]]$na_free, "\n")
  cat("First Row Is String:", results[[i]]$first_row_is_string, "\n")
  cat("Dates Valid And First Of Month:", results[[i]]$ddates_valid_and_first_of_month, "\n")
  cat("Population Group Valid:", results[[i]]$population_group_valid, "\n")
  cat("Cols 4 To 17 Are Integers:", results[[i]]$cols_4_to_17_are_integers, "\n")
  cat("Percentages Valid:", results[[i]]$percentages_valid, "\n")
  cat("All Population Percentage Check:", results[[i]]$all_population_percentage_check, "\n\n")
}



#### Test for cleaned and simulated COVID data####
# Define the perform_covid_tests function
perform_covid_tests <- function(data) {
  # Perform tests on the COVID data
  year_numeric <- all(is.numeric(data$Year))
  data_before_2024_na <- all(!is.na(data[1:4, 2:13]))
  
  # Check Numeric Data
  all_numeric <- all(sapply(data[, -1], is.numeric))
  
  # Check for Non-Negative Values
  non_negative_values <- all(sapply(data[, -1], function(x) all(x >= 0)))
  
  # Return results
  list(
    year_numeric = year_numeric,
    data_before_2024_na = data_before_2024_na,
    all_numeric = all_numeric,
    non_negative_values = non_negative_values
  )
}

# Perform tests on the cleaned COVID data
cleaned_covid_results <- perform_covid_tests(cleaned_covid_data)

# Print results for the cleaned COVID data
cat("Cleaned COVID Data Test Results:\n")
cat("Year Numeric:", cleaned_covid_results$year_numeric, "\n")
cat("Non-NA values from January 2020 to February 2024:", cleaned_covid_results$data_before_2024_na, "\n")
cat("All Numeric Data:", cleaned_covid_results$all_numeric, "\n")
cat("Non-Negative Values:", cleaned_covid_results$non_negative_values, "\n")

# Perform tests on the simulated COVID data
simulated_covid_results <- perform_covid_tests(simulated_covid_data)

# Print results for the simulated COVID data
cat("\nSimulated COVID Data Test Results:\n")
cat("Year Numeric:", simulated_covid_results$year_numeric, "\n")
cat("Non-NA values from January 2020 to February 2024:", simulated_covid_results$data_before_2024_na, "\n")
cat("All Numeric Data:", simulated_covid_results$all_numeric, "\n")
cat("Non-Negative Values:", simulated_covid_results$non_negative_values, "\n")

