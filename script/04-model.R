---
#### Preamble ####
# Purpose: build the linear models 
# Author: Yiyi Feng
# Date: 16th April 2024
# Contact: yiyi.feng@mail.utoronto.ca
# License: MIT
# Pre-requisites: "data/cleaned_data/cleaned_data_covid.csv" file and "data/cleaned_data/cleaned_data_fig2.csv" file.
---

#### Workspace setup ####
library(tidyverse)
library(here)
library(tidyr)
library(tibble)
library(dplyr)
library(caret)


#### Read data ####
cleaned_data_figtwo <-
  read_csv(
    file = "data/cleaned_data/cleaned_data_fig2.csv",
    show_col_types = FALSE)

cleaned_data_covid <-
  read_csv(
    file = "data/cleaned_data/cleaned_data_covid.csv",
    show_col_types = FALSE)


# Only take the rows representing all population
cleaned_data_model <- cleaned_data_figtwo[cleaned_data_figtwo$population_group == "All Population", ]

# Convert date_mmm_yy to Date format
cleaned_data_model$date_mmm_yy <- as.Date(cleaned_data_model$date_mmm_yy)

# Find the earliest year and month
earliest_year <- min(format(cleaned_data_model$date_mmm_yy, "%Y"))
earliest_month <- min(format(cleaned_data_model$date_mmm_yy, "%m"))

#sum the number of homeless for different age groups
cleaned_data_model$total_population <- rowSums(cleaned_data_model[, c("ageunder16", "age16_24", "age25_44", "age45_64", "age65over")])


# Filter homeless data to include only the years 2020 to 2024
cleaned_data_model_filtered <- cleaned_data_model[cleaned_data_model$date_mmm_yy >= "2020/1/1" & cleaned_data_model$date_mmm_yy <= "2024/2/1", ]

# Reshape the COVID-19 data to long format
cleaned_data_covid_long <- pivot_longer(cleaned_data_covid, -Year, names_to = "Month", values_to = "Cases")

# Remove rows with NA values
cleaned_data_covid_long <- na.omit(cleaned_data_covid_long)

# Generate a sequence of dates from January 2020 to February 2024
dates <- seq(as.Date("2020-01-01"), as.Date("2024-02-01"), by = "month")

cleaned_data_covid_long$date_mmm_yy <- dates

# Add all columns from cleaned_data_model_filtered to cleaned_data_covid_long based on the Date_mm_dd column
cleaned_data_covid_long_merged <- merge(cleaned_data_covid_long, cleaned_data_model_filtered, by = "date_mmm_yy", all.x = TRUE)

#### Save data ####
write_csv(
  x = cleaned_data_covid_long_merged,
  file = "data/cleaned_data/cleaned_data_model.csv"
)

# Define outcome variable
outcome_variable <- "Cases"

# Define predictor variables for each age group and total homeless population
predictor_variables <- c("ageunder16", "age16_24", "age25_44", "age45_64", "age65over", "total_population")

# Create a list to store model results
model_results <- list()

# Loop through each predictor variable
for (predictor in predictor_variables) {
  # Define formula for the regression model
  formula <- as.formula(paste(outcome_variable, "~", predictor))
  
  # Split the data into training and testing sets (70% training, 30% testing)
  set.seed(3) # for reproducibility
  train_index <- createDataPartition(cleaned_data_covid_long_merged$Cases, p = 0.7, list = FALSE)
  train_data <- cleaned_data_covid_long_merged[train_index, ]
  test_data <- cleaned_data_covid_long_merged[-train_index, ]
  
  # Fit the multiple linear regression model using training data
  lm_model <- lm(formula, data = train_data)
  
  # Evaluate the model's performance on the test data
  predictions <- predict(lm_model, newdata = test_data)
  rmse <- RMSE(predictions, test_data$Cases)
  r_squared <- R2(predictions, test_data$Cases)
  
  # Store model results
  model_results[[predictor]] <- list(
    model = lm_model,
    rmse = rmse,
    r_squared = r_squared
  )
  
  # Save the model as an .rds file in the 'model' directory
  saveRDS(
    lm_model,
    file = file.path("model", paste0("model_", predictor, ".rds"))
  )
}

# Print model results
for (predictor in predictor_variables) {
  cat("Model results for", predictor, "\n")
  print(model_results[[predictor]])
  cat("\n")
}

