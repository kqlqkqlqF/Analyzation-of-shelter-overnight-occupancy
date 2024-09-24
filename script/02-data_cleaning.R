---
#### Preamble ####
# Purpose: Clean the data download from OpendataToronto to make the 
#          datas into a uniform and cleaned style
# Author: Yiyi Feng
# Date: 16th April 2024
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
#### Basic cleaning ####
#import raw data
raw_data_homeless <-
  read_csv(
    file = "data/raw_data/raw_data_homeless.csv",
    show_col_types = FALSE
  )

raw_data_covid_2020 <-
  read_csv(
    file = "data/raw_data/raw_data_covid_2020.csv",
    skip = 1, col_select = -1, show_col_types = FALSE
  )
raw_data_covid_2021 <-
  read_csv(
    file = "data/raw_data/raw_data_covid_2021.csv",
    skip = 1, col_select = -1, show_col_types = FALSE
  )

raw_data_covid_2022 <-
  read_csv(
    file = "data/raw_data/raw_data_covid_2022.csv",
    skip = 1, col_select = -1, show_col_types = FALSE
  )

raw_data_covid_2023 <-
  read_csv(
    file = "data/raw_data/raw_data_covid_2023.csv",
    skip = 1, col_select = -1, show_col_types = FALSE
  )

raw_data_covid_2024 <-
  read_csv(
    file = "data/raw_data/raw_data_covid_2024.csv",
    skip = 1, col_select = -1, show_col_types = FALSE
  )

#### Clean needed homeless data ####

#switch setting of local system time for converting date in data cleaning, 
#please mute this sentence if not needed on your computer
loc <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C") 

#switch the character formed time to date format and select information needed
cleaned_data_homeless <-
  clean_names(raw_data_homeless) |>
  mutate(date_mmm_yy = as.factor(date_mmm_yy)) |>
  mutate(date_mmm_yy = paste("01",date_mmm_yy,sep="-")) |>
  mutate(date_mmm_yy = strptime(date_mmm_yy,format="%d-%b-%y")) |>
  mutate(date_mmm_yy = as.Date(date_mmm_yy, format = "%Y-%m-%d"))
cleaned_data_homeless#take a look at the cleaned data for checking

cleaned_data_homeless <- na.omit(cleaned_data_homeless)

#set local time back to original setting, please mute this sentence if not needed on your computer
Sys.setlocale("LC_TIME", loc) 

#### Save data ####
write_csv(
  x = cleaned_data_homeless,
  file = "data/cleaned_data/cleaned_data_homeless.csv"
)


#### Clean needed COVID data ####

# Filter the rows based on the ...2 column containing "COVID-19"

filtered_data_2020 <- raw_data_covid_2020 %>%
  filter(...2 == "Disease" | ...2 == "COVID-19")
filtered_data_2021 <- subset(raw_data_covid_2021, ...2 == "COVID-19")
filtered_data_2022 <- subset(raw_data_covid_2022, ...2 == "COVID-19")
filtered_data_2023 <- subset(raw_data_covid_2023, ...2 == "COVID-195")
filtered_data_2024 <- subset(raw_data_covid_2024, ...2 == "COVID-19")

# Combine the filtered data into a new tibble
combined_data_covid <- bind_rows(filtered_data_2020, filtered_data_2021, filtered_data_2022, filtered_data_2023, filtered_data_2024)

# Remove the first row
new_combined_data_covid <- combined_data_covid[-1, ]

# Set column names using the values from the first row
colnames(new_combined_data_covid) <- as.character(combined_data_covid[1, ])

# Define the new names for the diseases
new_names <- c("2020", "2021", "2022", "2023", "2024")

# Replace the values in the "Disease" column
new_combined_data_covid$Year <- new_names

# Select the desired columns
selected_cols <- c("Year", "January", "February", "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December")

# Filter the dataframe to keep only the selected columns
filtered_combined_data_covid <- new_combined_data_covid %>% 
  select(all_of(selected_cols))

# Find the index of the row where the Year is "COVID 2024"
row_index <- which(filtered_combined_data_covid$Year == "2024")

# Replace numeric values in columns March to July with NA
filtered_combined_data_covid[row_index, 4:9] <- NA

#### Save data ####
write_csv(
  x = filtered_combined_data_covid,
  file = "data/cleaned_data/cleaned_data_covid.csv"
)


####summarize data for figures####

#clean up and summarize data needed for figure one
#select needed columns for figure one
figone_data <- 
  cleaned_data_homeless |>
  select(date_mmm_yy, population_group, returned_from_housing, returned_to_shelter, newly_identified, moved_to_housing, became_inactive, actively_homeless)

#Only remain the rows containing all population data
figone_data_clean <- subset(figone_data, figone_data$population_group == "All Population")
head(figone_data_clean)#Take a look at the data set for checking

#remove the population_group column since it doesn't contain valid data
figone_data_clean <- figone_data_clean %>% 
  select(-population_group)
head(figone_data_clean)#Take a look at the data set for checking

# Extract the year from the date_mmm_yy column
figone_data_clean$year <- format(figone_data_clean$date_mmm_yy, "%Y")

# Summarize the data by year
figone_data_clean <- figone_data_clean %>%
  group_by(year) %>%
  summarize(
    returned_from_housing = sum(returned_from_housing),
    returned_to_shelter = sum(returned_to_shelter),
    newly_identified = sum(newly_identified),
    moved_to_housing = sum(moved_to_housing),
    became_inactive = sum(became_inactive),
    actively_homeless = sum(actively_homeless)
  )

# Remove the 2024 row since there's only January data for 2024, which can cause statistical inaccuracy
figone_data_clean <- figone_data_clean %>% 
  slice(-n())
figone_data_clean

#### Save data ####
write_csv(
  x = figone_data_clean,
  file = "data/cleaned_data/cleaned_data_fig1.csv"
)

#summarize data for figure two
#only select the columns needed
figtwo_data_clean <- 
  cleaned_data_homeless |>
  select(date_mmm_yy, population_group,ageunder16, age16_24, age25_44, age45_64, age65over)
figtwo_data_clean

#### Save data ####
write_csv(
  x = figtwo_data_clean,
  file = "data/cleaned_data/cleaned_data_fig2.csv"
)

#summarize data for figure three
#select the columns needed
figthree_data_clean <- 
  cleaned_data_homeless |>
  select(date_mmm_yy, population_group,gender_male, gender_female, gender_transgender_non_binary_or_two_spirit)
figthree_data_clean

#extract year from the date_mmm_yy column
figthree_data_clean <- figthree_data_clean %>%
  mutate(year = lubridate::year(date_mmm_yy))

#group by year and population_group, and summarize the counts
figthree_data_clean <- figthree_data_clean %>%
  group_by(year, population_group) %>%
  summarize(gender_male = sum(gender_male),
            gender_female = sum(gender_female),
            gender_transgender_non_binary_or_two_spirit = sum(gender_transgender_non_binary_or_two_spirit))

#remove rows with 2024 or NA in the year column, same reason as above
figthree_data_clean <- figthree_data_clean %>%
  filter(!is.na(year))

#### Save data ####
write_csv(
  x = figthree_data_clean,
  file = "data/cleaned_data/cleaned_data_fig3.csv"
)


