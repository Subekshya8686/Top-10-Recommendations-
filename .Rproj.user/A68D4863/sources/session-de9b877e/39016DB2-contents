# Install and load necessary packages
#install.packages(c("dplyr", "readr", "purrr"))
library(dplyr)
library(readr)
library(purrr)
library(tidyverse)

# Function to clean column names
clean_names <- function(df) {
  colnames(df) <- tolower(colnames(df))
  colnames(df) <- gsub(" ", "_", colnames(df))
  return(df)
}

# Define a function to read and clean crime data
read_and_clean_crime_data <- function(folder_path) {
  # List all CSV files in the directory, including subdirectories
  files <- list.files(folder_path, recursive = TRUE, full.names = TRUE, pattern = "\\.csv$")
  
  # Function to read a single file with specified column types
  read_crime_csv <- function(file) {
    read_csv(file, col_types = cols(
      `Crime ID` = col_character(),
      `Month` = col_character(),
      `Reported by` = col_character(),
      `Falls within` = col_character(),
      `Location` = col_character(),
      `LSOA code` = col_character(),
      `LSOA name` = col_character(),
      `Crime type` = col_character(),
      `Last outcome category` = col_character(),
      `Context` = col_logical(),
      `Longitude` = col_double(),
      `Latitude` = col_double()
    ))
  }
  
  # Read all CSV files and combine them into a single dataframe
  crime_data <- map_df(files, read_crime_csv)
  
  # Perform cleaning steps
  crime_data_clean <- crime_data %>%
    clean_names() %>%
    filter(!is.na(crime_type)) %>%
    mutate(across(where(is.character), ~na_if(.x, "")))
  
  return(crime_data_clean)
}

# Define path to the crime data folder
crime_data_path <- "E:/DataScience/Top_10_Recommendations/obtain data/crime_data"
cleaned_data_path <- "E:/DataScience/Top_10_Recommendations/cleaned data/crime_data_cleaned.csv"


# Read and clean crime data
crime_data_clean <- read_and_clean_crime_data(crime_data_path)

# Save cleaned crime data to the specified path
write_csv(crime_data_clean, cleaned_data_path)

View(crime_data_clean)
