library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(tidyverse)

# Function to clean column names
clean_names <- function(df) {
  colnames(df) <- tolower(colnames(df))
  colnames(df) <- gsub(" ", "_", colnames(df))
  return(df)
}

# Function to read and clean data from a given file
read_and_clean_csv <- function(file_path) {
  # Read CSV file
  data <- read_csv(file_path)
  
  # Clean column names
  data <- clean_names(data)
  
  # Additional cleaning steps (if necessary)
  data_clean <- data %>%
    mutate(across(where(is.character), ~na_if(.x, "")))
  
  return(data_clean)
}

# Define file paths
file_paths <- c(
  "E:/DataScience/Top_10_Recommendations/obtain data/broad_band_speed/201805_fixed_pc_performance_r03.csv",
  "E:/DataScience/Top_10_Recommendations/obtain data/broad_band_speed/201809_fixed_pc_coverage_r01.csv"
)

# Read and clean each CSV file
cleaned_data <- map(file_paths, read_and_clean_csv)

# Merge cleaned data into one dataframe
merged_data <- bind_rows(cleaned_data)

# Define path for merged file
merged_file_path <- "E:/DataScience/Top_10_Recommendations/cleaned data/merged_broad_band_speed_data.csv"

# Save merged data to CSV file
write_csv(merged_data, merged_file_path)

# View merged data (optional)
print("Data from both files merged and saved successfully!")


#view(merged_data)
merged_data
cleaned_data
