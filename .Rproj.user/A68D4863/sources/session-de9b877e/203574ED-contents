library(readr)  # For reading CSV files
library(dplyr)  # For data manipulation functions
library(tidyverse)

# Set your working directory to the location of your CSV files
setwd("E:/DataScience/Top_10_Recommendations/obtain data/school/")

# Function to read and clean data from a directory
read_and_clean_data <- function(directory) {
  files <- list.files(path = directory, pattern = "*.csv", full.names = TRUE)
  data_list <- lapply(files, read_csv)
  cleaned_data <- lapply(data_list, na.omit)  # Clean each dataset
  merged_data <- Reduce(function(x, y) merge(x, y, all = TRUE), cleaned_data)  # Merge datasets
  return(merged_data)
}

# Read and clean data from Bristol and Cornwall directories
bristol_data <- read_and_clean_data("bristol/2020-2021")
cornwall_data <- read_and_clean_data("cornwall/2020-2021")

# Merge the data from both locations
merged_data <- merge(bristol_data, cornwall_data, all = TRUE)  # Adjust merge parameters as needed

# Save merged data to a new CSV file if needed
write_csv(merged_data, "E:/DataScience/Top_10_Recommendations/cleaned data/merged_schools_data.csv")  # row.names = FALSE by default

merged_data
view(merged_data)
