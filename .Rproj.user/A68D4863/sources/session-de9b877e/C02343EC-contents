library(dplyr)
library(readr)
library(purrr)
library(tidyr)

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

# Define folder path for housing data
housing_data_path <- "E:/DataScience/Top_10_Recommendations/obtain data/house_pricing"

# List all CSV files in the directory
csv_files <- list.files(housing_data_path, full.names = TRUE, pattern = "\\.csv$")

# Read and clean each CSV file
cleaned_housing_data <- map(csv_files, read_and_clean_csv)

# Merge cleaned data into one dataframe
merged_housing_data <- bind_rows(cleaned_housing_data)

# Define path for merged file
merged_housing_file_path <- "E:/DataScience/Top_10_Recommendations/cleaned data/merged_house_pricing_data.csv"

# Save merged data to CSV file
write_csv(merged_housing_data, merged_housing_file_path)

# Print confirmation message
print("Housing data cleaned and merged successfully!")


merged_housing_data
