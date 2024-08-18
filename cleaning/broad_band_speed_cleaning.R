library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(tidyverse)

# Define file paths
file_paths <- c(
  "E:/DataScience/Top_10_Recommendations/obtain data/broad_band_speed/201805_fixed_pc_performance_r03.csv",
  "E:/DataScience/Top_10_Recommendations/obtain data/broad_band_speed/201809_fixed_pc_coverage_r01.csv"
)

# Read and clean data from each file using a pipeline
cleaned_data <- map(file_paths, ~ read_csv(.x) %>%
                      # Clean column names
                      rename_with(~ tolower(.)) %>%
                      rename_with(~ gsub(" ", "_", .)) %>%
                      # Replace NA values with 0
                      mutate(across(everything(), ~ replace_na(.x, 0)))
)

# Merge cleaned data into one dataframe
merged_data <- bind_rows(cleaned_data)

# Define path for merged file
merged_file_path <- "E:/DataScience/Top_10_Recommendations/cleaned data/merged_broad_band_speed_data.csv"

# Save merged data to CSV file
write_csv(merged_data, merged_file_path)

# View merged data
print("Data from both files merged and saved successfully!")

# Optional: View merged data
View(merged_data)
merged_data
colnames(merged_data)
