# Load necessary libraries
library(readr)  # For reading CSV files
library(dplyr)  # For data manipulation

# Set your working directory to the location of your CSV file
setwd("E:/DataScience/Top_10_Recommendations/obtain data/")

# Read the CSV file
postcode_data <- read_csv("E:/DataScience/Top_10_Recommendations/obtain data/Postcode to LSOA.csv")

# Clean the data
postcode_data_clean <- postcode_data %>%
  select(pcd7, pcd8, pcds, lsoa11cd, msoa11cd, ladcd, ladnm) %>%
  rename(
    postcode_7 = pcd7,
    postcode_8 = pcd8,
    postcode_standard = pcds,
    lsoa_code = lsoa11cd,
    msoa_code = msoa11cd,
    lad_code = ladcd,
    local_authority = ladnm
  ) %>%
  drop_na() %>%
  distinct()

# Search for local authorities containing "Cornwall"
filtered_data <- postcode_data_clean %>%
  filter(grepl("Cornwall|Bristol", local_authority, ignore.case = TRUE))

# View the filtered data
print(filtered_data)

# Optionally, save the filtered data to a new CSV file
write_csv(filtered_data, "E:/DataScience/Top_10_Recommendations/cleaned data/cleaned_postcode_data.csv")
