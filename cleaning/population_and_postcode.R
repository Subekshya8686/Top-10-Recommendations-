# Load required libraries
library(dplyr)
library(readr)

# Read the data files
population_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/cleaned_population_data.csv")
postcode_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/cleaned_postcode_data.csv")

# Clean and format postcode columns in both datasets
population_data <- population_data %>%
  mutate(Postcode = gsub(" ", "", toupper(Postcode)))

postcode_data <- postcode_data %>%
  mutate(postcode_standard = gsub(" ", "", toupper(postcode_standard))) %>%
  mutate(PostcodeDistrict = substr(postcode_standard, 1, 4))  # Extract postcode district

# Filter population data for postcodes starting with BS or TR
filtered_population_data <- population_data %>%
  filter(grepl("^BS|^TR", Postcode))

# Merge filtered population data with postcode data using PostcodeDistrict
merged_population_postcode_data <- filtered_population_data %>%
  left_join(postcode_data, by = c("Postcode" = "PostcodeDistrict"))

# Check the result of the merge
head(merged_population_postcode_data)

# Save the merged data to a new CSV file
write_csv(merged_population_postcode_data, "E:/DataScience/Top_10_Recommendations/cleaned data/merged_population_postcode_data.csv")



