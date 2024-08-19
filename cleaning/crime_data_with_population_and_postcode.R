library(dplyr)
library(readr)

# Load the data files
crime_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/cleaned_crime_summary.csv")
postcode_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/cleaned_postcode_data.csv")
population_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/cleaned_population_data.csv")

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

# Summarize all crime data by LSOA code
summary_crimes <- crime_data %>%
  group_by(`LSOA code`) %>%
  summarise(TotalCrimes = sum(TotalCrimes, na.rm = TRUE), .groups = 'drop')

# Join the crime summary with merged postcode and population data
final_data <- summary_crimes %>%
  inner_join(merged_population_postcode_data, by = c("LSOA code" = "lsoa_code"))

# Save the final data to a CSV file
write_csv(final_data, "E:/DataScience/Top_10_Recommendations/cleaned data/crime_data_with_postcode_and_population.csv")

# Check the structure of final_data
print(colnames(final_data))
print(head(final_data))
