# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# Read the data files
school_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/filtered_school_data.csv")
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

# Clean the school data
school_data_clean <- school_data %>%
  filter(!is.na(ATT8SCR) & !is.na(county)) %>%
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>%
  replace_na(list(ATT8SCR = 0))

# Filter data for the academic year 2021-2022
data_2021_2022 <- school_data_clean %>%
  filter(year == "2021-2022" & ATT8SCR > 0)

# Merge school data with postcode data
merged_school_data <- data_2021_2022 %>%
  mutate(PCODE = gsub(" ", "", toupper(PCODE))) %>%
  left_join(postcode_data, by = c("PCODE" = "postcode_standard"))

# Merge school data with population data
merged_data_with_population <- merged_school_data %>%
  left_join(filtered_population_data, by = c("postcode_standard" = "Postcode"))

# Filter and summarize drug crime data
drug_crimes <- crime_data %>%
  filter(grepl("Drugs", CrimeType, ignore.case = TRUE)) %>%
  group_by(`LSOA code`) %>%
  summarise(TotalDrugCrimes = sum(TotalCrimes, na.rm = TRUE))

# Merge drug crime summary with the merged school and population data
final_data <- drug_crimes %>%
  left_join(merged_data_with_population, by = c("LSOA code" = "lsoa_code"))

# Calculate CrimeRatePer1000
final_data <- final_data %>%
  mutate(CrimeRatePer1000 = (TotalDrugCrimes / Population) * 1000)

# Check if final_data has data
if (nrow(final_data) > 0) {
  # Fit the linear model: Attainment 8 Score vs Drug Offense Rate
  linear_model <- lm(ATT8SCR ~ CrimeRatePer1000, data = final_data)
  
  # Print summary statistics of the model
  print(summary(linear_model))
  
  # Visualization
  ggplot(final_data, aes(x = CrimeRatePer1000, y = ATT8SCR)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(
      title = "Attainment 8 Score vs Drug Offense Rate (2022)",
      x = "Drug Crime Rate per 1000 People",
      y = "Attainment 8 Score"
    ) +
    theme_minimal()
} else {
  print("No data available for modeling.")
}
