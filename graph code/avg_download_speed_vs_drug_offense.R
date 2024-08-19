# Load required libraries
library(dplyr)
library(ggplot2)
library(readr)

# Read the CSV files
crime_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/cleaned_crime_summary.csv")
postcode_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/cleaned_postcode_data.csv")
population_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/cleaned_population_data.csv")
broadband_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/merged_broad_band_speed_data.csv")

# Clean and format postcode columns
population_data <- population_data %>%
  mutate(Postcode = gsub(" ", "", toupper(Postcode)))

postcode_data <- postcode_data %>%
  mutate(postcode_standard = gsub(" ", "", toupper(postcode_standard))) %>%
  mutate(PostcodeDistrict = substr(postcode_standard, 1, 4))  # Extract postcode district

broadband_data <- broadband_data %>%
  mutate(postcode = gsub(" ", "", toupper(postcode)))

# Filter population and broadband data for postcodes starting with BS or TR
filtered_population_data <- population_data %>%
  filter(grepl("^BS|^TR", Postcode))

filtered_broadband_data <- broadband_data %>%
  filter(grepl("^BS|^TR", postcode))

# Merge filtered population data with postcode data using PostcodeDistrict
merged_population_postcode_data <- filtered_population_data %>%
  left_join(postcode_data, by = c("Postcode" = "PostcodeDistrict"))

# Filter and summarize drug-related crime data
drug_crimes <- crime_data %>%
  filter(grepl("Drugs", CrimeType, ignore.case = TRUE))

summary_drug_crimes <- drug_crimes %>%
  group_by(`LSOA code`) %>%
  summarise(TotalDrugCrimes = sum(TotalCrimes, na.rm = TRUE))

# Join the drug crimes summary with merged postcode and population data
final_data <- summary_drug_crimes %>%
  inner_join(merged_population_postcode_data, by = c("LSOA code" = "lsoa_code"))

# Calculate DrugCrimeRate per 1000 people
final_data <- final_data %>%
  mutate(DrugCrimeRatePer1000 = (TotalDrugCrimes / Population) * 1000)

# Merge the final data with broadband data
merged_data <- final_data %>%
  inner_join(filtered_broadband_data, by = c("postcode_standard" = "postcode"))

# Check if merged data is empty
if (nrow(merged_data) == 0) {
  print("Merged data is empty. No matching postcodes.")
} else {
  # Extract relevant columns and clean the data
  data <- merged_data %>%
    select(
      DrugCrimeRatePer1000,
      `median_download_speed_(mbit/s)`
    ) %>%
    rename(
      DrugCrimeRate = DrugCrimeRatePer1000,
      DownloadSpeed = `median_download_speed_(mbit/s)`
    )
  
  # Check the first few rows of the cleaned data
  head(data)
  
  # Fit the linear model
  lm_model <- lm(DownloadSpeed ~ DrugCrimeRate, data = data)
  
  # Print summary statistics of the model
  summary(lm_model)
  
  # Plot with ggplot2 and format y-axis
  ggplot(data, aes(x = DrugCrimeRate, y = DownloadSpeed)) +
    geom_point() +  # Plot data points
    geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add line of best fit
    scale_x_continuous(labels = scales::comma_format()) +
    scale_y_continuous(labels = scales::comma_format()) +
    labs(
      title = "Average Download Speed vs Drug Offense Rate (2022)",
      x = "Drug Crime Rate per 1000 People",
      y = "Average Download Speed (mbit/s)"
    ) +
    theme_minimal()
}
