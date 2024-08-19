library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(fmsb)

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

# Ensure the 'Year' column is of type character (if not already)
crime_data <- crime_data %>%
  mutate(Year = as.character(Year))

# Filter and summarize crime data for each type
drug_crimes <- crime_data %>%
  filter(grepl("Drugs", CrimeType, ignore.case = TRUE)) %>%
  group_by(`LSOA code`, Year, FallsWithin) %>%
  summarise(TotalDrugCrimes = sum(TotalCrimes, na.rm = TRUE), .groups = 'drop')

vehicle_crimes <- crime_data %>%
  filter(grepl("Vehicle crime", CrimeType, ignore.case = TRUE)) %>%
  group_by(`LSOA code`, Year,FallsWithin) %>%
  summarise(TotalVehicleCrimes = sum(TotalCrimes, na.rm = TRUE), .groups = 'drop')

robbery_crimes <- crime_data %>%
  filter(grepl("Robbery", CrimeType, ignore.case = TRUE)) %>%
  group_by(`LSOA code`, Year,FallsWithin) %>%
  summarise(TotalRobberyCrimes = sum(TotalCrimes, na.rm = TRUE), .groups = 'drop')

# Merge the crime summaries with merged postcode and population data
final_data_drug <- drug_crimes %>%
  inner_join(merged_population_postcode_data, by = c("LSOA code" = "lsoa_code")) %>%
  mutate(CrimeRatePer10000 = (TotalDrugCrimes / Population) * 10000) %>%
  mutate(
    local_authority = replace_na(local_authority, "Unknown") # Replace NA values with "Unknown"
  ) %>%
  filter(substr(Year, 1, 4) == "2022")

final_data_vehicle <- vehicle_crimes %>%
  inner_join(merged_population_postcode_data, by = c("LSOA code" = "lsoa_code")) %>%
  mutate(CrimeRatePer10000 = (TotalVehicleCrimes / Population) * 10000)

final_data_robbery <- robbery_crimes %>%
  inner_join(merged_population_postcode_data, by = c("LSOA code" = "lsoa_code")) %>%
  mutate(CrimeRatePer10000 = (TotalRobberyCrimes / Population) * 10000)

# Visualization
# Box Plot for Drug Crime Rate
# Box Plot for Drug Crime Rate by Local Authority
ggplot(final_data_drug, aes(x = FallsWithin, y = CrimeRatePer10000)) +
  geom_boxplot() +
  labs(
    title = "Drug Crime Rate by Local Authority (2022)",
    x = "Local Authority",
    y = "Drug Crime Rate per 10000 People"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))


# Filter vehicle crime data for a specific month in 2022 (e.g., January)
# Filter the data for the year 2022 first
vehicle_crime_year <- final_data_vehicle %>%
  filter(substr(Year, 1, 4) == "2022")
# Example of how to prepare data for fmsb radar chart
# Filter vehicle crime data for January 2022
vehicle_crime_month <- vehicle_crime_year %>%
  filter(Year == "2022-01")

# Aggregate data by LSOA code for the specific month
aggregated_vehicle_crime_month <- vehicle_crime_month %>%
  group_by(`LSOA code`) %>%
  summarise(
    TotalVehicleCrimes = sum(TotalVehicleCrimes, na.rm = TRUE),
    AverageCrimeRatePer10000 = mean(CrimeRatePer10000, na.rm = TRUE),
    .groups = 'drop'
  )

# Select only necessary columns and convert to a wide format
radar_data_fmsb <- aggregated_vehicle_crime_month %>%
  select(AverageCrimeRatePer10000) %>%
  as.data.frame()

# Prepare the data for fmsb
# fmsb requires data in a specific format: add max and min values for each variable
radar_data_fmsb <- rbind(rep(max(radar_data_fmsb$AverageCrimeRatePer10000), ncol(radar_data_fmsb)),
                         rep(min(radar_data_fmsb$AverageCrimeRatePer10000), ncol(radar_data_fmsb)),
                         radar_data_fmsb)

# Set column names (assuming a single variable)
colnames(radar_data_fmsb) <- c("AverageCrimeRatePer10000")

# Create radar chart with fmsb
radarchart(radar_data_fmsb,
           axistype = 1,
           pcol = "blue", pfcol = rgb(0.2,0.5,0.5,0.5), plwd = 2,
           cglcol = "grey", cglty = 1, axislabcol = "grey",
           title = "Vehicle Crime Rate per 10,000 People in January 2022")

# Filter robbery crime data for the year 2022
robbery_crime_year <- final_data_robbery %>%
  filter(substr(Year, 1, 4) == "2022")

# Create the pie chart
ggplot(robbery_crime_year, aes(x = "", y = CrimeRatePer10000, fill = FallsWithin)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(
    title = "Robbery Crime Rate Distribution (2022)",
    fill = "Local Authority"
  ) +
  theme_void()

summary(robbery_crime_year)

# Line Chart for Drug Crime Rate per 10000 People (showing both counties)
ggplot(final_data_drug, aes(x = Year, y = CrimeRatePer10000, color= FallsWithin)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Drug Crime Rate per 10000 People (2022)",
    x = "Year",
    y = "Drug Crime Rate per 10000 People"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
