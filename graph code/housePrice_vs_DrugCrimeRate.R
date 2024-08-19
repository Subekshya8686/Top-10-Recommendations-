library(dplyr)
library(readr)
library(ggplot2)

# Load the data files
crime_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/cleaned_crime_summary.csv")
postcode_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/cleaned_postcode_data.csv")
population_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/cleaned_population_data.csv")
house_price_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/filtered_house_price_data.csv")

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

# Filter and summarize crime data
drug_crimes <- crime_data %>%
  filter(grepl("Drugs", CrimeType, ignore.case = TRUE))
summary_drug_crimes <- drug_crimes %>%
  group_by(`LSOA code`) %>%
  summarise(TotalDrugCrimes = sum(TotalCrimes, na.rm = TRUE))

# Join the drug crimes summary with merged postcode and population data
final_data <- summary_drug_crimes %>%
  inner_join(merged_population_postcode_data, by = c("LSOA code" = "lsoa_code"))

# Clean house price data
house_price_data <- house_price_data %>%
  mutate(Postcode = gsub(" ", "", toupper(Postcode))) %>%
  rename(postcode_standard = Postcode)

# Check the column names in house_price_data
colnames(house_price_data)
# Check first few rows to confirm column names and data
head(house_price_data)


# Merge final data with house price data
final_data_with_prices <- final_data %>%
  inner_join(house_price_data, by = "postcode_standard")

# Calculate CrimeRatePer1000 and update final_data_with_prices
final_data_with_prices <- final_data_with_prices %>%
  mutate(CrimeRatePer1000 = (TotalDrugCrimes / Population) * 1000)

# Verify the update
print(colnames(final_data_with_prices))
print(head(final_data_with_prices))


# Check if final_data_with_prices has data
print(head(final_data_with_prices))

# Linear model
if (nrow(final_data_with_prices) > 0) {
  linear_model <- lm(Price ~ CrimeRatePer1000, data = final_data_with_prices)
  print(summary(linear_model))
  
  # Visualization
  ggplot(final_data_with_prices, aes(x = CrimeRatePer1000, y = Price)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(
      title = "House Price vs Drug Crime Rate (2022)",
      x = "Drug Crime Rate per 1000 People",
      y = "House Price"
    ) +
    theme_minimal()
} else {
  print("No data available for modeling.")
}

# Check column names and data structure
print(colnames(final_data_with_prices))
print(head(final_data_with_prices))

# Linear model
if ("CrimeRatePer1000" %in% colnames(final_data_with_prices) && nrow(final_data_with_prices) > 0) {
  linear_model <- lm(Price ~ CrimeRatePer1000, data = final_data_with_prices)
  print(summary(linear_model))
  
  # Visualization
  ggplot(final_data_with_prices, aes(x = CrimeRatePer1000, y = Price)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    scale_x_continuous(labels = scales::comma_format(), 
                       breaks = scales::pretty_breaks()) +
    scale_y_continuous(labels = scales::comma_format(), 
                       breaks = scales::pretty_breaks()) +
    labs(
      title = "House Price vs Drug Crime Rate (2022)",
      x = "Drug Crime Rate per 1000 People",
      y = "House Price"
    ) +
    theme_minimal()
} else {
  print("No data available for modeling or 'CrimeRatePer1000' column is missing.")
}

ggplot(final_data_with_prices, aes(x = CrimeRatePer1000, y = Price)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "black", linetype = "dashed") +
  labs(
    title = "House Prices vs Drug Crime Rate (Colored by Property Type)",
    x = "Drug Crime Rate per 1000 People",
    y = "House Price (£)"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")


