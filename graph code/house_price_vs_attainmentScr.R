# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

# Read the data
school_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/filtered_school_data.csv")
house_price_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/filtered_house_price_data.csv")

# Clean the data by removing rows with NA values in relevant columns and standardizing postcodes
school_data_clean <- school_data %>%
  filter(!is.na(ATT8SCR) & !is.na(county)) %>%
  mutate(PCODE = gsub(" ", "", toupper(PCODE)))

house_price_data_clean <- house_price_data %>%
  mutate(Postcode = gsub(" ", "", toupper(Postcode)))

# Convert ATT8SCR to numeric and replace NA values with 0
school_data_clean <- school_data_clean %>%
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>%
  replace_na(list(ATT8SCR = 0))

# Filter data for the academic year 2021-2022 and remove zero values
data_2021_2022 <- school_data_clean %>%
  filter(year == "2021-2022" & ATT8SCR > 0)

# Filter house price data for the year 2022
house_price_2022 <- house_price_data_clean %>%
  filter(grepl("2022", Date))
ggplot(school_data_clean, aes(x = PCODE, y = ATT8SCR)) + geom_point() + theme_minimal()
ggplot(house_price_2022, aes(x = Postcode, y = Price)) + geom_point() + theme_minimal()


# Merge the datasets on the Postcode column
merged_data <- merge(data_2021_2022, house_price_2022, by.x = "PCODE", by.y = "Postcode")

nrow(merged_data)


# Check if merged data is empty
if (nrow(merged_data) == 0) {
  print("Merged data is empty. No matching postcodes.")
} else {
  # Extract relevant columns and clean the data
  data <- merged_data %>%
    select(
      Price,
      ATT8SCR
    ) %>%
    rename(
      HousePrice = Price,
      AttainmentScore = ATT8SCR
    )
  # Check the first few rows of the cleaned data
  head(data)
  # Fit the linear model
  lm_model <- lm(HousePrice ~ AttainmentScore, data = data)
  # Print summary statistics of the model
  summary(lm_model)
  
  # Plot with ggplot2 and format y-axis
  ggplot(data, aes(x = AttainmentScore, y = HousePrice)) +
    geom_point() +  # Plot data points
    geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add line of best fit
    scale_y_continuous(labels = scales::comma_format()) +  # Format y-axis as comma-separated values
    labs(
      title = "House Prices vs Attainment 8 Score (2021-2022)",
      x = "Attainment 8 Score",
      y = "House Price"
    ) +
    theme_minimal()
}
