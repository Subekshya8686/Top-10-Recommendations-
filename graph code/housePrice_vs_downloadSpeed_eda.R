# Load required libraries
library(dplyr)
library(ggplot2)
library(readr)

# Read the CSV files
house_price_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/filtered_house_price_data.csv")
broadband_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/merged_broad_band_speed_data.csv")

# Clean and format postcode columns
house_price_data <- house_price_data %>%
  mutate(Postcode = gsub(" ", "", toupper(Postcode)))

broadband_data <- broadband_data %>%
  mutate(postcode = toupper(postcode))

# Filter broadband data for postcodes starting with "BS" or "TR"
filtered_broadband_data <- broadband_data %>%
  filter(grepl("^BS|^TR", postcode))

# Filter house price data for postcodes starting with "BS" or "TR"
new_house_price_data <- house_price_data %>%
  filter(grepl("^BS|^TR", Postcode))

# Merge the filtered datasets on the Postcode column
merged_data <- merge(new_house_price_data, filtered_broadband_data, by.x = "Postcode", by.y = "postcode")

# Check if merged data is empty
if (nrow(merged_data) == 0) {
  print("Merged data is empty. No matching postcodes.")
} else {
  # Extract relevant columns and clean the data
  data <- merged_data %>%
    select(
      Price,
      `median_download_speed_(mbit/s)`
    ) %>%
    rename(
      HousePrice = Price,
      DownloadSpeed = `median_download_speed_(mbit/s)`
    )
  
  # Check the first few rows of the cleaned data
  head(data)
  
  # Fit the linear model
  lm_model <- lm(HousePrice ~ DownloadSpeed, data = data)
  
  # Print summary statistics of the model
  summary(lm_model)
  
  # Plot with ggplot2 and format y-axis
  ggplot(data, aes(x = DownloadSpeed, y = HousePrice)) +
    geom_point() +  # Plot data points
    geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add line of best fit
    scale_y_continuous(labels = scales::comma_format()) +  # Format y-axis as comma-separated values
    labs(
      title = "House Prices vs Download Speed",
      x = "Download Speed (mbit/s)",
      y = "House Price"
    ) +
    theme_minimal()
}
