# Load the necessary packages
library(readr)
library(dplyr)
library(lubridate)  # For date handling

# Define the path to the crime data folder
crime_data_path <- "E:/DataScience/Top_10_Recommendations/obtain data/crime_data"
cleaned_data_path <- "E:/DataScience/Top_10_Recommendations/cleaned data/crime_data_summary_cleaned.csv"

# Load and clean the crime data
crime_data <- list.files(crime_data_path, recursive = TRUE, full.names = TRUE, pattern = "\\.csv$") %>%
  map_df(~ read_csv(.x))

# Calculate the total number of crimes by Year, CrimeType, and FallsWithin
crime_summary <- crime_data %>%
  group_by(Year = Month, CrimeType= `Crime type`, FallsWithin= `Falls within`, `LSOA code`) %>%
  summarize(TotalCrimes = n(), .groups = 'drop')

# View the summarized data
print(crime_summary)
# Define the path for the cleaned data output
cleaned_data_path <- "E:/DataScience/Top_10_Recommendations/cleaned data/cleaned_crime_summary.csv"

# Write the cleaned data to a CSV file using the `file` argument
write_csv(crime_summary, file = cleaned_data_path)
colnames(crime_summary)
