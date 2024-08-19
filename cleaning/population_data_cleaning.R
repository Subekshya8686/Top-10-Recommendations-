# Load necessary libraries
library(dplyr)
library(readr)

# Define the path to your CSV file
population_data_path <- "E:/DataScience/Top_10_Recommendations/obtain data/population_dataset/population_data.csv"

# Read the CSV file
population_data <- read_csv(population_data_path)

# Check column names before cleaning (if necessary)
print(colnames(population_data))

# Assuming 'Population2011' is one of the columns, calculate 'Population2023'
population_data <- population_data %>%
  mutate(Population = 1.00561255390388033 * Population)

# Define the path for the cleaned file
cleaned_population_data_path <- "E:/DataScience/Top_10_Recommendations/cleaned data/cleaned_population_data.csv"

# Save the cleaned data to a CSV file
write_csv(population_data, cleaned_population_data_path)

# Print confirmation message
print("Population data cleaned, calculated, and saved successfully!")

# Display the data frame with the calculated Population2023
print(population_data)
