# Load necessary libraries
library(dplyr)
library(readr)

# Define the path to your CSV file
population_data_path <- "E:/DataScience/Top_10_Recommendations/obtain data/population_dataset/population_data.csv"

# Read CSV file
population_data <- read_csv(population_data_path)

# Check column names before cleaning (if necessary)
print(colnames(population_data))

# Perform any necessary data cleaning steps
# Example: Removing rows with NA values
population_data <- na.omit(population_data)

# Define path for cleaned file
cleaned_population_data_path <- "E:/DataScience/Top_10_Recommendations/cleaned data/cleaned_population_data.csv"

# Save cleaned data to CSV file
write_csv(population_data, cleaned_population_data_path)

# Print confirmation message
print("Population data cleaned and saved successfully!")

population_data

