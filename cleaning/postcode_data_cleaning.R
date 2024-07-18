# Load necessary library
library(readr)  # For reading CSV files

# Set your working directory to the location of your CSV file
setwd("E:/DataScience/Top_10_Recommendations/obtain data/")

# Read the CSV file
postcode_data <- read_csv("Postcode to LSOA.csv")

# Display the structure of the data frame to understand its columns and types
str(postcode_data)

# View the first few rows of the data to inspect its contents
head(postcode_data)


# If you need to save the cleaned data back to a CSV file, you can use write_csv
write_csv(postcode_data, "E:/DataScience/Top_10_Recommendations/cleaned data/cleaned_Postcode_to_LSOA.csv")

# Print summary or structure of postcode_data to verify
summary(postcode_data)
str(postcode_data)

postcode_data
