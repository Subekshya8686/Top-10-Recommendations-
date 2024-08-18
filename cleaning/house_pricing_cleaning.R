# Load required libraries
library(dplyr)
library(readr)
library(purrr)

# Specify the path to the directories containing the CSV files and where to save the merged file
housing_data_path <- "E:/DataScience/Top_10_Recommendations/obtain data/house_pricing"
merged_housing_file_path <- "E:/DataScience/Top_10_Recommendations/cleaned data"

# List all CSV files in the directory
csv_files <- list.files(path = housing_data_path, pattern = "*.csv", full.names = TRUE)

# Read, combine all CSV files, assign column names, and filter for Bristol and Cornwall counties
filtered_House_Price_data <- csv_files %>%
  map(~ read_csv(.x, col_names = FALSE)) %>%  # Read the CSV files without assuming column names
  bind_rows() %>%  # Combine all the data into one data frame
  set_names(c("ID", "Price", "Date", "Postcode", "Property_Type", "New_Build",
              "Tenure", "Number", "Secondary_Street", "Street", "Locality",
              "Town", "District", "County", "Category_Type", "Ownership_Type")) %>%  # Set the column names
  filter(County %in% c("CITY OF BRISTOL", "CORNWALL"))  # Filter to include only rows from Bristol and Cornwall

# Print the first few rows of the filtered data to verify
print(head(filtered_House_Price_data))

# Optionally, save the filtered data to a new CSV file
write_csv(filtered_House_Price_data, file.path(merged_housing_file_path, "filtered_house_price_data.csv"))

# Print the dimensions of the filtered data to verify
print(dim(filtered_House_Price_data))
colnames(filtered_House_Price_data)
