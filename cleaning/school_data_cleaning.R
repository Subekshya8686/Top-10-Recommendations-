library(readr)   # For reading CSV files
library(dplyr)   # For data manipulation functions
library(tidyr)   # For handling missing values

# Define function to read, clean, and standardize data from a directory
read_and_clean_data <- function(directory, county_name, year) {
  files <- list.files(path = directory, pattern = "*.csv", full.names = TRUE, recursive = TRUE)
  
  data_list <- lapply(files, function(file) {
    df <- read_csv(file)
    # Standardize column types: Convert all columns to character
    df <- df %>%
      mutate(across(everything(), as.character)) %>%
      # Replace NA values with "0"
      mutate(across(everything(), ~ replace_na(.x, "0"))) %>%
      # Add the county and year columns
      mutate(county = county_name, year = year)
    return(df)
  })
  
  # Combine all data frames into one
  combined_data <- bind_rows(data_list)
  return(combined_data)
}

# Read and clean data from Bristol directories
bristol_data_2021_2022 <- read_and_clean_data("E:/DataScience/Top_10_Recommendations/obtain data/school/bristol/2021-2022", "Bristol", "2021-2022")
bristol_data_2022_2023 <- read_and_clean_data("E:/DataScience/Top_10_Recommendations/obtain data/school/bristol/2022-2023", "Bristol", "2022-2023")

# Read and clean data from Cornwall directories
cornwall_data_2021_2022 <- read_and_clean_data("E:/DataScience/Top_10_Recommendations/obtain data/school/cornwall/2021-2022", "Cornwall", "2021-2022")
cornwall_data_2022_2023 <- read_and_clean_data("E:/DataScience/Top_10_Recommendations/obtain data/school/cornwall/2022-2023", "Cornwall", "2022-2023")

# Combine data from all locations and years
combined_data <- bind_rows(
  bristol_data_2021_2022, bristol_data_2022_2023,
  cornwall_data_2021_2022, cornwall_data_2022_2023
)

# Keep only the desired columns
desired_columns <- c("PCODE","year", "SCHNAME", "ATT8SCR", "URN", "TOWN", "SCHOOLTYPE", "OFSTEDRATING","county")
filtered_data <- combined_data %>%
  select(all_of(desired_columns))


# Define the path to save the cleaned data
output_path <- "E:/DataScience/Top_10_Recommendations/cleaned data/filtered_school_data.csv"

# Write the filtered data to a CSV file
write_csv(filtered_data, output_path)
