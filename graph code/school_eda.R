# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the data
school_data <- read.csv("E:/DataScience/Top_10_Recommendations/cleaned data/filtered_school_data.csv")

# Clean the data by removing rows with NA values in relevant columns
school_data_clean <- school_data %>%
  filter(!is.na(ATT8SCR) & !is.na(county))

# Convert ATT8SCR to numeric and replace NA values with 0
school_data_clean <- school_data_clean %>%
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>%
  replace_na(list(ATT8SCR = 0))

# Filter data for the academic year 2021-2022 and remove zero values
data_2021_2022 <- school_data_clean %>%
  filter(year == "2021-2022" & ATT8SCR > 0)

# 1. Boxplot for Average Attainment 8 Score in 2021-2022 for Both Counties
ggplot(data_2021_2022, aes(x = county, y = ATT8SCR, fill = county)) +
  geom_boxplot() +
  labs(title = "Average Attainment 8 Score (2021-2022) for Both Counties",
       x = "County",
       y = "Attainment 8 Score") +
  theme_minimal()

# 2. Line Chart for Bristol Schools
# Filter data for Bristol, remove zero values, and calculate average score by school name
bristol_data <- data_2021_2022 %>%
  filter(county == "Bristol") %>%
  group_by(SCHNAME) %>%
  summarize(AverageATT8SCR = mean(ATT8SCR, na.rm = TRUE)) %>%
  arrange(SCHNAME)

# Create a line chart for Bristol
ggplot(bristol_data, aes(x = SCHNAME, y = AverageATT8SCR, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Average Attainment 8 Score for Bristol Schools (2021-2022)",
       x = "School Name",
       y = "Average Attainment 8 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 3. Line Chart for Cornwall Schools
# Filter data for Cornwall, remove zero values, and calculate average score by school name
cornwall_data <- data_2021_2022 %>%
  filter(county == "Cornwall") %>%
  group_by(SCHNAME) %>%
  summarize(AverageATT8SCR = mean(ATT8SCR, na.rm = TRUE)) %>%
  arrange(SCHNAME)

# Create a line chart for Cornwall
ggplot(cornwall_data, aes(x = SCHNAME, y = AverageATT8SCR, group = 1)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Average Attainment 8 Score for Cornwall Schools (2021-2022)",
       x = "School Name",
       y = "Average Attainment 8 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
