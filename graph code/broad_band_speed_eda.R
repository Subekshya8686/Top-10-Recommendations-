# Load necessary libraries
library(ggplot2)
library(dplyr)
library(scales) 

# Define the file path
file_path <- "E:/DataScience/Top_10_Recommendations/cleaned data/merged_broad_band_speed_data.csv"

# Read the CSV file
merged_data <- read.csv(file_path, stringsAsFactors = FALSE)
colnames(merged_data)

# Add county information based on postcode_area
merged_data$county <- ifelse(merged_data$postcode_area %in% c("BS"), "Bristol", 
                             ifelse(merged_data$postcode_area %in% c("TR"), "Cornwall", NA))

# Filter out any NA values
filtered_data <- filter(merged_data, !is.na(county))

# Boxplot of Average Download Speeds in Both Counties
ggplot(filtered_data, aes(x = county, y = `average_download_speed_.mbit.s.`, fill = county)) +
  geom_boxplot() +
  labs(title = "Average Download Speeds in Bristol and Cornwall",
       x = "County",
       y = "Average Download Speed (Mbit/s)") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()


# Bar Chart for Bristol
bristol_data <- filter(filtered_data, county == "Bristol")

# Plot
ggplot(bristol_data) +
  geom_bar(aes(x = factor(1), y = average_download_speed_.mbit.s., fill = "Average"), stat = "identity", position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_bar(aes(x = factor(2), y = maximum_download_speed_.mbit.s., fill = "Maximum"), stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Download Speeds in Bristol",
       x = "Metric",
       y = "Speed (Mbit/s)") +
  scale_fill_manual(values = c("Average" = "blue", "Maximum" = "red")) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Bar Chart for Cornwall
cornwall_data <- filter(filtered_data, county == "Cornwall")

# Plot
ggplot(cornwall_data) +
  geom_bar(aes(x = factor(1), y = average_download_speed_.mbit.s., fill = "Average"), stat = "identity", position = position_dodge(width = 0.8), alpha = 0.5) +
  geom_bar(aes(x = factor(2), y = maximum_download_speed_.mbit.s., fill = "Maximum"), stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Download Speeds in Cornwall",
       x = "Metric",
       y = "Speed (Mbit/s)") +
  scale_fill_manual(values = c("Average" = "blue", "Maximum" = "red")) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
