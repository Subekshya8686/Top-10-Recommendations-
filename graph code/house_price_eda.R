# libraries
library(ggplot2)
library(dplyr)
library(scales)

filtered_House_Price_data <- read.csv("E:/DataScience/Top_10_Recommendations/cleaned data/filtered_house_price_data.csv")

# Convert 'Date' to Date type and extract year
filtered_House_Price_data$Date <- as.Date(filtered_House_Price_data$Date)
filtered_House_Price_data$Year <- format(filtered_House_Price_data$Date, "%Y")

# Filter data for the year 2022
data_2022 <- filtered_House_Price_data %>%
  filter(Year == "2022")

# Calculate average price per county for 2022
avg_price_2022 <- data_2022 %>%
  group_by(County) %>%
  summarise(Average_Price = mean(Price, na.rm = TRUE))

# Filter data for years 2020 to 2023
data_2020_2023 <- filtered_House_Price_data %>%
  filter(Year %in% c("2020", "2021", "2022", "2023"))

# Calculate average price per year and county
avg_price_yearly <- data_2020_2023 %>%
  group_by(County, Year) %>%
  summarise(Average_Price = mean(Price, na.rm = TRUE))

# Boxplot for average house price in 2022
boxplot_2022 <- ggplot(data_2022, aes(x = County, y = Price)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  labs(title = "Average House Price in 2022 (Boxplot)",
       x = "County",
       y = "Price") +
  theme_minimal()
boxplot_2022

# Save the boxplot to the specified directory
ggsave("E:/DataScience/Top_10_Recommendations/graph/avg_house_price_boxplot.png", plot = boxplot_2022, width = 8, height = 6)

# Bar chart for average house price in 2022
bar_chart_2022 <- ggplot(avg_price_2022, aes(x = County, y = Average_Price, fill = County)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  labs(title = "Average House Price in 2022 (Bar Chart)",
       x = "County",
       y = "Average Price") +
  theme_minimal()
bar_chart_2022

# Save the bar chart to the specified directory
ggsave("E:/DataScience/Top_10_Recommendations/graph/avg_house_price_barchart_2022.png", plot = bar_chart_2022, width = 8, height = 6)

# Line chart for average house price from 2020 to 2023
line_chart_2020_2023 <- ggplot(avg_price_yearly, aes(x = Year, y = Average_Price, color = County, group = County)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  labs(title = "Average House Price from 2020 to 2023",
       x = "Year",
       y = "Average Price") +
  theme_minimal()
line_chart_2020_2023

# Save the line chart to the specified directory
ggsave("E:/DataScience/Top_10_Recommendations/graph/avg_house_price_2020_2023.png", plot = line_chart_2020_2023, width = 8, height = 6)