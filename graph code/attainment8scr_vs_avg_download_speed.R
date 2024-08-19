# Load the datasets
school_data <- read.csv("E:/DataScience/Top_10_Recommendations/cleaned data/filtered_school_data.csv")
broadband_data <- read_csv("E:/DataScience/Top_10_Recommendations/cleaned data/merged_broad_band_speed_data.csv")

# Clean and format the data
school_data <- school_data %>%
  mutate(postcode = str_remove_all(postcode, "\\s") %>%  
           toupper())                          

broadband_data <- broadband_data %>%
  toupper())                          

# Step 2: Filter school data for the academic year 2022-2023 and relevant postcodes
school_data_2022 <- school_data %>%
  filter(Year == "2022-2023") %>%
  filter(str_starts(postcode, "BS") | str_starts(postcode, "TR")) %>%
  select(postcode, ATT8SCR)

# Merge datasets on the 'postcode' column
merged_data <- inner_join(school_data_2022, broadband_data, by = "postcode")

# Remove rows with non-numeric ATT8SCR values
cleaned_data <- merged_data %>%
  filter(!is.na(ATT8SCR) & grepl("^-?\\d*\\.?\\d+$", ATT8SCR)) %>%  
  mutate(ATT8SCR = as.numeric(ATT8SCR))

# Perform Linear Modeling
lm_model <- lm(Attainment ~ `Average download speed (Mbit/s)`, data = cleaned_data)

# Get summary statistics
summary_stats <- summary(lm_model)
print(summary_stats)

# Visualize the Data
scatter_plot <- ggplot(cleaned_data, aes(x = `Average download speed (Mbit/s)`, y = Attainment)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "brown") +
  labs(title = "Attainment 8 Score vs Average Download Speed (2022)",
       x = "Average Download Speed (Mbit/s)",
       y = "Attainment 8 Score") +
  theme_minimal()

# Save the plot
print(scatter_plot)

# Print the Coefficients and R-Squared Value
coefficients <- tidy(lm_model)
r_squared <- glance(lm_model)$r.squared

print("Coefficients for Attainment 8 Score vs Average Download Speed:")
print(coefficients)
print(paste("R-squared for Attainment 8 Score vs Average Download Speed:", r_squared))