# Load necessary libraries
library(dplyr)

# Read both datasets
df_left <- read.csv("microplastics/Marine Microplastic Concentrations-left.csv", stringsAsFactors = FALSE)
df_right <- read.csv("microplastics/Marine Microplastic Concentrations-right.csv", stringsAsFactors = FALSE)

# Concatenate the datasets
df_combined <- bind_rows(df_left, df_right)

# Remove duplicate rows
df_combined <- distinct(df_combined)

# Save the cleaned dataset (without duplicates)
write.csv(df_combined, "microplastics/Microplastics_Combined_NoDuplicates.csv", row.names = FALSE)

# Convert Latitude & Longitude to numeric
df_combined$Latitude <- as.numeric(df_combined$Latitude)
df_combined$Longitude <- as.numeric(df_combined$Longitude)

# Check for misalignment
summary(df_combined$Latitude)
summary(df_combined$Longitude)

# If Latitude/Longitude are incorrect, replace them with x and y
df_combined <- df_combined %>%
  mutate(
    Latitude = ifelse(is.na(Latitude) | Latitude < -90 | Latitude > 90, y, Latitude),
    Longitude = ifelse(is.na(Longitude) | Longitude < -180 | Longitude > 180, x, Longitude)
  )

# Save the final cleaned dataset
write.csv(df_combined, "microplastics/Microplastics_Cleaned.csv", row.names = FALSE)

print("Data processing complete! Check the saved CSV files.")
