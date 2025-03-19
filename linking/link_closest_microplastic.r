library(FNN)      # For kNN nearest neighbor search
library(dplyr)    # For data manipulation
library(ggplot2)  # For plotting
library(viridis)  # For color scale

# Load datasets
currents <- read.csv("currents/currents_by_bins.csv")
microplastics <- read.csv("microplastics/Microplastics_Cleaned.csv")

# Find nearest microplastic for each currents row
nearest_mp <- get.knnx(data = microplastics[, c("Longitude", "Latitude")], 
                       query = currents[, c("lon", "lat")], 
                       k = 1)

# Add the matched microplastic data to currents
currents$Concentration.Class <- microplastics$Concentration.Class[nearest_mp$nn.index]
currents$mp_lon <- microplastics$Longitude[nearest_mp$nn.index]
currents$mp_lat <- microplastics$Latitude[nearest_mp$nn.index]

# Save the updated dataset
write.csv(currents, "currents_with_microplastics.csv", row.names = FALSE)

# Plot the combined dataset
ggplot() +
  # First layer: Currents (background layer)
  geom_point(data = currents, aes(x = lon, y = lat, fill = measurement_count), 
             shape = 21, size = 3, alpha = 0.4, color = "black", stroke = 0.3) +  
  scale_fill_viridis_c(name = "Total Measurements", option = "C") +

  # Second layer: Microplastics (plotted using their original coordinates)
  geom_point(data = currents[!is.na(currents$Concentration.Class), ], 
             aes(x = mp_lon, y = mp_lat, color = Concentration.Class), 
             alpha = 0.7, size = 1) +  
  scale_color_manual(name = "Density Class", 
                     values = c("Very Low" = "white", "Low" = "green", 
                                "Medium" = "blue", "High" = "orange", 
                                "Very High" = "red")) +

  # Theme and labels
  theme_minimal() +
  labs(title = "Microplastics Overlaid on Binned Currents",
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "right")
