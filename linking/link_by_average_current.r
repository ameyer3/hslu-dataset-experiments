library(FNN)      
library(dplyr)    
library(ggplot2)  
library(viridis)  

# Load datasets
currents <- read.csv("currents/currents_by_bins.csv")
microplastics <- read.csv("microplastics/Microplastics_Cleaned.csv")

# Find nearest microplastic for each current
nearest_mp <- get.knnx(data = microplastics[, c("Longitude", "Latitude")], 
                       query = currents[, c("lon", "lat")], 
                       k = 1)

# Add microplastic ID to currents
currents$mp_id <- nearest_mp$nn.index  

# Merge so each current row gets the closest microplastic row
currents_matched <- merge(currents, microplastics, 
                          by.x = "mp_id", by.y = "row.names", 
                          all.x = TRUE)

# 🔹 Instead of grouping by Concentration.Class, group by each unique microplastic location
ocean_measurements_avg <- currents_matched %>%
  group_by(mp_id, Longitude, Latitude, Concentration.Class) %>%
  summarise(speed_sum = sum(speed_avg, na.rm = TRUE),
            speed_avg = mean(speed_avg, na.rm = TRUE),
            ve_avg = mean(ve_avg, na.rm = TRUE),
            vn_avg = mean(vn_avg, na.rm = TRUE),
            buoy_count = n_distinct(OBJECTID),
            measurement_count = n(), .groups = "drop")  # <- Keeps unique microplastic rows!

# Save the dataset
write.csv(ocean_measurements_avg, "ocean_measurements_avg.csv", row.names = FALSE)

ggplot() +
  # 🔹 First layer: Binned currents dataset (as a smooth background)
  geom_point(data = ocean_measurements_avg, aes(x = Longitude, y = Latitude, fill = measurement_count), 
             shape = 21, size = 3, alpha = 0.1, color = NA) +  # 🔹 No outline, just background
  scale_fill_viridis_c(name = "Total Measurements", option = "C") +
  
  # 🔹 Second layer: Microplastics dataset (clear points on top)
  geom_point(data = ocean_measurements_avg, aes(x = Longitude, y = Latitude, color = Concentration.Class), 
             alpha = 0.9, size = 1.5) +  # 🔹 More visible microplastic points
  scale_color_manual(name = "Density Class", 
                     values = c("Very Low" = "white", "Low" = "green", 
                                "Medium" = "blue", "High" = "orange", 
                                "Very High" = "red")) +
  
  # Theme and labels
  theme_minimal() +
  labs(title = "Microplastics Overlaid on Averaged Currents",
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "right")
