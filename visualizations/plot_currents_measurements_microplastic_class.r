# install.packages("viridis")  # If not installed

library(ggplot2)
library(viridis)

microplastics <- read.csv("microplastics/Microplastics_Cleaned.csv")
currents_binned <- read.csv("currents/currents_by_bins.csv")

ggplot() +
  # First layer: Binned currents dataset (measurement counts)
  geom_point(data = currents_binned, aes(x = lon, y = lat, fill = measurement_count), 
             shape = 21, size = 3, alpha = 0.1, color = "black", stroke = 0.3) +  
  scale_fill_viridis_c(name = "Total Measurements", option = "C") +
  
  # Second layer: Microplastics dataset (concentration class)
  geom_point(data = microplastics, aes(x = Longitude, y = Latitude, color = Concentration.Class), 
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
