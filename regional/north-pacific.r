library(dplyr)
library(ggplot2)


currents <- read.csv("linking/currents_with_microplastics.csv")
# North pacific: Lat: 5-60 / Lon 120-120
currents_north_pacific <- currents %>%
    filter((lon >= -180 & lon <= -120) & lat >=5 & lat <= 40)


ggplot() +
  # First layer: Currents (background layer)
  geom_point(data = currents_north_pacific, aes(x = lon, y = lat, fill = measurement_count), 
             shape = 21, size = 3, alpha = 0.4, color = "black", stroke = 0.3) +  
  scale_fill_viridis_c(name = "Total Measurements", option = "C") +

  # Second layer: Microplastics (plotted using their original coordinates)
  geom_point(data = currents_north_pacific[!is.na(currents_north_pacific$Concentration.Class), ], 
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

currents_north_pacific$Concentration.Class <- recode(currents_north_pacific$Concentration.Class, 
                                          "Very Low" = 1, "Low" = 2, "Medium" = 3, "High" = 4, "Very High" = 5)
correlation <- cor(currents_north_pacific$measurement_count, currents_north_pacific$Concentration.Class, use = "complete.obs")
print(correlation)
cor.test(currents_north_pacific$measurement_count, currents_north_pacific$Concentration.Class)

cor.test(currents_north_pacific$buoy_count, currents_north_pacific$Concentration.Class)
cor.test(currents_north_pacific$speed_sum, currents_north_pacific$Concentration.Class)
cor.test(currents_north_pacific$speed_avg, currents_north_pacific$Concentration.Class)
cor.test(currents_north_pacific$ve_avg, currents_north_pacific$Concentration.Class)
cor.test(currents_north_pacific$vn_avg, currents_north_pacific$Concentration.Class)

ggplot() +
  # First layer: Currents (background layer)
  geom_point(data = currents_north_pacific, aes(x = lon, y = lat, fill = speed_avg), 
             shape = 21, size = 3, alpha = 0.4, color = "black", stroke = 0.3) +  
  scale_fill_viridis_c(name = "Total Measurements", option = "C") +

  # Second layer: Microplastics (plotted using their original coordinates)
  geom_point(data = currents_north_pacific[!is.na(currents_north_pacific$Concentration.Class), ], 
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
