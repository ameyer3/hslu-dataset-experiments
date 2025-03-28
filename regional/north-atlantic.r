library(dplyr)
library(ggplot2)

currents <- read.csv("linking/currents_with_microplastics.csv")
currents_north_atlantic <- currents %>%
    filter(lon >= -80 & lon <= -20 & lat >= 10 & lat <= 70)

ggplot() +
  # First layer: Currents (background layer)
  geom_point(data = currents_north_atlantic, aes(x = lon, y = lat, fill = measurement_count), 
             shape = 21, size = 3, alpha = 0.4, color = "black", stroke = 0.3) +  
  scale_fill_viridis_c(name = "Total Measurements", option = "C") +

  # Second layer: Microplastics (plotted using their original coordinates)
  geom_point(data = currents_north_atlantic[!is.na(currents_north_atlantic$Concentration.Class), ], 
             aes(x = mp_lon, y = mp_lat, color = Concentration.Class), 
             alpha = 0.7, size = 1) +  
  scale_color_manual(name = "Density Class", 
                     values = c("Very Low" = "white", "Low" = "green", 
                                "Medium" = "blue", "High" = "black", 
                                "Very High" = "red")) +

  # Theme and labels
  theme_minimal() +
  labs(title = "Microplastics Overlaid on Binned Currents",
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "right")

currents_north_atlantic$Concentration.Class <- recode(currents_north_atlantic$Concentration.Class, 
                                          "Very Low" = 1, "Low" = 2, "Medium" = 3, "High" = 4, "Very High" = 5)
cor.test(currents_north_atlantic$measurement_count, currents_north_atlantic$Concentration.Class)
cor.test(currents_north_atlantic$buoy_count, currents_north_atlantic$Concentration.Class)
cor.test(currents_north_atlantic$speed_sum, currents_north_atlantic$Concentration.Class)
cor.test(currents_north_atlantic$speed_avg, currents_north_atlantic$Concentration.Class)
cor.test(currents_north_atlantic$ve_avg, currents_north_atlantic$Concentration.Class)
cor.test(currents_north_atlantic$vn_avg, currents_north_atlantic$Concentration.Class)
