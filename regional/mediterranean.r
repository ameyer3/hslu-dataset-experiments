# show that a lot of trash and measurements -> both are kinda trapped

library(dplyr)
library(ggplot2)


currents <- read.csv("linking/currents_with_microplastics.csv")

currents_mediterranean <- currents %>%
    filter(lon >= -10 & lon <= 40 & lat >= 30 & lat <= 45)


ggplot() +
  # First layer: Currents (background layer)
  geom_point(data = currents_mediterranean, aes(x = lon, y = lat, fill = measurement_count), 
             shape = 21, size = 3, alpha = 0.4, color = "black", stroke = 0.3) +  
  scale_fill_viridis_c(name = "Total Measurements", option = "C") +

  # Second layer: Microplastics (plotted using their original coordinates)
  geom_point(data = currents_mediterranean[!is.na(currents_mediterranean$Concentration.Class), ], 
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

currents_mediterranean$Concentration.Class <- recode(currents_mediterranean$Concentration.Class, 
                                          "Very Low" = 1, "Low" = 2, "Medium" = 3, "High" = 4, "Very High" = 5)
cor.test(currents_mediterranean$measurement_count, currents_mediterranean$Concentration.Class)
cor.test(currents_mediterranean$buoy_count, currents_mediterranean$Concentration.Class)
cor.test(currents_mediterranean$speed_sum, currents_mediterranean$Concentration.Class)
cor.test(currents_mediterranean$speed_avg, currents_mediterranean$Concentration.Class)
cor.test(currents_mediterranean$ve_avg, currents_mediterranean$Concentration.Class)
cor.test(currents_mediterranean$vn_avg, currents_mediterranean$Concentration.Class)
