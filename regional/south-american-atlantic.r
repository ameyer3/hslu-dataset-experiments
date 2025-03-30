# microplastic no currents
library(dplyr)
library(ggplot2)

currents <- read.csv("linking/currents_with_microplastics.csv")
currents_mid_atlantic_upper_south_america <- currents %>%
    filter(lon >= -60 & lon <= -20 & lat >= -5 & lat <= 10)

ggplot() +
  # First layer: Currents (background layer)
  geom_point(data = currents_mid_atlantic_upper_south_america, aes(x = lon, y = lat, fill = measurement_count), 
             shape = 21, size = 3, alpha = 0.4, color = "black", stroke = 0.3) +  
  scale_fill_viridis_c(name = "Total Measurements", option = "C") +

  # Second layer: Microplastics (plotted using their original coordinates)
  geom_point(data = currents_mid_atlantic_upper_south_america[!is.na(currents_mid_atlantic_upper_south_america$Concentration.Class), ], 
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

currents_mid_atlantic_upper_south_america$Concentration.Class <- recode(currents_mid_atlantic_upper_south_america$Concentration.Class, 
                                          "Very Low" = 1, "Low" = 2, "Medium" = 3, "High" = 4, "Very High" = 5)
cor.test(currents_mid_atlantic_upper_south_america$measurement_count, currents_mid_atlantic_upper_south_america$Concentration.Class)
cor.test(currents_mid_atlantic_upper_south_america$buoy_count, currents_mid_atlantic_upper_south_america$Concentration.Class)
cor.test(currents_mid_atlantic_upper_south_america$speed_sum, currents_mid_atlantic_upper_south_america$Concentration.Class)
cor.test(currents_mid_atlantic_upper_south_america$speed_avg, currents_mid_atlantic_upper_south_america$Concentration.Class)
cor.test(currents_mid_atlantic_upper_south_america$ve_avg, currents_mid_atlantic_upper_south_america$Concentration.Class)
cor.test(currents_mid_atlantic_upper_south_america$vn_avg, currents_mid_atlantic_upper_south_america$Concentration.Class)
