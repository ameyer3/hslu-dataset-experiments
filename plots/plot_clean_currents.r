install.packages("ggplot2")  # If not installed

library(ggplot2)

currents_location <- read.csv("currents_by_location.csv")
currents_time <- read.csv("currents_by_buoy_time.csv")

#weird gap around americas but speed hotspots visible
ggplot(currents_location, aes(x = lon, y = lat, color = speed)) +
  geom_point(alpha = 0.7, size = 1) +  # Adjust transparency & size
#   scale_color_manual(values = c("Low" = "blue", "Medium" = "orange", "High" = "red")) +  # Custom colors
  theme_minimal() +
  labs(title = "Microplastics Distribution",
       x = "Longitude", y = "Latitude", color = "Density Class")

# this gets quite splotchy is it maybe still wrong? The logic?
# and gap around americas is still possible, some hotspots are visible as well
ggplot(currents_time, aes(x = lon, y = lat, color = speed)) +
  geom_point(alpha = 0.7, size = 1) +  # Adjust transparency & size
#   scale_color_manual(values = c("Low" = "blue", "Medium" = "orange", "High" = "red")) +  # Custom colors
  theme_minimal() +
  labs(title = "Microplastics Distribution",
       x = "Longitude", y = "Latitude", color = "Density Class")
