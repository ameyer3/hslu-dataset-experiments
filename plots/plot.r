install.packages("ggplot2")  # If not installed
install.packages("hexbin")   # Useful for big datasets

library(ggplot2)
library(hexbin)


currents <- read.table("buoydata_10001_15000.dat", 
           header=TRUE)
colnames(currents) <- c("id", "something", "time", "date", "lat", "lon", "t", "ve", "vn", "speed", "varlat", "varlon", "vart")

microplastics <- read.csv("microplastics/Microplastics_Cleaned.csv")
# microplastics <- subset(microplastics, select = -c(Short.Reference, Long.Reference, DOI, Keywords, Organization, Sampling.Method, Accession.Number, GlobalID, Accession.Link) )

microplastics <- microplastics %>%
  rename(lon = Longitude, lat = Latitude)

# currents <- currents %>%
#   filter(lon >= -180 & lon <= 180, lat >= -90 & lat <= 90)
microplastics <- microplastics %>%
  filter(lon >= -180 & lon <= 180, lat >= -90 & lat <= 90)

colnames(microplastics)
head(microplastics, 5)
ggplot(microplastics, aes(x = lon, y = lat, color = Microplastics.Measurement..density.)) +
  geom_point(alpha = 0.7, size = 1) +  # Adjust transparency & size
  scale_color_viridis_c() +  # Better color scale for visibility
  theme_minimal() +
  labs(title = "Microplastics Distribution",
       x = "Longitude", y = "Latitude", color = "Measurement Level")

ggplot(microplastics, aes(x = lon, y = lat, color = Concentration.Class)) +
  geom_point(alpha = 0.7, size = 1) +  # Adjust transparency & size
#   scale_color_manual(values = c("Low" = "blue", "Medium" = "orange", "High" = "red")) +  # Custom colors
  theme_minimal() +
  labs(title = "Microplastics Distribution",
       x = "Longitude", y = "Latitude", color = "Density Class")



ggplot(currents, aes(x = lon, y = lat, color = speed)) +
  geom_hex(bins = 100) +  # Use hexagonal binning to handle large datasets
  scale_fill_viridis_c() + 
  theme_minimal() +
  labs(title = "Current Speed Distribution",
       x = "Longitude", y = "Latitude", fill = "Current Speed")

ggplot(currents, aes(x = lon, y = lat, color = ve)) +
  geom_hex(bins = 100) +  # Use hexagonal binning to handle large datasets
  scale_fill_viridis_c() + 
  theme_minimal() +
  labs(title = "Current ve Distribution",
       x = "Longitude", y = "Latitude", fill = "Current ve")

ggplot(currents, aes(x = lon, y = lat, color = vn)) +
  geom_hex(bins = 100) +  # Use hexagonal binning to handle large datasets
  scale_fill_viridis_c() + 
  theme_minimal() +
  labs(title = "Current vn Distribution",
       x = "Longitude", y = "Latitude", fill = "Current VN")
