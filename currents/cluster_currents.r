library(hexbin)
library(dplyr)

currents <- read.table("buoydata_15001_jul24.dat",
           header=TRUE)
colnames(currents) <- c("id", "something", "time", "date", "lat", "lon", "t", "ve", "vn", "speed", "varlat", "varlon", "vart")
currents <- subset(currents, select = -c(something, varlat, varlon, vart) )

summary(currents$lon)

# Convert longitude (lon) to the -180 to 180 range
currents$lon <- ifelse(currents$lon > 180, currents$lon - 360, currents$lon)
summary(currents$lon)
currents <- currents %>%
  filter(lon >= -180 & lon <= 180, lat >= -90 & lat <= 90)
summary(currents$lon)
currents <- currents %>%
  filter(ve < 999, vn < 999, speed < 999)
colnames(currents)

install.packages("FNN")

library(FNN)  # Fast Nearest Neighbors

# Define number of bins
num_bins <- 500

# Create hexagonal bins
hex_bins <- hexbin(currents$lon, currents$lat, xbins = num_bins)

# Extract bin centers
bin_centers <- data.frame(
  bin_id = 1:length(hex_bins@count),  # Unique bin IDs
  lon = hex_bins@xcm,  # Bin center longitude
  lat = hex_bins@ycm   # Bin center latitude
)

# Use nearest neighbor search to match each data point to a bin center
nearest_bins <- get.knnx(data = bin_centers[, c("lon", "lat")], 
                         query = currents[, c("lon", "lat")], 
                         k = 1)

# Assign bin ID to each data point
currents$bin_id <- bin_centers$bin_id[nearest_bins$nn.index]

# # Aggregate speed sums for each bin
# speed_sums <- currents %>%
#   group_by(bin_id) %>%
#   summarise(speed = sum(speed, na.rm = TRUE), .groups = "drop")

# # Merge speed sums with bin centers
# currents_binned <- left_join(bin_centers, speed_sums, by = "bin_id")

# # Fill missing speed values (bins with no data)
# currents_binned$speed[is.na(currents_binned$speed)] <- 0

# Aggregate by bin
currents_binned <- currents %>%
  group_by(bin_id) %>%
  summarise(
    speed_sum = sum(speed, na.rm = TRUE), 
    speed_avg = mean(speed, na.rm = TRUE), 
    ve_avg = mean(ve, na.rm = TRUE), 
    vn_avg = mean(vn, na.rm = TRUE), 
    buoy_count = n_distinct(id),  # Unique buoys per bin
    measurement_count = n()  # Total data points per bin
  ) %>%
  ungroup()

# Merge with bin centers
currents_binned <- left_join(bin_centers, currents_binned, by = "bin_id")

# Fill missing values (bins with no data)
currents_binned[is.na(currents_binned)] <- 0


# Plot the binned data
ggplot(currents_binned, aes(x = lon, y = lat, color = speed_sum)) +
  geom_point(size = 2) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Binned Current Speed Distribution",
       x = "Longitude", y = "Latitude", color = "Total Speed")

ggplot(currents_binned, aes(x = lon, y = lat, color = speed_avg)) +
  geom_point(size = 2) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Binned Current Speed Distribution",
       x = "Longitude", y = "Latitude", color = "Average Speed")

ggplot(currents_binned, aes(x = lon, y = lat, color = ve_avg)) +
  geom_point(size = 2) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Binned Current Speed Distribution",
       x = "Longitude", y = "Latitude", color = "Avg ve")


ggplot(currents_binned, aes(x = lon, y = lat, color = vn_avg)) +
  geom_point(size = 2) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Binned Current Speed Distribution",
       x = "Longitude", y = "Latitude", color = "Avg vn")


ggplot(currents_binned, aes(x = lon, y = lat, color = buoy_count)) +
  geom_point(size = 2) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Binned Current Speed Distribution",
       x = "Longitude", y = "Latitude", color = "Total Buoys")


ggplot(currents_binned, aes(x = lon, y = lat, color = measurement_count)) +
  geom_point(size = 2) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "Binned Current Speed Distribution",
       x = "Longitude", y = "Latitude", color = "Total measurements")

write.csv(currents_binned, "currents_by_bins.csv", row.names = FALSE)
