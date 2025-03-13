install.packages("dplyr")
install.packages("geosphere")
install.packages("sf")
library(sf)
library(dplyr)
library(geosphere)

currents <- read.table("buoydata_10001_15000.dat", 
           header=TRUE)
colnames(currents) <- c("id", "something", "time", "date", "lat", "lon", "t", "ve", "vn", "speed", "varlat", "varlon", "vart")

microplastics <- read.csv("microplastics/Microplastics_Cleaned.csv")
microplastics <- subset(microplastics, select = -c(Short.Reference, Long.Reference, DOI, Keywords, Organization, Sampling.Method, NCEI.Accession.Number, OBJECTID, NCEI.Accession.Link) )
colnames(microplastics)
microplastics <- microplastics %>%
  rename(lon = Longitude, lat = Latitude)

summary(currents$lon)
summary(microplastics$lon)
currents <- currents %>%
  filter(lon >= 0 & lon <= 260, lat >= -90 & lat <= 90)
# Convert longitude (lon) to the -180 to 180 range
currents$lon <- ifelse(currents$lon > 180, currents$lon - 360, currents$lon)

microplastics <- microplastics %>%
  filter(lon >= -180 & lon <= 180, lat >= -90 & lat <= 90)
summary(currents$lon)
summary(currents$lat)

summary(microplastics$lon)


# TODO: instead of getting THE nearest one, combine currents rows first and then correlate that
# get_nearest_current <- function(micro_lon, micro_lat, currents) {
#   # Convert current coordinates into a matrix (coordinates of all current points)
#   current_coords <- cbind(currents$lon, currents$lat)
  
#   # Compute distances between current points and the microplastic point
#   distances <- distVincentySphere(current_coords, c(micro_lon, micro_lat))  # Passing a vector for the microplastic
#   # Return the index of the nearest current
#   return(which.min(distances))
# }

# # Apply the function to the microplastics dataset
# microplastics <- microplastics %>%
#   rowwise() %>%
#   mutate(
#     nearest_index = get_nearest_current(lon, lat, currents),
#     nearest_lon = currents$lon[nearest_index],
#     nearest_lat = currents$lat[nearest_index],
#     nearest_speed = currents$speed[nearest_index]  # Adding current speed of the nearest point
#   )
  # Convert to sf objects
currents_sf <- st_as_sf(currents, coords = c("lon", "lat"), crs = 4326)
microplastics_sf <- st_as_sf(microplastics, coords = c("lon", "lat"), crs = 4326)

# Find nearest currents for each microplastic point
nearest_idx <- st_nearest_feature(microplastics_sf, currents_sf)

# Extract nearest data
microplastics <- microplastics %>%
  mutate(
    nearest_lon = currents$lon[nearest_idx],
    nearest_lat = currents$lat[nearest_idx],
    nearest_speed = currents$speed[nearest_idx],
    nearest_ve = currents$ve[nearest_idx],
    nearest_vn = currents$vn[nearest_idx]
  )
colnames(microplastics)

merged_data <- microplastics %>%
  left_join(currents, by = c("nearest_lon" = "lon", "nearest_lat" = "lat"))
nrow(merged_data)


sum(is.na(merged_data$speed)) 
sum(is.na(merged_data$Measurement)) # 3 are NA

correlation <- cor(merged_data$nearest_speed, merged_data$Microplastics.Measurement..density., use = "complete.obs")
print(correlation)
correlation <- cor(merged_data$nearest_ve, merged_data$Microplastics.Measurement..density., use = "complete.obs")
print(correlation)
correlation <- cor(merged_data$nearest_vn, merged_data$Microplastics.Measurement..density., use = "complete.obs")
print(correlation)

library(dplyr)
merged_data$Concentration.Class <- recode(merged_data$Concentration.Class, 
                                          "Very Low" = 1, "Low" = 2, "Medium" = 3, "High" = 4, "Very High" = 5)
correlation <- cor(merged_data$nearest_speed, merged_data$Concentration.Class, use = "complete.obs")
print(correlation)
cor(merged_data$nearest_speed, merged_data$Concentration.Class, use = "complete.obs", method = "spearman")
