currents_newest <- read.table("buoydata_10001_15000.dat", 
           header=TRUE)
head(currents, 5)
nrow(currents)
colnames(currents) <- c("id", "", "time", "date", "lat", "lon", "t", "ve", "vn", "speed", "varlat", "varlon", "vart")
colnames(currents)
head(currents, 5)


install.packages('ggplot2')
library(ggplot2)

install.packages("hexbin")
ggplot(currents, aes(x = lon, y = lat)) +
  geom_hex(bins = 100) +  # Adjust bins for resolution
  theme_minimal()
