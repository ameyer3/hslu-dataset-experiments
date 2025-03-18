library(ggplot2)  # For plotting
library(viridis)  # For color scale

plot_ocean_measurements <- function(data, fill_var, title, fill_label) {
  ggplot() +
    # First layer: Currents (background layer)
    geom_point(data = data, aes(x = lon, y = lat, fill = !!sym(fill_var)), 
               shape = 21, size = 3, alpha = 0.4, color = "black", stroke = 0.3) +  
    scale_fill_viridis_c(name = fill_label, option = "C") +

    # Second layer: Microplastics (plotted using their original coordinates)
    geom_point(data = data[!is.na(data$Concentration.Class), ], 
               aes(x = mp_lon, y = mp_lat, color = Concentration.Class), 
               alpha = 0.7, size = 1) +  
    scale_color_manual(name = "Density Class", 
                       values = c("Very Low" = "white", "Low" = "green", 
                                  "Medium" = "blue", "High" = "orange", 
                                  "Very High" = "red")) +

    # Theme and labels
    theme_minimal() +
    labs(title = title, x = "Longitude", y = "Latitude") +
    theme(legend.position = "right")
}

# Generate plots for all attributes
plot_list <- list(
  plot_ocean_measurements(currents, "measurement_count", "Microplastics Overlaid on Measurement Count", "Total Measurements"),
  plot_ocean_measurements(currents, "speed_sum", "Microplastics Overlaid on Speed Sum", "Speed Sum"),
  plot_ocean_measurements(currents, "speed_avg", "Microplastics Overlaid on Speed Average", "Speed Average"),
  plot_ocean_measurements(currents, "ve_avg", "Microplastics Overlaid on Ve Average", "Ve Average"),
  plot_ocean_measurements(currents, "vn_avg", "Microplastics Overlaid on Vn Average", "Vn Average"),
  plot_ocean_measurements(currents, "buoy_count", "Microplastics Overlaid on Buoy Count", "Buoy Count")
)

# Display all plots
library(gridExtra)
grid.arrange(grobs = plot_list, ncol = 2)

plot_ocean_measurements(currents, "measurement_count", "Microplastics Overlaid on Measurement Count", "Total Measurements")
plot_ocean_measurements(currents, "speed_sum", "Microplastics Overlaid on Speed Sum", "Speed Sum")
plot_ocean_measurements(currents, "speed_avg", "Microplastics Overlaid on Speed Average", "Speed Average")
plot_ocean_measurements(currents, "ve_avg", "Microplastics Overlaid on Ve Average", "Ve Average")
plot_ocean_measurements(currents, "vn_avg", "Microplastics Overlaid on Vn Average", "Vn Average")
plot_ocean_measurements(currents, "buoy_count", "Microplastics Overlaid on Buoy Count", "Buoy Count")

