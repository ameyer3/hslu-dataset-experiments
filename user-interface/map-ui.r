library(shiny)
library(ggplot2)
library(viridis)

microplastics <- read.csv("microplastics/Microplastics_Cleaned.csv")
currents <- read.csv("currents/currents_by_bins.csv")

# Define function to generate the plot
plot_ocean_measurements <- function(fill_var, title, fill_label) {
  ggplot() +
    # First layer: Currents (background layer)
    geom_point(data = currents, aes(x = lon, y = lat, fill = !!sym(fill_var)), 
               shape = 21, size = 3, alpha = 0.1, color = "black", stroke = 0.3) +  
    scale_fill_viridis_c(name = fill_label, option = "C") +
    
    # Second layer: Microplastics (plotted using their original coordinates)
    geom_point(data = microplastics[!is.na(microplastics$Concentration.Class), ], 
               aes(x = Longitude, y = Latitude, color = Concentration.Class), 
               alpha = 0.7, size = 0.7) +  
    scale_color_manual(name = "Density Class", 
                       values = c("Very Low" = "white", "Low" = "green", 
                                  "Medium" = "blue", "High" = "orange", 
                                  "Very High" = "red")) +

    theme_minimal() +
    labs(title = title, x = "Longitude", y = "Latitude") +
    theme(legend.position = "right")
}

# Shiny UI
ui <- fluidPage(
  titlePanel("Microplastics & Ocean Currents"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_var", "Select Attribute:", 
                  choices = list("Measurement Count" = "measurement_count",
                                 "Speed Sum" = "speed_sum",
                                 "Speed Average" = "speed_avg",
                                 "Ve Average" = "ve_avg",
                                 "Vn Average" = "vn_avg",
                                 "Buoy Count" = "buoy_count"),
                  selected = "measurement_count")
    ),
    
    mainPanel(
      plotOutput("ocean_plot")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  output$ocean_plot <- renderPlot({
    plot_ocean_measurements(input$selected_var, 
                            paste("Microplastics Overlaid on", input$selected_var), 
                            input$selected_var)
  })
}

# Run App
shinyApp(ui = ui, server = server)
