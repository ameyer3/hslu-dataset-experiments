library(shiny)
library(ggplot2)
library(viridis)
library(dplyr)

# Load the dataset
currents_and_microplastics <- read.csv("linking/currents_with_microplastics.csv")

# Define oceanic regions
filter_region <- function(data, region) {
  switch(region,
         "North Atlantic" = data %>% filter(lon >= -80 & lon <= -20 & lat >= 10 & lat <= 70),
         "Mediterranean" = data %>% filter(lon >= -10 & lon <= 40 & lat >= 30 & lat <= 45),
         "North Pacific" = data %>% filter((lon >= -180 & lon <= -120) & lat >= 5 & lat <= 40),
         "Northern North Atlantic" = data %>% filter(lon >= -80 & lon <= -5 & lat >= 50 & lat <= 80),
         "Mid Atlantic (Upper South America)" = data %>% filter(lon >= -60 & lon <= -20 & lat >= -5 & lat <= 10),
         "South Atlantic" = data %>% filter(lon >= -80 & lon <= 20 & lat >= -60 & lat <= -10),
         data) # Default: return all data
}

# Define function to generate the plot
plot_ocean_measurements <- function(data, fill_var, title, fill_label) {
  ggplot() +
    geom_point(data = data, aes(x = lon, y = lat, fill = !!sym(fill_var)), 
               shape = 21, size = 3, alpha = 0.4, color = "black", stroke = 0.3) +  
    scale_fill_viridis_c(name = fill_label, option = "C") +
    
    geom_point(data = data[!is.na(data$Concentration.Class), ], 
               aes(x = mp_lon, y = mp_lat, color = Concentration.Class), 
               alpha = 0.7, size = 1) +  
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
                  selected = "measurement_count"),
      
      selectInput("selected_region", "Select Region:", 
                  choices = c("North Atlantic", "Mediterranean", "North Pacific",
                              "Northern North Atlantic", "Mid Atlantic (Upper South America)",
                              "South Atlantic")),
      
      actionButton("plot_button", "Show Map & Correlation")
    ),
    
    mainPanel(
      plotOutput("ocean_plot"),
      verbatimTextOutput("correlation_results")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  filtered_currents <- eventReactive(input$plot_button, {
    filter_region(currents_and_microplastics, input$selected_region)
  })
  
  output$ocean_plot <- renderPlot({
    req(input$plot_button)  # Wait until button is clicked
    data <- filtered_currents()
    plot_ocean_measurements(data, input$selected_var, 
                            paste(input$selected_region, "-", input$selected_var), 
                            input$selected_var)
  })
  
  output$correlation_results <- renderPrint({
    req(input$plot_button)  # Wait until button is clicked
    data <- filtered_currents()
    
    if (!("Concentration.Class" %in% colnames(data)) || all(is.na(data$Concentration.Class))) {
      return("No microplastic concentration data available for this region.")
    }
    
    # Convert Concentration.Class to numeric
    data$Concentration.Class <- factor(data$Concentration.Class, 
                                       levels = c("Very Low", "Low", "Medium", "High", "Very High"),
                                       labels = 1:5)
    
    # Perform correlation test
    cor_test <- cor.test(data[[input$selected_var]], as.numeric(data$Concentration.Class))
    
    # Print results
    cat("Correlation:", round(cor_test$estimate, 3), "\n",
        "T-value:", round(cor_test$statistic, 2), "\n",
        "Degrees of Freedom:", cor_test$parameter, "\n",
        "P-value:", format.pval(cor_test$p.value, digits = 3, eps = 0.001), "\n",
        "95% CI: [", round(cor_test$conf.int[1], 3), ", ", round(cor_test$conf.int[2], 3), "]\n")
  })
}

# Run App
shinyApp(ui = ui, server = server)
