install.packages("shiny")

library(shiny)


ui <- fluidPage(
    sliderInput(inputId = "num", label = "Choose a number", value = 25, min = 1, max = 100), # input
    plotOutput("hist") # output to display
    )  

server <- function(input, output) {
    output$hist <- renderPlot({hist(rnorm(input$num))})
    # define my output: output$hist
    # render object = build objects to display
    # access input values: input$num
}

shinyApp(ui = ui, server = server)
