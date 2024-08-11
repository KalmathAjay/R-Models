library(shiny)

ui <- fluidPage(
  titlePanel("BMI Calculator"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("height", "Height (cm):", min = 20, max = 250, value = 175),
      sliderInput("weight", "Weight (kg):", min = 20, max = 250, value = 50),
      actionButton("calculate", "Calculate BMI", class = "btn-primary")
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$calculate, {
    bmi <- input$weight / ((input$height / 100) ^ 2)
    output$result <- renderText(paste("Your BMI is:", round(bmi, 2)))
    cat("Input - Height:", input$height, "cm, Weight:", input$weight, "kg\n")
    cat("Output - BMI:", round(bmi, 2), "\n")
  })
}

shinyApp(ui = ui, server = server)
