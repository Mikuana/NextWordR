library(shiny)
library(htmltools)
library(NextWordR)

ui =
  fluidPage(
    titlePanel("Next Word Text Prediction"),
      mainPanel(
        fluidRow(textInput('usertext', NULL, placeholder = "enter your text to see suggestions")),
        fluidRow(
          uiOutput('buttons')
        )
      )
    )

# Define server logic required to draw a histogram
server = function(input, output) {
  ntext = eventReactive(input$goButton, {
    input$n
  })

   output$buttons = renderUI({
     suggestions = nextword(input$usertext)
     div(
        lapply(suggestions, function(x) {
          actionButton(inputId = x, label = x)
        })
     )
   })
}

# Run the application
shinyApp(ui = ui, server = server)
