library(htmltools)
library(shiny)
library(NextWordR)

# Come up with a programmatic naming scheme to ensure consistent calls in
# dynamic button generation and listeing
sugg_butt_name = function(suggestion) { paste("sugg", suggestion, sep='_') }

# Generate a single dynamic suggestion button
sugg_butt_make = function(suggestion) {
  actionButton(sugg_butt_name(suggestion), label=suggestion)
}

# Generate a listener for a single dynamic suggestion button
sugg_butt_listen = function(input, output, session, suggestion) {
  observeEvent(input[[sugg_butt_name(suggestion)]],{
    updateTextInput(
      session, 'usertext',
      value = paste(input$usertext, suggestion)
    )
  })
}

ui =
  fluidPage(
    titlePanel("Next Word Text Prediction"),
      mainPanel(
        fluidRow(textInput('usertext', NULL, placeholder = "enter your text to see suggestions")),
        fluidRow(uiOutput('buttons'))
      )
    )


server = function(input, output, session) {
  ntext = eventReactive(input$goButton, {
    input$n
  })

  # Watch for a change in user text
  observeEvent(input$usertext, {
    suggestions = nextword(input$usertext)  # generate new suggestion based upon user text

    output$buttons = renderUI({  # generate new buttons for suggestions
      div(
        lapply(suggestions, sugg_butt_make)
      )
    })

    lapply(suggestions, function(x) {  # generate listeners for new buttons
      sugg_butt_listen(input, output, session, suggestion = x)
    })

  })
}

shinyApp(ui = ui, server = server)
