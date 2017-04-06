library(htmltools)
library(shiny)
library(NextWordR)

# programmatic naming scheme to ensures consistent calls in button generation and listeners
sugg_butt_name = function(suggestion) { paste("sugg", suggestion, sep='_') }

# Generate a single dynamic suggestion button
sugg_butt_make = function(suggestion) {
  actionButton(sugg_butt_name(suggestion), label=suggestion)
}

# Generate a listener for a single dynamically generated suggestion button
sugg_butt_listen = function(input, output, session, suggestion) {
  observeEvent(input[[sugg_butt_name(suggestion)]],{
    updateTextInput(
      session, 'usertext',
      value = paste(input$usertext, suggestion)
    )
  })
}


# Start UI definition
ui =
  fluidPage(
    titlePanel("Next Word Text Prediction"),
      mainPanel(
        fluidRow(
          textAreaInput(
            'usertext', NULL, placeholder = "type or click",
            width = 400, height = 150
          )
        ),
        fluidRow(uiOutput('buttons'))
      )
    )


# Start Server Definition
server = function(input, output, session) {
  ntext = eventReactive(input$goButton, {
    input$n
  })

  observeEvent(input$usertext, {  # Watch for a change in user text
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
