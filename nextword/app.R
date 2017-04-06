library(htmltools)
library(shiny)
library(NextWordR)

# Suggestion button module UI component
ab_moduleUI = function(id){
  ns = NS(id)
  tagList(
    fluidRow(
      actionButton(ns("btn"), id) #,
      # textOutput(ns("txt"))
    )
  )
}

# Suggestion button module server component
ab_module = function(input, output, session){
  observeEvent(input$btn,{
    output$txt = renderText("More information shown")
  })
}


ui =
  fluidPage(
    titlePanel("Next Word Text Prediction"),
      mainPanel(
        fluidRow(textInput('usertext', NULL, placeholder = "enter your text to see suggestions")),
        uiOutput('buttons')
      )
    )


server = function(input, output) {
  ntext = eventReactive(input$goButton, {
    input$n
  })

  observeEvent(input$usertext, {
    suggestions = nextword(input$usertext)

    output$buttons = renderUI({
      div(
        lapply(suggestions, ab_moduleUI)
      )
    })

    lapply(suggestions, function(x) callModule(ab_module, x))
  })
}

shinyApp(ui = ui, server = server)
