#' Suggestion Button Namer
#'
#' programmatic naming scheme to ensures consistent calls in button generation and listeners
#' @keywords internal
sugg_butt_name = function(suggestion) { paste("sugg", suggestion, sep='_') }

#' Suggestion Button Generator
#'
#' Generate a single dynamic suggestion button
#' @keywords internal
sugg_butt_make = function(suggestion) {
  shiny::actionButton(sugg_butt_name(suggestion), label=suggestion)
}

#' Suggestion Button Observer Generator
#'
#' Generate a listener for a single dynamically generated suggestion button
#' @keywords internal
sugg_butt_listen = function(input, output, session, suggestion) {
  shiny::observeEvent(input[[sugg_butt_name(suggestion)]],{
    shiny::updateTextInput(
      session, 'usertext',
      value = paste(input$usertext, suggestion)
    )
  })
}


#' NextWordR User Interface Definition
#' @keywords internal
NextWordR_ui =
  shiny::fluidPage(
    shiny::titlePanel("Next Word Text Prediction"),
    shiny::mainPanel(
      shiny::fluidRow(
        shiny::textAreaInput(
          'usertext', NULL, placeholder = "type or click",
          width = 400, height = 150
        )
      ),
      shiny::fluidRow(shiny::uiOutput('buttons'))
    )
  )


#' NextWordR Server Function Definition
#' @keywords internal
NextWordR_server = function(input, output, session) {
  shiny::observeEvent(input$usertext, {  # Watch for a change in user text
    suggestions = next_word(input$usertext)  # generate new suggestion based upon user text

    output$buttons = shiny::renderUI({  # generate new buttons for suggestions
      htmltools::div(
        lapply(suggestions, sugg_butt_make)
      )
    })

    lapply(suggestions, function(x) {  # generate listeners for new buttons
      sugg_butt_listen(input, output, session, suggestion = x)
    })

  })
}

#' Run NextWordR App
#'
#' Call UI and server definitions included in package.
#' @export
nextword_app = function() {
  shiny::shinyApp(ui = NextWordR_ui, server = NextWordR_server)
}
