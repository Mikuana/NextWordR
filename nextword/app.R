library(shiny)
source(file.path('R', 'misc.R'))

ui =
  fluidPage(
    titlePanel("Next Word Text Prediction"),
      mainPanel(
        fluidRow(textInput('usertext', NULL, placeholder = "enter your text to see suggestions")),
        fluidRow(textOutput('suggestions'))

      )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$suggestions <- renderText({
     input$usertext
   })
}

# Run the application
shinyApp(ui = ui, server = server)


# library(shiny)
# library(miniUI)
#
# nextword_app = function() {
#   ui =
#     miniPage(
#       gadgetTitleBar("Text Prediction"),
#       fillCol(
#         miniContentPanel(
#           textInput('usertext', label=NULL, value = "", placeholder = 'write your text')
#         ),
#         miniContentPanel(
#           textOutput('suggestion')
#         )
#       )
#     )
#
#   server = function(input, output, session) {
#     output$suggestion =  renderText({
#       nextword(input$usertext)
#     })
#   }
#
#   runGadget(ui, server)
# }
#
# nextword_app()
# ```
