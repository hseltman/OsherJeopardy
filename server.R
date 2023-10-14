#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define server logic required to draw a histogram
function(input, output, session) {
  
  # File selection
  shinyFileChoose(input, "files", roots = roots, filetype="txt")
  # 
  data <- reactive({
    req(input$files)
    f1 = readLines(input$files)[1]
    print(f1)
    f1
  })
  
  # Add headers to Jeopardy board
  lapply(1:6, function(i) {
    outputId <- paste0("jbs", LETTERS[i])
    output[[outputId]] <- renderText(sjCateg[i])
  })  
  
  # Add headers to Double Jeopardy board
  lapply(1:6, function(i) {
    outputId <- paste0("jbd", LETTERS[i])
    output[[outputId]] <- renderText(djCateg[i])
  })  

  #output$question <- renderText({"No question"})
  output$question <- renderText({data})
}
