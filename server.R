#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyalert)


# Define server logic required to draw a histogram
function(input, output, session) {
  
  # File selection
  shinyFileChoose(input, "inputFile", roots = roots, session=session, filetype="txt")
  
  gameName = reactive({
    req(input$inputFile)
    parseFilePaths(roots, input$inputFile)$datapath
  })
  
  gameData <- reactive({
    fName = gameName()
    req(fName)
    inData = readLines(fName)
    if (length(inData) != (1+5)*6*2+1) {
        shinyalert("Bad input",
                 paste("File must contain", (1+5)*6*2+1, "lines"),
                 type="error")
      return("Bad Game Data")
    } else {
      return(inData)
    }
    inData
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
  
  output$gameNameText <- renderText({paste("Game:", gameName())})

  #output$question <- renderText({"No question"})
  output$question <- renderText({gameData()[1]})
}
