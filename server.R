#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(shinydashboard)
library(shinythemes)
library(shinyFiles)
library(shinyalert)
#library(shinyjs)


# Define server logic required to draw a histogram
function(input, output, session) {
  
  # File selection
  shinyFileChoose(input, "inputFile", roots = roots, session=session, filetype="txt")
  
  gameName = reactive({
    req(input$inputFile)
    parseFilePaths(roots, input$inputFile)$datapath
  })
  
  # When input file name changes, read in new game board data
  gameData <- reactive({
    req(gameName())
    fName = gameName()
    # read data and delete blank lines and comment lines
    inData = readLines(fName)
    inData = inData[!grepl("(^\\s*$)|(^\\s*#)", inData)]
    
    if (length(inData) < 12) {
      shinyalert("Bad input",
                 paste("File must contain 12 sets of 6 lines each",
                       "(where line 1 is a Category Name, and lines",
                       "2 to 5 are 'answer|question' pairs), followed",
                       "by a Final Jeopardy Category, and then a Final",
                       "Jeopardy 'answer|question' pair."),
                 type="error")
      return("Bad Game Data")
    } 
    
    # Find which lines have a single bar
    singleBar = grepl("^([^|]+?)[|]([^|]+)$", inData)

    # Handle correct pattern
    if (length(singleBar) == length(sbPattern) && all(singleBar==sbPattern)) {
      sjCatLoc = seq(1, by=6, length.out=6)
      sjCategories = inData[sjCatLoc]
      djCatLoc = seq(37, by=6, length.out=6)
      dfCategories = inData[djCatLoc]
      fjCategory = inData[73]
      tc = textConnection(inData[1:36][-sjCatLoc])
      sjAQ = read.table(tc, sep="|", quote="", col.names=c("Answer", "Question"))
      close(tc)
      tc = textConnection(inData[37:72][-sjCatLoc]) # [sic]
      djAQ = read.table(tc, sep="|", quote="", col.names=c("Answer", "Question"))
      close(tc)
      temp = strsplit(inData[74], "\\|")[[1]]
      fjAQ = data.frame(Answer=temp[1], Question=temp[2])
      return("Data Parsed Successfully")    
    }
    
    # Handle first part of pattern not 'FTTTTTF'
    if (singleBar[1]) {
      shinyalert("Bad input",
                 paste("First line of game file must contain a category name (no '|')."),
                 type="error")
      return("Bad Game Data")
    }
    if (!all(singleBar[2:6])) {
      shinyalert("Bad input",
                 paste("Lines 2 to 6 of game file must contain 'answer|question' pairs."),
                 type="error")
      return("Bad Game Data")
    }
    if (singleBar[7]) {
      shinyalert("Bad input",
                 paste("Line 7 of game file must contain a category name (no '|')."),
                 type="error")
      return("Bad Game Data")
    }
    
    # Use run length encoding to characterize pattern of categories and A|Q pairs
    catAQPair = rle(singleBar)
    temp = catAQPair$lengths[catAQPair$values==FALSE]
    if (length(temp) != 13) {
      shinyalert("Bad input",
                 paste("File must contain 6 Jeopardy Categories, 6 Double",
                       "Jeopardy Categories, and one Final Jeopary Category.",
                       "You have", length(temp), "categories."),
                 type="error")
      return("Bad Game Data")
    }
    if (any(temp != 1)) {
      index = which(temp != 1)[1]
      shinyalert("Bad input",
                 paste0("It appears that the '|' is missing in 'Answer|Question' for",
                       "category number ", index, "."),
                 type="error")
      return("Bad Game Data")
    }
    temp = catAQPair$lengths[catAQPair$values==TRUE]
    if (length(temp) != 13) {
      shinyalert("Bad input",
                 paste("File must contain 6 groups of 5 'Answer|Question' pairs",
                       "for Jeopardy, 6 groups of 5 'Answer|Question' pairs for",
                       "Double Jeopardy, and one Final Jeopary 'Answer|Question' pair",
                       "for a total of 61 pairs.  ",
                       "You have", sum(temp), "pairs in ", length(temp), "groups."),
                 type="error")
      return("Bad Game Data")
    }
    if (temp[13] != 1) {
      shinyalert("Bad input",
                 paste("There should be just one Final Jeopary 'Answer|Question' pair."),
                 type="error")
      return("Bad Game Data")
    }
    if (any(temp[1:12] != 5)) {
      index = which(temp != 5)[1]
      shinyalert("Bad input",
                 paste0("Category ", index, " has ", temp[index], " 'Answer|Question'",
                        "pairs."),
                 type="error")
      return("Bad Game Data")
    }
    shinyalert("Bad input", "Unhandled exception", type="error")
    return("Bad Game Data")
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
