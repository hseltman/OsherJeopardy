#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyFiles)
library(shinyalert)
library(shinyjs)
# library(leaflet)


# Define server logic required to draw a histogram
function(input, output, session) {
  #observe({cat("currentIncorect =", currentIncorrect(), "\n")})
  #observe({cat("input$jbsA1() =", class(input$jbsA1), "\n")})

  # Reactive values to monitor game progress
  scores <- reactiveValues(P1=0, P2=0, P3=0)
  dollarAmount <- reactiveVal(0)
  stage <- reactiveVal("s")
  answersLeft <- reactiveVal(0)  # per board
  currentIncorrect <- reactiveVal(0)  # 0 to 3 incorrect answers given
  
  # End the app
  observeEvent(input$quitApp, {stopApp()})
  
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
    
    # Guess that user totally forgot data format
    if (length(inData) < 12) {
      shinyalert("Bad input",
                 paste0("File must contain 12 sets of 6 lines each ",
                       "(where line 1 is a Category Name, and lines ",
                       "2 to 5 are 'answer", input$AQSeparator, "question' pairs), ",
                       "followed by a Final Jeopardy Category, and then a Final ",
                       "Jeopardy 'answer", input$AQSeparator, "question' pair."),
                 type="error")
      return(NULL)
    } 
    
    # Find which lines have a single bar (or 'AQSeparator')
    singleBar = grepl(paste0("^([^", input$AQSeparator, "]+?)[", input$AQSeparator,
                             "]([^", input$AQSeparator, "]+)$"), inData)

    # Handle correct pattern
    if (length(singleBar) == length(sbPattern) && all(singleBar==sbPattern)) {
      sjCatLoc = seq(1, by=6, length.out=6)
      sjCategories = inData[sjCatLoc]
      djCatLoc = seq(37, by=6, length.out=6)
      djCategories = inData[djCatLoc]
      fjCategory = inData[73]
      tc = textConnection(inData[1:36][-sjCatLoc])
      sjAQ = read.table(tc, sep=input$AQSeparator, quote="",
                        col.names=c("Answer", "Question"))
      close(tc)
      tc = textConnection(inData[37:72][-sjCatLoc]) # [sic]
      djAQ = read.table(tc, sep=input$AQSeparator, quote="",
                        col.names=c("Answer", "Question"))
      close(tc)
      temp = strsplit(inData[74], input$AQSeparator, fixed=TRUE)[[1]]
      fjAQ = data.frame(Answer=temp[1], Question=temp[2])
      answersLeft(answersPerBoard)
      return(list(sjCategories=sjCategories,
                  djCategories=djCategories,
                  fjCategory=fjCategory,
                  sjAQ=sjAQ,
                  djAQ=djAQ,
                  fjAQ=fjAQ))
    }
    
    # Handle first part of pattern not 'FTTTTTF'
    if (singleBar[1]) {
      shinyalert("Bad input",
                 paste0("First line of game file must contain a category name ",
                       "(no '", input$AQSeparator, "')."),
                 type="error")
      return(NULL)
    }
    if (!all(singleBar[2:6])) {
      shinyalert("Bad input",
                 paste0("Lines 2 to 6 of game file must contain 'answer",
                        input$AQSeparator, "question' pairs."),
                 type="error")
      return(NULL)
    }
    if (singleBar[7]) {
      shinyalert("Bad input",
                 paste0("Line 7 of game file must contain a category name (no '",
                        input$AQSeparator, "')."),
                 type="error")
      return(NULL)
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
      return(NULL)
    }
    if (any(temp != 1)) {
      index = which(temp != 1)[1]
      shinyalert("Bad input",
                 paste0("It appears that the '", input$AQSeparator,
                        "' is missing in 'Answer", input$AQSeparator, "Question' for",
                        " category number ", index, "."),
                 type="error")
      return(NULL)
    }
    temp = catAQPair$lengths[catAQPair$values==TRUE]
    if (length(temp) != 13) {
      shinyalert("Bad input",
                 paste0("File must contain 6 groups of 5 'Answer", input$AQSeparator,
                       "Question' pairs ",
                       "for Jeopardy, 6 groups of 5 'Answer", input$AQSeparator,
                       "Question' pairs for Double Jeopardy, and one Final ",
                       "Jeopary 'Answer", input$AQSeparator, "Question' pair ",
                       "for a total of 61 pairs.  ",
                       "You have", sum(temp), "pairs in ", length(temp), "groups."),
                 type="error")
      return(NULL)
    }
    if (temp[13] != 1) {
      shinyalert("Bad input",
                 paste0("There should be just one Final Jeopary 'Answer",
                 input$AQSeparator, "Question' pair."),
                 type="error")
      return(NULL)
    }
    if (any(temp[1:12] != 5)) {
      index = which(temp != 5)[1]
      shinyalert("Bad input",
                 paste0("Category ", index, " has ", temp[index], " 'Answer",
                 input$AQSeparator, "Question' pairs."),
                 type="error")
      return(NULL)
    }
    shinyalert("Bad input", "Unhandled exception", type="error")
    return(NULL)
  }) # end definition of gameData() reactive function
  
  
  # Add headers to Jeopardy board
  lapply(1:6, function(column) {
    outputId <- paste0("jbs", LETTERS[column])
    output[[outputId]] <- renderText(gameData()[["sjCategories"]][column])
  })  
  
  # Add headers to Double Jeopardy board
  lapply(1:6, function(column) {
    outputId <- paste0("jbd", LETTERS[column])
    output[[outputId]] <- renderText(gameData()[["djCategories"]][column])
  })  
  
  # Show game board file
  output$gameNameText <- renderText({
    tName <- gameName()
    tName <- ifelse(isTruthy(tName), basename(tName), "None")
    paste("Game board file:", tName)
  })

  output$categoryReminder <- renderText({"Nothing selected"})
  
  # Function to handle click on a board resulting in showing the Answer on the 
  # "Answer" tab
  # Board must be "s" or "d"
  generateClickToAnswer <- function(position, board) {
    columnNum <- (position+4) %/% 5
    column <- LETTERS[columnNum]
    row <- ((position+4) %% 5) + 1
    id <- paste0("jb", board, column, row)
    observeEvent(input[[id]], {
      stage(board)
      dollarAmount(100*row*ifelse(board=="s", 1, 2))
      updateActionButton(inputId=id, label="")
      disable(id)
      answersLeft(isolate(answersLeft()) - 1)
      output$categoryReminder <- renderText(
        {paste0("$", dollarAmount(), ": ", 
                gameData()[[paste0(board, "jCategories")]][columnNum])})
      output$selectedAnswer <- renderUI(
        {HTML(gameData()[[paste0(board, "jAQ")]][position, "Answer"])})
      updateNavbarPage(session=session, "myNavbar", "Answer")
    })
  }

  # Setup Jeopardy action buttons to change tab and show Answer
  lapply(1:answersPerBoard, generateClickToAnswer, board="s")

  # Setup Double Jeopardy action buttons to change tab and show Answer
  lapply(1:answersPerBoard, generateClickToAnswer, board="d")
  
  # Show scores on Jeopardy board
  output$jbP1Score <- renderText({ paste0(input$P1Name, ": $", scores$P1)})
  output$jbP2Score <- renderText({ paste0(input$P2Name, ": $", scores$P2)})
  output$jbP3Score <- renderText({ paste0(input$P3Name, ": $", scores$P3)})

  # Show scores on double Jeopardy board
  output$djbP1Score <- renderText({ paste0(input$P1Name, ": $", scores$P1)})
  output$djbP2Score <- renderText({ paste0(input$P2Name, ": $", scores$P2)})
  output$djbP3Score <- renderText({ paste0(input$P3Name, ": $", scores$P3)})
  
  # Show scores on Answer tab
  output$answerP1Score <- renderText({ paste0(input$P1Name, ": $", scores$P1)})
  output$answerP2Score <- renderText({ paste0(input$P2Name, ": $", scores$P2)})
  output$answerP3Score <- renderText({ paste0(input$P3Name, ": $", scores$P3)})

  # Start button
  observeEvent(input$start, {
    updateNavbarPage(session=session, "myNavbar", "Jeopardy")
  })

  
  ###########################
  ### Handle "Answer" tab ###
  ###########################
  resetAllCorrectOrIncorrect <- function() {
    enable("P1Correct")
    enable("P1Incorrect")
    enable("P2Correct")
    enable("P2Incorrect")
    enable("P3Correct")
    enable("P3Incorrect")
  }
  
  returnToBoard <- function() {
    if (answersLeft() == 0) {
      nextBoard()
    }
    page <- as.character(stageMatch[stage()]) # unnamed object required
    updateNavbarPage(session=session, "myNavbar", page)
  }
  
  nextBoard <- function() {
    if (stage() == "s") {
      answersLeft(answersPerBoard)
      stage("d")
    } else {
      stage("f")
      hide("backToBoard")
      if (scores$P1>0) {
        show("P1ddBet")
      }
      if (scores$P2>0) {
        show("P2ddBet")
      }
      if (scores$P3>0) {
        show("P3ddBet")
      }
      show("nextGame")
      output$categoryReminder <- renderText(
        {paste0("Final Jeopardy: ", 
                gameData()[["fjCategory"]])})
      output$selectedAnswer <- renderUI(
        {HTML(gameData()[["fjAQ"]][1, "Answer"])})
    }
  }
  
  observeEvent(input$P1Correct, {
    scores$P1 <- scores$P1 + dollarAmount()
    resetAllCorrectOrIncorrect()
    currentIncorrect(0)
    returnToBoard()
  }, ignoreInit=TRUE)
  observeEvent(input$P2Correct, {
    scores$P2 <- scores$P2 + dollarAmount()
    resetAllCorrectOrIncorrect()
    currentIncorrect(0)
    returnToBoard()
  }, ignoreInit=TRUE)
  observeEvent(input$P3Correct, {
    scores$P3 <- scores$P3 + dollarAmount()
    resetAllCorrectOrIncorrect()
    currentIncorrect(0)
    returnToBoard()
  }, ignoreInit=TRUE)
  
  observeEvent(input$P1Incorrect, {
    scores$P1 <- scores$P1 - dollarAmount()
    currentIncorrect(isolate(currentIncorrect()) + 1)
    if (currentIncorrect() == 3) {
      resetAllCorrectOrIncorrect()
      currentIncorrect(0)
      returnToBoard()
    } else {
      disable("P1Correct")
      disable("P1Incorrect")
    }
  }, ignoreInit=TRUE)
  observeEvent(input$P2Incorrect, {
    scores$P2 <- scores$P2 - dollarAmount()
    currentIncorrect(isolate(currentIncorrect()) + 1)
    if (currentIncorrect() == 3) {
      resetAllCorrectOrIncorrect()
      currentIncorrect(0)
      returnToBoard()
    } else {
      disable("P2Correct")
      disable("P2Incorrect")
    }
  }, ignoreInit=TRUE)
  observeEvent(input$P3Incorrect, {
    scores$P3 <- scores$P3 - dollarAmount()
    currentIncorrect(isolate(currentIncorrect()) + 1)
    if (currentIncorrect() == 3) {
      resetAllCorrectOrIncorrect()
      currentIncorrect(0)
      returnToBoard()
    } else {
      disable("P3Correct")
      disable("P3Incorrect")
    }
  }, ignoreInit=TRUE)

  observeEvent(input$backToBoard, {
    currentIncorrect(0)
    resetAllCorrectOrIncorrect()
    returnToBoard()
  })
    
} # end server function
