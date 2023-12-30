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


# Define server logic for Jeopardy Game
function(input, output, session) {
  observe({cat("imageAnswer =", imageAnswer(), "\n")})
  #observe({cat("input$jbsA1() =", class(input$jbsA1), "\n")})

  # Reactive values to monitor game progress
  scores <- reactiveValues(P1=0, P2=0, P3=0)
  dollarAmount <- reactiveVal(0)
  stage <- reactiveVal("s")
  answersLeft <- reactiveVal(0)  # per board
  currentIncorrect <- reactiveVal(0)  # 0 to 3 incorrect answers given
  question <- reactiveVal("")
  inControl <- reactiveVal("P1")
  finalAnswerHidden <- reactiveVal(TRUE)
  onDailyDouble <- reactiveVal(FALSE)
  dailyDoubleAnswerHidden <- reactiveVal()
  ddAnswer <- reactiveVal()
  sjdd <- reactiveVal(sample(1:answersPerBoard, 1)) # Jeopardy daily double
  temp <- sample(1:answersPerBoard, 2)  # Double Jeopardy daily doubles
  djdd <- reactiveValues(dd1=temp[1], dd2=temp[2])
  imageAnswer <- reactiveVal(FALSE)
  image <- reactiveVal("")
  
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
  
  # Function to handle click on Jeopardy or Double Jeopardy board resulting in showing
  # the Answer on the "Answer" tab
  # Board must be "s" or "d"
  generateClickToAnswer <- function(position, board) {
    columnNum <- (position+4) %/% 5
    column <- LETTERS[columnNum]
    row <- ((position+4) %% 5) + 1
    id <- paste0("jb", board, column, row)
    observeEvent(input[[id]], {
      stage(board)   #  ?? needed
      dollarAmount(100*row*ifelse(board=="s", 1, 2))
      updateActionButton(inputId=id, label="")
      disable(id)
      answersLeft(isolate(answersLeft()) - 1)
      output$categoryReminder <- renderText(
        {paste0("$", dollarAmount(), ": ", 
                gameData()[[paste0(board, "jCategories")]][columnNum])})
      AQ <- paste0(board, "jAQ")
      answer <- gameData()[[AQ]][position, "Answer"]
      nca <- nchar(answer)
      # Check if answer is an image
      if (substring(answer, 1, 1) == "[" && substring(answer, nca) == "]") {
        imageAnswer(TRUE)
        image(substring(answer, 2, nca-1))
      }
      # Check if answer is a daily double
      if (stage() == "s") {
        foundOne <- position == isolate(sjdd())
      } else {
        foundOne <- position == isolate(djdd$dd1) || position == isolate(djdd$dd2)
      }
      onDailyDouble(foundOne)
      if (foundOne) {
        output$selectedAnswer <- renderUI({HTML("Daily Double")})
        ddAnswer(answer)
        dailyDoubleAnswerHidden(TRUE)
        updateActionButton(inputId="backToBoard", label="Bet Entered")
        for (player in c("P1", "P2", "P3")) {
          disable(paste0(player, "Correct"))
          disable(paste0(player, "Incorrect"))
          hide(paste0(player, "ddBet"))
        }
        ICB <- paste0(inControl(), "ddBet")
        updateTextInput(inputId=ICB, value="")
        show(ICB)
      } else {
        # if not a daily double
        if (imageAnswer()==FALSE) {
          output$selectedAnswer <- renderUI({HTML(gameData()[[AQ]][position, "Answer"])})
        } else {
          output$selectedImageAnswer <- renderImage(list(src="./images/RayLiotta.jpeg", height="225px"), 
                                                    deleteFile=FALSE)
            # renderImage(list(src=file.path("./images", image()), 
            #                                              contentType="jpeg", alt="No image"),
            #                                         deleteFile=FALSE)
        }
      }
      question(gameData()[[AQ]][position, "Question"])
      updateNavbarPage(session=session, "myNavbar", "Answer")
    })
  }

  # Setup Jeopardy action buttons to change tab and show Answer
  lapply(1:answersPerBoard, generateClickToAnswer, board="s")

  # Setup Double Jeopardy action buttons to change tab and show Answer
  lapply(1:answersPerBoard, generateClickToAnswer, board="d")
  
  # Show scores on Jeopardy board
  output$jbP1Score <- renderText({ 
    paste0(ifelse(inControl()=="P1", "*", ""), input$P1Name, ": $", scores$P1)
  })
  output$jbP2Score <- renderText({
    paste0(ifelse(inControl()=="P2", "*", ""), input$P2Name, ": $", scores$P2)
  })
  output$jbP3Score <- renderText({
    paste0(ifelse(inControl()=="P3", "*", ""), input$P3Name, ": $", scores$P3)
  })
  
  # Show scores on double Jeopardy board
  output$djbP1Score <- renderText({ 
    paste0(ifelse(inControl()=="P1", "*", ""), input$P1Name, ": $", scores$P1)
  })
  output$djbP2Score <- renderText({
    paste0(ifelse(inControl()=="P2", "*", ""), input$P2Name, ": $", scores$P2)
  })
  output$djbP3Score <- renderText({
    paste0(ifelse(inControl()=="P3", "*", ""), input$P3Name, ": $", scores$P3)
  })
  
  # Show scores on Answer tab
  output$answerP1Score <- renderText({
    paste0(ifelse(inControl()=="P1" && stage()!="f", "*", ""), input$P1Name, ": $", scores$P1)
  })
  output$answerP2Score <- renderText({
    paste0(ifelse(inControl()=="P2" && stage()!="f", "*", ""), input$P2Name, ": $", scores$P2)
  })
  output$answerP3Score <- renderText({
    paste0(ifelse(inControl()=="P3" && stage()!="f", "*", ""), input$P3Name, ": $", scores$P3)
  })
  
  # Start button
  observeEvent(input$start, {
    updateNavbarPage(session=session, "myNavbar", "Jeopardy")
  })

  
  #####################################################################
  ### Make "Start Game" button disabled until enough info is entered ##
  #####################################################################
  observe({
    if (! isTruthy(isolate(gameName)) || ! isTruthy(input$P1Name) ||
        ! isTruthy(input$P2Name) || ! isTruthy(input$P3Name)) {
      disable("start")
    } else {
      enable("start")
    }
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
  
  # Prepare for a new game
  resetAll <- function() {
    resetAllCorrectOrIncorrect()
    scores$P1 <- 0
    scores$P2 <- 0
    scores$P3 <- 0
    dollarAmount(0)
    stage("s")
    answersLeft(answersPerBoard)
    currentIncorrect(0)
    question("")
    inControl("P1")
    finalAnswerHidden(TRUE)
    hide("nextGame")
    show("backToBoard")
    for (player in c("P1", "P2", "P3")) {
      show(paste0(player, "Correct"))
      show(paste0(player, "Incorrect"))
      show(paste0(player, "fjBetPW"))
      show(paste0(player, "fjBet"))
    }
    updateTextInput(inputId="P1Name", value="")
    updateTextInput(inputId="P2Name", value="")
    updateTextInput(inputId="P3Name", value="")
    updateTextInput(inputId="P1fjBetPW", value="")
    updateTextInput(inputId="P2fjBetPW", value="")
    updateTextInput(inputId="P3fjBetPW", value="")
    for (i in 1:answersPerBoard) {
      bLet <- LETTERS[(i+4) %/% 5]
      bNum <- (i-1) %% 5 + 1
      bname <- paste0("jbs", bLet, bNum)
      updateActionButton(inputId=bname, label=paste0("$", 100*bNum))
      enable(bname)
      bname <- paste0("jbd", bLet, bNum)
      updateActionButton(inputId=bname, label=paste0("$", 200*bNum))
      enable(bname)
    }
    dailyDoubleAnswerHidden(TRUE)
    sjdd(sample(1:answersPerBoard, 1)) # Jeopardy daily double
    temp <- sample(1:answersPerBoard, 2)  # Double Jeopardy daily doubles
    djdd$dd1=temp[1]
    djdd$dd2=temp[2]
  }
  
  ### Handle "Return to Board"
  returnToBoard <- function() {
    onDailyDouble(FALSE)
    imageAnswer(FALSE)
    if (stage() != "f" && answersLeft() == 0) {
      nextStage()
    }
    page <- as.character(stageMatch[stage()]) # unnamed object required
    updateNavbarPage(session=session, "myNavbar", page)
  }

  # Go to next stage: called only when all questions on a board are completed
  nextStage <- function() {
    if (stage() == "s") {
      # Go from Jeopardy to Double Jeopardy
      answersLeft(answersPerBoard)
      stage("d")
    } else {
      # Go from Double Jeopardy to Final Jeopardy
      stage("f")
      finalAnswerHidden(TRUE) # Step one of Final Jeopardy
      answersLeft(answersPerBoard)
      currentIncorrect(0)
      hide("P1fjBet")
      hide("P1Correct")
      hide("P1Incorrect")
      hide("P2fjBet")
      hide("P2Correct")
      hide("P2Incorrect")
      hide("P3fjBet")
      hide("P3Correct")
      hide("P3Incorrect")
      for (player in c("P1", "P2", "P3")) {
        PB <- paste0(player, "fjBetPW")
        if (scores[[player]] > 0) {
          show(PB)
        } else {
          hide(PB)
        }
      }
      output$categoryReminder <- renderText(
        {paste0("Final Jeopardy: ", gameData()[["fjCategory"]])})
      output$selectedAnswer <- renderUI({HTML("")})
      question(gameData()[["fjAQ"]][1, "Question"])
      updateActionButton(inputId="backToBoard", label="Bets Entered")
      show("backToBoard")
    }
  }
  
  ### Handle "Next Game" button
  observeEvent(input$nextGame, {
    resetAll()
    updateNavbarPage(session=session, "myNavbar", "Intro")
  })
  
  ### Handle "backToBoard" actionButton() which shows:
  ### mostly: "Back to Board"
  ### for a daily double: "Bet Entered"
  ### for Final Jeopardy: "Bets Entered"
  observeEvent(input$backToBoard, {
    if (onDailyDouble()) {
      # handle "Bet Entered" for daily double
      output$selectedAnswer <- renderUI({HTML(ddAnswer())})
      who <- inControl()
      enable(paste0(who, "Correct"))
      enable(paste0(who, "Incorrect"))
      updateActionButton(inputId="backToBoard", label="Back to Board")
      hide("backToBoard")  # host must use Correct/Incorrect buttons to proceed
      dailyDoubleAnswerHidden(FALSE)
    } else if (stage()=="f" && finalAnswerHidden()) {
      # handle "Bets Entered" for Final Jeopardy
      # Switch from Final Jeopardy Step 1 to Step 2
      for (player in c("P1", "P2", "P3")) {
        if (scores[[player]] > 0) {
          show(paste0(player, "Correct"))
          show(paste0(player, "Incorrect"))
        } else {
          hide(paste0(player, "Correct"))
          hide(paste0(player, "Incorrect"))
        }
      }
      output$selectedAnswer <- renderUI({HTML(gameData()[["fjAQ"]][1, "Answer"])})
      finalAnswerHidden(FALSE)
      updateActionButton(inputId="backToBoard", label="Back to Board")
      hide("backToBoard")
      show("nextGame")
    } else {
      # handle ordinary "Back to Board"
      currentIncorrect(0)
      resetAllCorrectOrIncorrect()
      show("backToBoard")
      returnToBoard()
    }
  })  # end observeEvent() for "backToBoard"
  
  
  ### Code Correct and Incorrect buttons ###
  observeEvent(input$P1Correct, {
    if (stage() != "f") {
      # Jeopardy or Double Jeopardy
      show("backToBoard")
      if (onDailyDouble()) {
        dollars <- as.numeric(input$P1ddBet)
        if (is.na(dollars)) dollars <- 0
        upper <- max(scores$P1, 500 + 500*(stage()=="d"))
        if (dollars < 0 || dollars > upper)
          dollars <- min(upper, max(0, dollars))
      } else {
        dollars <- dollarAmount()
      }
      resetAllCorrectOrIncorrect()
      currentIncorrect(0)
      inControl("P1")
    } else {
      # Final Jeopardy
      hide("backToBoard")
      dollars <- as.numeric(input$P1fjBetPW)
      if (is.na(dollars)) dollars <- 0
      upper <- scores$P1
      if (dollars < 0 || dollars > upper)
        dollars <- min(upper, max(0, dollars))
      output$P1fjBet <- renderText(paste("     Bet: $:", dollars))
      show("P1fjBet")
      disable("P1Correct")
      disable("P1Incorrect")
    }
    scores$P1 <- as.numeric(scores$P1) + dollars
    returnToBoard()
  }, ignoreInit=TRUE)
  
  observeEvent(input$P2Correct, {
    if (stage() != "f") {
      # Jeopardy or Double Jeopardy
      show("backToBoard")
      if (onDailyDouble()) {
        dollars <- as.numeric(input$P2ddBet)
        if (is.na(dollars)) dollars <- 0
        upper <- max(scores$P2, 500 + 500*(stage()=="d"))
        if (dollars < 0 || dollars > upper)
          dollars <- min(upper, max(0, dollars))
      } else {
        dollars <- dollarAmount()
      }
      resetAllCorrectOrIncorrect()
      currentIncorrect(0)
      inControl("P2")
    } else {
      # Final Jeopardy
      hide("backToBoard")
      dollars <- as.numeric(input$P2fjBetPW)
      if (is.na(dollars)) dollars <- 0
      upper <- scores$P2
      if (dollars < 0 || dollars > scores$P2)
        dollars <- min(scores$P2, max(0, dollars))
      
      output$P2fjBet <- renderText(paste("     Bet: $:", dollars))
      show("P2fjBet")
      disable("P2Correct")
      disable("P2Incorrect")
    }
    scores$P2 <- as.numeric(scores$P2) + dollars
    returnToBoard()
  }, ignoreInit=TRUE)
  
  observeEvent(input$P3Correct, {
    if (stage() != "f") {
      # Jeopardy or Double Jeopardy
      show("backToBoard")
      if (onDailyDouble()) {
        dollars <- as.numeric(input$P3ddBet)
        if (is.na(dollars)) dollars <- 0
        upper <- max(scores$P3, 500 + 500*(stage()=="d"))
        if (dollars < 0 || dollars > upper)
          dollars <- min(upper, max(0, dollars))
      } else {
        dollars <- dollarAmount()
      }
      resetAllCorrectOrIncorrect()
      currentIncorrect(0)
      inControl("P3")
    } else {
      # Final Jeopardy
      hide("backToBoard")
      dollars <- as.numeric(input$P3fjBetPW)
      if (is.na(dollars)) dollars <- 0
      upper <- scores$P3
      if (dollars < 0 || dollars > upper)
        dollars <- min(upper, max(0, dollars))
      output$P3fjBet <- renderText(paste("     Bet: $:", dollars))
      show("P3fjBet")
      disable("P3Correct")
      disable("P3Incorrect")
    }
    scores$P3 <- as.numeric(scores$P3) + dollars
    returnToBoard()
  }, ignoreInit=TRUE)
  
  observeEvent(input$P1Incorrect, {
    if (stage() != "f") {
      # Jeopardy or Double Jeopardy
      show("backToBoard")
      if (onDailyDouble()) {
        dollars <- as.numeric(input$P1ddBet)
        if (is.na(dollars)) dollars <- 0
        upper <- max(scores$P1, 500 + 500*(stage()=="d"))
        if (dollars < 0 || dollars > upper)
          dollars <- min(upper, max(0, dollars))
      } else {
        dollars <- dollarAmount()
      }
    } else {
      # Final Jeopardy
      hide("backToBoard")
      dollars <- as.numeric(input$P1fjBetPW)
      if (is.na(dollars)) dollars <- 0
      upper <- scores$P1
      if (dollars < 0 || dollars > upper)
        dollars <- min(upper, max(0, dollars))
      output$P1fjBet <- renderText(paste("     Bet: $:", dollars))
      show("P1fjBet")
    }
    scores$P1 <- as.numeric(scores$P1) - dollars
    currentIncorrect(isolate(currentIncorrect()) + 1)
    if (currentIncorrect() == 3 || onDailyDouble()) {
      resetAllCorrectOrIncorrect()
      currentIncorrect(0)
      returnToBoard()
    } else {
      disable("P1Correct")
      disable("P1Incorrect")
    }
  }, ignoreInit=TRUE)
  
  observeEvent(input$P2Incorrect, {
    if (stage() != "f") {
      # Jeopardy or Double Jeopardy
      show("backToBoard")
      if (onDailyDouble()) {
        dollars <- as.numeric(input$P2ddBet)
        if (is.na(dollars)) dollars <- 0
        upper <- max(scores$P2, 500 + 500*(stage()=="d"))
        if (dollars < 0 || dollars > upper)
          dollars <- min(upper, max(0, dollars))
      } else {
        dollars <- dollarAmount()
      }
    } else {
      # Final Jeopardy
      hide("backToBoard")
      dollars <- as.numeric(input$P2fjBetPW)
      if (is.na(dollars)) dollars <- 0
      upper <- scores$P2
      if (dollars < 0 || dollars > upper)
        dollars <- min(upper, max(0, dollars))
      output$P2fjBet <- renderText(paste("     Bet: $:", dollars))
      show("P2fjBet")
    }
    scores$P2 <- as.numeric(scores$P2) - dollars
    currentIncorrect(isolate(currentIncorrect()) + 1)
    if (currentIncorrect() == 3 || onDailyDouble()) {
      resetAllCorrectOrIncorrect()
      currentIncorrect(0)
      returnToBoard()
    } else {
      disable("P2Correct")
      disable("P2Incorrect")
    }
  }, ignoreInit=TRUE)
  
  observeEvent(input$P3Incorrect, {
    if (stage() != "f") {
      # Jeopardy or Double Jeopardy
      show("backToBoard")
      if (onDailyDouble()) {
        dollars <- as.numeric(input$P3ddBet)
        if (is.na(dollars)) dollars <- 0
        upper <- max(scores$P3, 500 + 500*(stage()=="d"))
        if (dollars < 0 || dollars > upper)
          dollars <- min(upper, max(0, dollars))
      } else {
        dollars <- dollarAmount()
      }
    } else {
      # Final Jeopardy
      hide("backToBoard")
      dollars <- as.numeric(input$P3fjBetPW)
      if (is.na(dollars)) dollars <- 0
      upper <- scores$P3
      if (dollars < 0 || dollars > upper)
        dollars <- min(upper, max(0, dollars))
      output$P3fjBet <- renderText(paste("     Bet: $:", dollars))
      show("P3fjBet")
    }
    scores$P3 <- as.numeric(scores$P3) - dollars
    currentIncorrect(isolate(currentIncorrect()) + 1)
    if (currentIncorrect() == 3 || onDailyDouble()) {
      resetAllCorrectOrIncorrect()
      currentIncorrect(0)
      returnToBoard()
    } else {
      disable("P3Correct")
      disable("P3Incorrect")
    }
  }, ignoreInit=TRUE)

  
  observeEvent(input$showQuestion, {
    info(question())
  })
    
  # https://stackoverflow.com/questions/38895710/passing-reactive-values-to-conditionalpanel-condition
  output$finalStep1 <- reactive({
    stage()=="f" && finalAnswerHidden() == TRUE
  })
  output$finalStep2 <- reactive({
    stage()=="f" && finalAnswerHidden() == FALSE
  })
  output$onDD <- reactive({
    stage()!="f" && onDailyDouble() == TRUE
  })
  output$normal <- reactive({
    imageAnswer() == FALSE
  })
  output$isImage <- reactive({
    imageAnswer() == TRUE
  })
  outputOptions(output, "finalStep1", suspendWhenHidden = FALSE)
  outputOptions(output, "finalStep2", suspendWhenHidden = FALSE)
  outputOptions(output, "onDD", suspendWhenHidden = FALSE)
  outputOptions(output, "normal", suspendWhenHidden = FALSE)
  outputOptions(output, "isImage", suspendWhenHidden = FALSE)
  
  #hide("myNavbar")  # Works, but save for after all debugging
} # end server function
