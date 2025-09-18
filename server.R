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


# Define server logic for Jeopardy Game
function(input, output, session) {
  debugging <- FALSE
  if (debugging) {
    answersPerBoard <- 5
    observe({cat("imageAnswer() =", imageAnswer(), "\n")})
    observe({cat("bettingAnswer() =", bettingAnswer(), "\n")})
    observe({cat("stage() =", stage(), "\n")})
    observe({cat("onDD =", stage()!="f" && bettingAnswer() == TRUE, "\n")})
    observe({cat("subStage() =", subStage(), "\n")})
    observe({cat("showText = ", 
                 imageAnswer() == FALSE || (bettingAnswer() && subStage()=="A"), "\n")})
    observe({cat("showImage = ", 
                 imageAnswer() == TRUE && (bettingAnswer()==FALSE || subStage()=="B"), "\n")})
  } else {
    hide("myNavbar")  # Works, but save for after all debugging
  }
  
  # Reactive values to monitor game progress
  scores <- reactiveValues(P1=0, P2=0, P3=0)
  dollarAmount <- reactiveVal(0)
  stage <- reactiveVal("s")
  answersLeft <- reactiveVal(0)  # per board
  currentIncorrect <- reactiveVal(0)  # 0 to 3 incorrect answers given
  inControl <- reactiveVal("P1")
  bettingAnswer <- reactiveVal(FALSE)  # Daily double or Final Jeopardy
  answerText <- reactiveVal()
  sjdd <- reactiveVal(genDD(1, ifelse(debugging, answersPerBoard, NA)))
  temp <- genDD(2, ifelse(debugging, answersPerBoard, NA))
  djdd <- reactiveValues(dd1=temp[1], dd2=temp[2])
  imageAnswer <- reactiveVal(FALSE)
  audioAnswer <- reactiveVal(FALSE)
  videoAnswer <- reactiveVal(FALSE)
  imageFileName <- reactiveVal("")
  audioFileName <- reactiveVal("")
  videoFileName <- reactiveVal("")
  subStage <- reactiveVal("A") # Daily Double or Final Jeopardy before image is shown
  
  # End the app
  observeEvent(input$quitApp, {stopApp()})
  
  # File selection
  shinyFileChoose(input, "inputFile", roots = roots, session=session, filetype="txt")
  
  # This is the file name as shown on the Intro page 
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
                        col.names=c("Answer", "Question"), comment.char="")
      close(tc)
      tc = textConnection(inData[37:72][-sjCatLoc]) # [sic]
      djAQ = read.table(tc, sep=input$AQSeparator, quote="",
                        col.names=c("Answer", "Question"), comment.char="")
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
                       "Jeopardy Categories, and one Final Jeopardy Category.",
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
                       "Jeopardy 'Answer", input$AQSeparator, "Question' pair ",
                       "for a total of 61 pairs.  ",
                       "You have", sum(temp), "pairs in ", length(temp), "groups."),
                 type="error")
      return(NULL)
    }
    if (temp[13] != 1) {
      shinyalert("Bad input",
                 paste0("There should be just one Final Jeopardy 'Answer",
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

  # Function to parse an answer for audio-visuals
  parseInput <- function(txt) {
    rslt <- list(text="", imageFile=NULL, audioFile=NULL, videoFile=NULL)
    for (i in 1:2) {
      LB <- regexpr("\\[.+?[.].+?\\]", txt)
      if (LB == -1) break
      if (LB>1) {
        warning(txt, " is not in a form not handled by parseInput()")
        break
      }
      fLen <- attr(LB, "match.length")
      fName <- substring(txt, 2, fLen - 1)
      txt <- substring(txt, fLen+1)
      startExt <- regexpr("[^.]+$", fName)
      ext <- substring(fName, startExt)
      if (ext %in% imageExtensions) rslt$imageFile <- fName
      if (ext %in% audioExtensions) rslt$audioFile <- fName
      if (ext %in% videoExtensions) rslt$videoFile <- fName
    } # up to two multimedia files per answer (only supported combo is image+audio)
    rslt$text <- txt
    return(rslt)
  }
  
  
  # Function to handle click on Jeopardy or Double Jeopardy board resulting in showing
  # the Answer on the "Answer" tab
  # Board must be "s" or "d"
  generateClickToAnswer <- function(position, board) {
    columnNum <- (position+4) %/% 5
    column <- LETTERS[columnNum]
    row <- ((position+4) %% 5) + 1
    id <- paste0("jb", board, column, row)
    observeEvent(input[[id]], {
      if (stage()!=board) alert("stage()!=board")
      stage(board)   #  ?? needed
      dollarAmount(200*row*ifelse(board=="s", 1, 2))
      updateActionButton(inputId=id, label="")
      disable(id)
      answersLeft(isolate(answersLeft()) - 1)
      output$categoryReminder <- renderText(
        {paste0("$", dollarAmount(), ": ", 
                gameData()[[paste0(board, "jCategories")]][columnNum])})
      AQ <- paste0(board, "jAQ")
      answer <- gameData()[[AQ]][position, "Answer"]
      parsedAnswer <- parseInput(answer)
      answerText(parsedAnswer$text)
      output$selectedAnswer <- renderUI({HTML(answerText())}) # overridden if daily double
      # Check if answer is multimedia
      if (!is.null(parsedAnswer$imageFile)) {
        imageAnswer(TRUE)
        imageFileName(parsedAnswer$imageFile)
        output$selectedImageAnswer <- renderImage(list(src=paste0("./images/", imageFileName()),
                                                       height="375px"), 
                                                  deleteFile=FALSE)
        
      }
      if (!is.null(parsedAnswer$audioFile)) {
        audioAnswer(TRUE)
        audioFileName(parsedAnswer$audioFile)
        enable("replayAudio")
        show("replayAudio")
      } else {
        disable("replayAudio")
        hide("replayAudio")
      }
      if (!is.null(parsedAnswer$videoFile)) {
        videoAnswer(TRUE)
        videoFileName(parsedAnswer$videoFile)
      }
      # Check if answer is a daily double
      if (stage() == "s") {
        foundOne <- position == isolate(sjdd())
      } else {
        foundOne <- position == isolate(djdd$dd1) || position == isolate(djdd$dd2)
      }
      bettingAnswer(foundOne)
      if (foundOne) {
        # This is a Daily Double
        if (imageAnswer() || videoAnswer()) {
          output$selectedAnswer <- renderUI({HTML("Video Daily Double")})
        } else if (audioAnswer()) {
          output$selectedAnswer <- renderUI({HTML("Audio Daily Double")})
        } else {
          output$selectedAnswer <- renderUI({HTML("Daily Double")})
        }
        subStage("A")
        updateActionButton(inputId="backToBoard", label="Bet Entered")
        for (player in c("P1", "P2", "P3")) {
          disable(paste0(player, "Correct"))
          disable(paste0(player, "Incorrect"))
          hide(paste0(player, "ddBet"))
        }
        ICB <- paste0(inControl(), "ddBet")
        updateTextInput(inputId=ICB, value="")
        show(ICB)
      } else if (audioAnswer() == TRUE) {
        # Audio, but not a Daily Double
        insertUI(selector = "#backToBoard",
                 where = "afterEnd",
                 ui= tags$div(
                   id = "jAudioVideo",
                   tags$audio(src = audioFileName(), type = "audio/mp3", autoplay = NA,
                              controls = NA, style="display:none;")  
                 ) # end tags$div()
        )
      } else if (videoAnswer() == TRUE) {
        # Video, but not a Daily Double
        insertUI(selector = "#categoryReminder",
                 where = "afterEnd",
                 ui= tags$div(
                   id = "jAudioVideo",
                   tags$video(src = videoFileName(), type = "video/mov", autoplay = NA,
                              controls = NA) #, style="display:none;")  
                 ) # end tags$div()
        )
      }      
      updateNavbarPage(session=session, "myNavbar", "Answer")
    })
  }
  
  observeEvent(input$fixScoresFromJ, {
    updateNavbarPage(session=session, "myNavbar", "FixScores")
  })
  observeEvent(input$fixScoresFromDJ, {
    updateNavbarPage(session=session, "myNavbar", "FixScores")
  })
  
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
  })  # end generateClickAnswer()
  
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
  
  # Show scores on fixScores tab
  output$fixP1Score <- renderText({
    paste0(ifelse(inControl()=="P1" && stage()!="f", "*", ""), input$P1Name, ": $", scores$P1)
  })
  output$fixP2Score <- renderText({
    paste0(ifelse(inControl()=="P2" && stage()!="f", "*", ""), input$P2Name, ": $", scores$P2)
  })
  output$fixP3Score <- renderText({
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
    for (player in c("P1", "P2", "P3")) {
      enable(paste0(player, "Correct"))
      enable(paste0(player, "Incorrect"))
    }
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
    inControl("P1")
    hide("nextGame")
    show("backToBoard")
    for (player in c("P1", "P2", "P3")) {
      show(paste0(player, "Correct"))
      show(paste0(player, "Incorrect"))
      show(paste0(player, "fjBetPW"))
      show(paste0(player, "fjBet"))
      updateTextInput(inputId=paste0(player, "Name"), value="")
      updateTextInput(inputId=paste0(player, "fjBetPW"), value="")
    }
    for (i in 1:answersPerBoard) {
      bLet <- LETTERS[(i+4) %/% 5]
      bNum <- (i-1) %% 5 + 1
      bname <- paste0("jbs", bLet, bNum)
      updateActionButton(inputId=bname, label=paste0("$", 200*bNum))
      enable(bname)
      bname <- paste0("jbd", bLet, bNum)
      updateActionButton(inputId=bname, label=paste0("$", 400*bNum))
      enable(bname)
    }
    imageAnswer(FALSE)
    audioAnswer(FALSE)
    videoAnswer(FALSE)
    stage("s")
    subStage("A")
    sjdd(genDD(1, ifelse(debugging, answersPerBoard, NA))) # Jeopardy daily double
    temp <- genDD(2, ifelse(debugging, answersPerBoard, NA))
    djdd$dd1=temp[1]
    djdd$dd2=temp[2]
  }
  
  ### Handle "Return to Board"
  returnToBoard <- function() {
    if (stage() != "f") {
      bettingAnswer(FALSE)
      subStage("A")
      imageAnswer(FALSE)
      if (audioAnswer() || videoAnswer()) {
        removeUI(selector = "#jAudioVideo")
        audioAnswer(FALSE)
        videoAnswer(FALSE)
      }
      if (answersLeft() == 0) {
        nextStage()
      }
    }
    page <- as.character(stageMatch[stage()]) # unnamed object required
    updateNavbarPage(session=session, "myNavbar", page)
  }

  # Go to next stage: called only when all questions on a board are completed
  nextStage <- function() {
    if (stage() == "s") {
      ## Go from Jeopardy to Double Jeopardy ##
      answersLeft(answersPerBoard)
      stage("d")
      scores3 = c(scores$P1, scores$P2, scores$P3)
      minScore = min(scores3)
      lowest = which(scores3==minScore)
      if (length(lowest)>1) lowest <- sample(lowest, 1)
      inControl(paste0("P", lowest))
    } else {
      ## Go from Double Jeopardy to Final Jeopardy ##
      stage("f")
      subStage("A")
      bettingAnswer(TRUE)
      answersLeft(answersPerBoard)
      currentIncorrect(0)
      for (player in c("P1", "P2", "P3")) {
        hide(paste0(player, "fjBet"))
        hide(paste0(player, "Correct"))
        hide(paste0(player, "Incorrect"))
        PB <- paste0(player, "fjBetPW")
        if (scores[[player]] > 0) {
          show(PB)
        } else {
          hide(PB)
        }
      }
      output$categoryReminder <- renderText(
        {paste0("Final Jeopardy: ", gameData()[["fjCategory"]])})
      # Check if answer is an image
      answer <- gameData()[["fjAQ"]][1, "Answer"]
      parsedAnswer <- parseInput(answer)
      if (!is.null(parsedAnswer$imageFile)) {
        imageAnswer(TRUE)
        imageFileName(parsedAnswer$imageFile)
      }
      output$selectedAnswer <- renderUI({HTML("")})
      updateActionButton(inputId="backToBoard", label="Bets Entered")
      show("backToBoard")
    }
  }
  
  ### Handle "Next Game" button
  observeEvent(input$nextGame, {
    resetAll()
    removeUI(selector = "#fjAudio")
    updateNavbarPage(session=session, "myNavbar", "Intro")
  })
  
  ### Handle "backToBoard" actionButton() which shows:
  ### mostly: "Back to Board"
  ### for a daily double: "Bet Entered"
  ### for Final Jeopardy: "Bets Entered"
  observeEvent(input$backToBoard, {
    if (bettingAnswer() && stage()!="f") {
      # handle "Bet Entered" for Daily Double
      output$selectedAnswer <- renderUI({HTML(answerText())})
      who <- inControl()
      enable(paste0(who, "Correct"))
      enable(paste0(who, "Incorrect"))
      updateActionButton(inputId="backToBoard", label="Back to Board")
      hide("backToBoard")  # host must use Correct/Incorrect buttons to proceed
      subStage("B")
      if (audioAnswer()) {
        insertUI(selector = "#backToBoard",
                 where = "afterEnd",
                 ui= tags$div(
                   id = "jAudioVideo",
                   tags$audio(src = audioFileName(), type = "audio/mp3", autoplay = NA,
                              controls = NA, style="display:none;")  
                 ) # end tags$div()
        )
      } else if (videoAnswer()) {
        insertUI(selector = "#categoryReminder",
                 where = "afterEnd",
                 ui= tags$div(
                   id = "jAudioVideo",
                   tags$vidio(src = videoFileName(), type = "video/mov", autoplay = NA,
                              controls = NA, style="display:none;")  
                 ) # end tags$div()
        )
      }
    } else if (stage()=="f" && subStage()=="A") {
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
      if (imageAnswer()) {
        output$selectedImageAnswer <- renderImage(list(src=paste0("./images/", imageFileName()),
                                                       height="375px"), 
                                                  deleteFile=FALSE)
      } else {
        # Note: Final Jeopardy cannot be an audio or video answer
        output$selectedAnswer <- renderUI({HTML(gameData()[["fjAQ"]][1, "Answer"])})
      }
      subStage("B")
      updateActionButton(inputId="backToBoard", label="Back to Board")
      hide("backToBoard")
      show("nextGame")
      # Play the final Jeopardy theme song
      # https://stackoverflow.com/questions/71504072/how-to-conditionally-play-an-audio-clip-in-r-shiny
      insertUI(selector = "#backToBoard",
               where = "afterEnd",
               ui= tags$div(
                 id = "jAudioVideo",
                   tags$audio(src = themeSong, type = "audio/mp3", autoplay = NA,
                              controls = NA, style="display:none;")  
               ) # end tags$div()
      )
    } else {
      # handle ordinary "Back to Board"
      currentIncorrect(0)
      resetAllCorrectOrIncorrect()
      show("backToBoard")
      returnToBoard()
    }
  })  # end observeEvent() for "backToBoard"
  
  ############################################
  ### Handle Correct and Incorrect buttons ###
  ############################################
  observeEvent(input$P1Correct, {
    if (stage() != "f") {
      # Jeopardy or Double Jeopardy
      show("backToBoard")
      if (bettingAnswer()) {
        dollars <- as.numeric(input$P1ddBet)
        if (is.na(dollars)) dollars <- 0
        upper <- max(scores$P1, 1000 + 1000*(stage()=="d"))
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
      if (bettingAnswer()) {
        dollars <- as.numeric(input$P2ddBet)
        if (is.na(dollars)) dollars <- 0
        upper <- max(scores$P2, 1000 + 1000*(stage()=="d"))
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
      if (bettingAnswer()) {
        dollars <- as.numeric(input$P3ddBet)
        if (is.na(dollars)) dollars <- 0
        upper <- max(scores$P3, 1000 + 1000*(stage()=="d"))
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
      if (bettingAnswer()) {
        dollars <- as.numeric(input$P1ddBet)
        if (is.na(dollars)) dollars <- 0
        upper <- max(scores$P1, 1000 + 1000*(stage()=="d"))
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
    if (currentIncorrect() == 3 || bettingAnswer()) {
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
      if (bettingAnswer()) {
        dollars <- as.numeric(input$P2ddBet)
        if (is.na(dollars)) dollars <- 0
        upper <- max(scores$P2, 1000 + 1000*(stage()=="d"))
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
    if (currentIncorrect() == 3 || bettingAnswer()) {
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
      if (bettingAnswer()) {
        dollars <- as.numeric(input$P3ddBet)
        if (is.na(dollars)) dollars <- 0
        upper <- max(scores$P3, 1000 + 1000*(stage()=="d"))
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
    if (currentIncorrect() == 3 || bettingAnswer()) {
      resetAllCorrectOrIncorrect()
      currentIncorrect(0)
      returnToBoard()
    } else {
      disable("P3Correct")
      disable("P3Incorrect")
    }
  }, ignoreInit=TRUE)

  # Handle audio replay
  observeEvent(input$replayAudio, {
    insertUI(selector = "#backToBoard",
             where = "afterEnd",
             ui= tags$div(
               id = "jAudioVideo",
               tags$audio(src = audioFileName(), type = "audio/mp3", autoplay = NA,
                          controls = NA, style="display:none;")  
             ) # end tags$div()
    )
  })
  
  # Handle Fix Scores tab
  observeEvent(input$fixScoresReturn, {
    for (player in 1:3) {
      who <- paste0("P", player)
      what <- paste0(who, "ScoreAdjust")
      dollars <- as.numeric(input[[what]])
      if (is.na(dollars)) dollars <- 0
      scores[[who]] <- as.numeric(scores[[who]]) + dollars
      updateTextInput(inputId=what, value="")
    }
    if (stage()=="s") {
      updateNavbarPage(session=session, "myNavbar", "Jeopardy")
    } else {
      updateNavbarPage(session=session, "myNavbar", "Double Jeopardy")
    }
  })
      
  
  # https://stackoverflow.com/questions/38895710/passing-reactive-values-to-conditionalpanel-condition
  output$finalStep1 <- reactive({
    stage()=="f" && subStage()=="A"
  })
  output$finalStep2 <- reactive({
    stage()=="f" && subStage()=="B"
  })
  output$onDD <- reactive({
    stage()!="f" && bettingAnswer() == TRUE
  })
  output$showText <- reactive({
    (imageAnswer() == FALSE && videoAnswer() == FALSE) ||
      (bettingAnswer() && subStage()=="A")
  })
  output$showImage <- reactive({
    imageAnswer() == TRUE && (bettingAnswer()==FALSE || subStage()=="B")
  })
  outputOptions(output, "finalStep1", suspendWhenHidden = FALSE)
  outputOptions(output, "finalStep2", suspendWhenHidden = FALSE)
  outputOptions(output, "onDD", suspendWhenHidden = FALSE)
  outputOptions(output, "showText", suspendWhenHidden = FALSE)
  outputOptions(output, "showImage", suspendWhenHidden = FALSE)
  
} # end server function
