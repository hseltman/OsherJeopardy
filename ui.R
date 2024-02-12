#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyFiles)
library(shinyalert)
library(shinyjs)


# For JColList(), make one dollar amount box
makeJBox = function(n, prefix, multiple=1, width=NULL, height=height) {
  box(actionButton(paste0(prefix, n), paste0("$", n*200*multiple), 
                   style='width:110%; height:65px; font-size:150%'), 
      width=12, height=height)
}

# For makeJHColumn(), make the five dollar amounts
makeJColList = function(prefix, width=NULL, height=height, multiple=1) {
  JColumns = list()
  for (n in 1:5) {
    JColumns = c(JColumns, list(makeJBox(n, prefix, multiple=multiple,
                                         width=width, height=height)))
  }
  return(JColumns)
}

# Make one Jeopardy board column including category and five dollar amounts
makeJHColumn = function(prefix, width=2, height=75, multiple=1) {
  list(column(width=width, 
              div(style=paste("text-align:center;",
                  "border-style: solid;",
                  "margin: 2px -4px 8px -4px;",
                  #"width:100%;",
                  "white-space: no-wrap;",
                  "height: 200px;",
                  "text-align: justify;",
                  "overflow: auto;",
                  "font-size: 150%;",
                  ")"), # end style()
                  box(textOutput(prefix), height=100), width=2),
       makeJColList(prefix, width=width, height=height, 
                           multiple=multiple)))
}

###########################
## Create User Interface ##
###########################

ui = navbarPage("Jeopardy Game", id="myNavbar", theme = shinytheme("flatly"),
                #tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                #tags$style(type = "text/css", ".navbar{padding-left:2px;
                #           padding-right:2px ; margin-right:2px; margin-left:2px;}"),
                # Intro Panel  
                tabPanel("Intro", style=noGrayBandStyle,
                         useShinyjs(),  # Set up shinyjs
                         sidebarLayout(
                            sidebarPanel(width=0),
                            mainPanel(
                              column(width=12,
                                box(textOutput("gameNameText"), width=12, height=30),
                                box(p("Player names"), width=12, height=25),
                                box(textInput("P1Name", "1: "), width=4),
                                box(textInput("P2Name", "2: "), width=4),
                                box(textInput("P3Name", "3: "), width=4),
                                box(shinyFilesButton("inputFile", "Select a game",
                                                     "Please select a game file", 
                                                     multiple=FALSE),
                                    width=12, height=50),
                                box(actionButton("start", "Start Game")),
                                box(selectInput("AQSeparator",
                                                "Separator between Answer and Question",
                                                choices=c("|", "/", "\\", "&"),
                                                selected="|", selectize=TRUE),
                                    width=12),
                                box(actionButton("quitApp", "Quit Jeopardy"))
                              ), # end column()
                            width=12) # end mainPanel()
                          ) # end sidebarLayout()
                        ), # end tabPanel
                
                # Jeopardy Panel
                tabPanel("Jeopardy", style=noGrayBandStyle,
                         sidebarLayout(
                           sidebarPanel(width=0),
                           mainPanel(#style=frStyle,
                             fluidRow(#style=frStyle,
                               mapply(makeJHColumn, 
                                      prefix=paste0("jbs", LETTERS[1:6]),
                                      MoreArgs=list(multiple=1, width=2)),
                             width=12), # end fluidRow()
                             column(width=4, style=colStyle,
                                    box(style=scoreStyle, width="100%",
                                        textOutput("jbP1Score"))),
                             column(width=4, style=colStyle,
                                    box(style=scoreStyle, width="100%",
                                        textOutput("jbP2Score"))),
                             column(width=4, style=paste(colStyle, "margin: 2px 0px 8px 0px;"),
                                    box(style=scoreStyle, width="100%",
                                        textOutput("jbP3Score"))),
                             fluidRow(p("")), fluidRow(p("")), fluidRow(p("")),
                             fluidRow(p("")), fluidRow(p("")), fluidRow(p("")),
                             fluidRow(p("")), fluidRow(p("")), fluidRow(p("")),
                             column(width=4,
                                    box(actionButton("fixScoresFromJ", "Fix scores",
                                        style="color: #b8b8b8; background-color: #EFEFEF; border-color=#EFEFEF;"))
                             ),
                             width=12) # end mainPanel()
                         )),
                
                # Double Jeopardy Panel
                tabPanel("Double Jeopardy", style=noGrayBandStyle,
                  sidebarLayout(
                  sidebarPanel(width=0),
                  mainPanel(
                    fluidRow(
                      mapply(makeJHColumn, 
                             prefix=paste0("jbd", LETTERS[1:6]),
                             MoreArgs=list(multiple=2, width=2))
                    ), # end fluidRow() for category names and dollar values
                    column(width=4, style=colStyle,
                           box(style=scoreStyle, width="100%",
                               textOutput("djbP1Score"))),
                    column(width=4, style=colStyle,
                           box(style=scoreStyle, width="100%",
                               textOutput("djbP2Score"))),
                    column(width=4, style=paste(colStyle, "margin: 2px 0px 8px 0px;"),
                           box(style=scoreStyle, width="100%",
                               textOutput("djbP3Score"))),
                    fluidRow(p("")), fluidRow(p("")), fluidRow(p("")),
                    fluidRow(p("")), fluidRow(p("")), fluidRow(p("")),
                    fluidRow(p("")), fluidRow(p("")), fluidRow(p("")),
                    column(width=4,
                           box(actionButton("fixScoresFromDJ", "Fix scores",
                                            style="color: #b8b8b8; background-color: #EFEFEF; border-color=#EFEFEF;"))
                    ),
                    width=12) # end mainPanel()
                    )),  # end sidebarLayout() and tabPanel()
                
                # Answer Panel
                tabPanel("Answer", style=noGrayBandStyle,
                         sidebarLayout(
                           sidebarPanel(width=0),
                           mainPanel(
                             ## Category and answer ##
                             fluidRow(box(textOutput("categoryReminder"),
                                          style="font-size: 320%;",
                                          width=12, height=80),
                                      conditionalPanel(condition='output.showText',
                                        box(htmlOutput("selectedAnswer"),
                                          style="font-size: 380%;", width=12, height=425)
                                      ),
                                      conditionalPanel(condition='output.showImage',
                                        box(imageOutput("selectedImageAnswer"),
                                          style="text-align: center;", 
                                          width=12, height=400)
                                      ),
                             ),  # end fluidRow() for category and selected answer 
                             ## Scores ##
                             column(width=4,
                                    box(style=scoreStyle, width="100%",
                                        textOutput("answerP1Score"))),
                             column(width=4,
                                    box(style=scoreStyle, width="100%",
                                        textOutput("answerP2Score"))),
                             column(width=4,
                                    box(style=scoreStyle, width="100%",
                                        textOutput("answerP3Score"))),
                             ## Final jeopardy bets ##
                             conditionalPanel(condition='output.finalStep1',
                               column(width=4,
                                      box(passwordInput("P1fjBetPW", "Bet"),
                                          width="100%", height=120)),
                               column(width=4,
                                      box(passwordInput("P2fjBetPW", "Bet"),
                                          width="100%", height=120)),
                               column(width=4,
                                      box(passwordInput("P3fjBetPW", "Bet"),
                                          width="100%", height=120)),
                             ), # end conditionalPanel() for betting (see server.R)
                             conditionalPanel(condition='output.finalStep2',
                                column(width=4,
                                       box(style=betStyle, hidden(textOutput("P1fjBet")),
                                           width="100%", height=20)),
                                column(width=4,
                                       box(style=betStyle, hidden(textOutput("P2fjBet")),
                                           width="100%", height=120)),
                                column(width=4,
                                       box(style=betStyle, hidden(textOutput("P3fjBet")),
                                           width="100%", height=120)),
                             ), # end conditionalPanel() for betting (see server.R)
                             conditionalPanel(condition='output.onDD',
                                column(style=betStyle, width=4,
                                       box(textInput("P1ddBet", NULL),
                                           width="100%", height=70)),
                                column(style=betStyle, width=4,
                                       box(textInput("P2ddBet", NULL),
                                           width="100%", height=70)),
                                column(style=betStyle, width=4,
                                       box(textInput("P3ddBet", NULL),
                                           width="100%", height=70)),
                             ), # end conditionalPanel() for betting (see server.R)
                             ## Correct/Incorrect buttons ##
                             column(width=4,
                                    box(actionButton("P1Correct", "P1 correct"),
                                        width=12, height=50),
                                    box(actionButton("P1Incorrect", "P1 incorrect"),
                                        width=12, height=50)
                             ),
                             column(width=4,
                                    box(actionButton("P2Correct", "P2 correct"),
                                        width=12, height=50),
                                    box(actionButton("P2Incorrect", "P2 incorrect"),
                                        width=12, height=50)
                             ),
                             column(width=4,
                                    box(actionButton("P3Correct", "P3 correct"),
                                        width=12, height=50),
                                    box(actionButton("P3Incorrect", "P3 incorrect"),
                                        width=12, height=50),
                             ), # end third column for Correct/Incorrect action buttons
                             fluidRow(box(p(), height=20)),
                             column(width=4,
                                    box(actionButton("backToBoard", "Back to Board"),
                                        width=12, height=50)
                             ),
                             column(width=4,
                                    box(hidden(actionButton("nextGame", "Next game")),
                                        width=12, height=50)
                             ), # end third column for special action buttons 
                           width=12)  # end mainPanel()
                         )),  # end sidebarLayout() and tabPanel()
                # Fix Scores Panel
                tabPanel("FixScores", style=noGrayBandStyle,
                         sidebarLayout(
                           sidebarPanel(width=0),
                           mainPanel(
                             ## Scores ##
                             column(width=4,
                                    box(style=scoreStyle, width="100%",
                                        textOutput("fixP1Score"))),
                             column(width=4,
                                    box(style=scoreStyle, width="100%",
                                        textOutput("fixP2Score"))),
                             column(width=4,
                                    box(style=scoreStyle, width="100%",
                                        textOutput("fixP3Score"))),
                             column(style=betStyle, width=4,
                                    box(textInput("P1ScoreAdjust", NULL),
                                        width="100%", height=70)),
                             column(style=betStyle, width=4,
                                    box(textInput("P2ScoreAdjust", NULL),
                                        width="100%", height=70)),
                             column(style=betStyle, width=4,
                                    box(textInput("P3ScoreAdjust", NULL),
                                        width="100%", height=70)),
                             column(width=4,
                                    box(actionButton("fixScoresReturn", "Return"))),
                             width=12)  # end mainPanel()
                         ))  # end sidebarLayout() and tabPanel()
) # end NavbarPage



